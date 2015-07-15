module Language.Prolog.HSwip.LowLevel where

import Data.Bits ((.|.))
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Language.Prolog.HSwip.Internals

type Term = CULong
type Atom = CULong
type Predicate = Ptr ()
type Module = Ptr ()
type Qid = CULong
type Fid = CULong

data TermType = TVariable
              | TAtom
              | TInteger
              | TFloat
              | TString
              | TTerm

-- PL_EXPORT(int)      PL_initialise(int argc, char **argv);
-- foreign import ccall "SWI-Prolog.h PL_initialise" c_PLInitialise :: CInt -> Ptr (Ptr CChar) -> IO CInt
plInitialise :: [String] -> IO Integer
plInitialise argv = do
    c_argvL <- mapM newCString argv
    c_argv <- mallocArray (length argv) :: IO (Ptr CString)
    pokeArray c_argv c_argvL
    res <- c_PLInitialise (fromInteger $ toInteger $ length argv) c_argv
    -- http://www.swi-prolog.org/pldoc/doc_for?object=c('PL_initialise')
    -- "Please note that the passed argument vector may be referred from
    -- Prolog at any time and should therefore be valid as long as the
    -- Prolog engine is used."
    -- free c_argv
    -- mapM_ free c_argvL
    return $ toInteger res

-- PL_EXPORT(void)     PL_halt(int status) NORETURN;
-- foreign import ccall "SWI-Prolog.h PL_halt" c_PLHalt :: CInt -> IO ()
plHalt :: Integer -> IO ()
plHalt = c_PLHalt . fromInteger

-- PL_EXPORT(fid_t)    PL_open_foreign_frame(void);
-- foreign import ccall "SWI-Prolog.h PL_open_foreign_frame" c_PL_open_foreign_frame :: IO CULong
plOpenForeignFrame :: IO Fid
plOpenForeignFrame = c_PL_open_foreign_frame

-- PL_EXPORT(void)     PL_close_foreign_frame(fid_t cid);
-- foreign import ccall "SWI-Prolog.h PL_close_foreign_frame" c_PL_close_foreign_frame :: CULong -> IO ()
plCloseForeignFrame :: Fid -> IO ()
plCloseForeignFrame = c_PL_close_foreign_frame

-- PL_EXPORT(predicate_t)  PL_predicate(const char *name, int arity,
--                      const char* module);
-- foreign import ccall "SWI-Prolog.h PL_predicate" c_PL_predicate :: Ptr CChar -> CInt -> Ptr CChar -> IO (Ptr ())
plPredicate :: String -> Integer -> IO Predicate
plPredicate name_ arity_ = do
    namePtr_ <- newCString name_
    pred_ <- c_PL_predicate namePtr_ (fromInteger arity_) nullPtr
    free namePtr_
    return pred_

-- PL_EXPORT(qid_t)    PL_open_query(module_t m, int flags,
--                       predicate_t pred, term_t t0);
-- foreign import ccall "SWI-Prolog.h PL_open_query" c_PL_open_query :: Ptr () -> CInt -> Ptr () -> CULong -> IO CULong
plOpenQuery :: Predicate -> Term -> IO Qid
plOpenQuery pred_ args_ = do
    -- PL_open_query(NULL, PL_Q_NORMAL, pred, args);
    -- #define PL_Q_NORMAL 0x02
    -- #defined PL_Q_NODEBUG 0x04      /* use this one */
    -- #define PL_Q_CATCH_EXCEPTION 0x08
    c_PL_open_query nullPtr 0x02 pred_ args_

-- PL_EXPORT(int)      PL_next_solution(qid_t qid);
-- foreign import ccall "SWI-Prolog.h PL_next_solution" c_PL_next_solution :: CULong -> IO CInt
plNextSolution :: Qid -> IO Bool
plNextSolution qid_ = do
    res <- c_PL_next_solution qid_
    if res == 0 then return False
                else return True

-- PL_EXPORT(void)     PL_close_query(qid_t qid);
-- foreign import ccall "SWI-Prolog.h PL_close_query" c_PL_close_query :: CULong -> IO ()
plCloseQuery :: Qid -> IO ()
plCloseQuery = c_PL_close_query

-- PL_EXPORT(term_t)   PL_exception(qid_t qid);
-- foreign import ccall "SWI-Prolog.h PL_exception" c_PL_exception :: CULong -> IO CULong
plException :: Qid -> IO Term
plException = c_PL_exception

-- foreign import ccall "SWI-Prolog.h PL_call" c_PL_call :: CULong -> Ptr () -> IO CInt
plCall :: Term -> IO ()
plCall term_ = do
    _ <- c_PL_call term_ nullPtr
    return ()

-- PL_EXPORT(term_t)   PL_new_term_refs(int n);
-- foreign import ccall "SWI-Prolog.h PL_new_term_refs" c_PL_new_term_refs :: CInt -> IO CULong
plNewTermRefs :: Integer -> IO Term
plNewTermRefs = c_PL_new_term_refs . fromInteger

-- PL_EXPORT(term_t)   PL_new_term_ref(void);
-- foreign import ccall "SWI-Prolog.h PL_new_term_ref" c_PL_new_term_ref :: IO CULong
plNewTermRef :: IO Term
plNewTermRef = c_PL_new_term_ref

-- foreign import ccall "SWI-Prolog.h PL_copy_term_ref" c_PL_copy_term_ref :: CULong -> IO CULong
plCopyTermRef :: Term -> IO Term
plCopyTermRef = c_PL_copy_term_ref

-- foreign import ccall "SWI-Prolog.h PL_reset_term_refs" c_PL_reset_term_refs :: CULong -> IO ()
plResetTermRefs :: Term -> IO ()
plResetTermRefs = c_PL_reset_term_refs

-- PL_EXPORT(const char *) PL_atom_chars(atom_t a);
-- foreign import ccall "SWI-Prolog.h PL_atom_chars" c_PL_atom_chars :: CULong -> IO (Ptr CChar)
plAtomChars :: Atom -> IO String
plAtomChars atom_ = do
    retcstr_ <- c_PL_atom_chars atom_
    retStr <- peekCString retcstr_
    -- free retcstr_   this free is unacceptable
    return retStr

-- PL_EXPORT(int)      PL_get_string(term_t t, char **s, size_t *len);
-- foreign import ccall "SWI-Prolog.h PL_get_string" c_PL_get_string :: CULong -> Ptr (Ptr CChar) -> Ptr CInt -> IO CInt
plGetString :: Term -> IO String
plGetString term_ = do
    resStr <- malloc :: IO (Ptr CString)
    _ <- c_PL_get_string term_ resStr nullPtr
    rescstring <- peek resStr
    res <- peekCString rescstring
    free resStr
    return res

-- PL_EXPORT(int)      PL_get_chars(term_t t, char **s, unsigned int flags);
-- foreign import ccall "SWI-Prolog.h PL_get_chars" c_PL_get_chars :: CULong -> Ptr (Ptr CChar) -> CUInt -> IO CInt
plGetChars :: Term -> IO String
plGetChars term_ = do
    resStr <- malloc :: IO (Ptr CString)
    -- from example : PL_get_chars(t,&s, CVT_ALL)
    -- CVT_ALL = CVT_INTEGER | CVT_FLOAT | CVT_ATOM | CVT_STRING | CVT_LIST
    --           0x1 | 0x2 | 0x4 | 0x8 | 0x10
    -- BUF_ALLOC = 0x0200
    _ <- c_PL_get_chars term_ resStr $ 0x1 .|. 0x2 .|. 0x4 .|. 0x8 .|. 0x10 .|. 0x0200
    rescstring <- peek resStr
    res <- peekCString rescstring
    free rescstring
    free resStr
    return res

-- PL_EXPORT(int)      PL_get_arg(int index, term_t t, term_t a);
-- foreign import ccall "SWI-Prolog.h PL_get_arg" c_PL_get_arg :: CInt -> CULong -> CULong -> IO CInt
plGetArg :: Integer -> Term -> Term -> IO ()
plGetArg index_ term1_ term2_ = do
    _ <- c_PL_get_arg (fromInteger index_) term1_ term2_
    return ()

-- foreign import ccall "SWI-Prolog.h PL_get_list" c_PL_get_list :: CULong -> CULong -> CULong -> IO CInt
plGetList :: Term -> Term -> Term -> IO Integer
plGetList list_ head_ tail_ = c_PL_get_list list_ head_ tail_ >>= return . toInteger

-- PL_EXPORT(int)      PL_get_name_arity(term_t t, atom_t *name, int *arity);
-- foreign import ccall "SWI-Prolog.h PL_get_name_arity" c_PL_get_name_arity :: CULong -> Ptr CULong -> Ptr CInt -> IO Int
plGetNameArity :: Term -> IO (String, Integer)
plGetNameArity term_ = do
    atomPtr_ <- malloc
    arityPtr_ <- malloc
    _ <- c_PL_get_name_arity term_ atomPtr_ arityPtr_
    atom_ <- peek atomPtr_
    arity_ <- peek arityPtr_
    free atomPtr_
    free arityPtr_
    atomName_ <- plAtomChars atom_
    return (atomName_, toInteger arity_)

-- PL_EXPORT(int)      PL_term_type(term_t t);
-- foreign import ccall "SWI-Prolog.h PL_term_type" c_PL_term_type :: CULong -> IO CInt
plTermType :: Term -> IO TermType
plTermType term_ = do
    retc <- c_PL_term_type term_
    return $ case retc of
                1 -> TVariable
                2 -> TAtom
                3 -> TInteger
                4 -> TFloat
                5 -> TString
                6 -> TTerm
                _ -> undefined

-- PL_EXPORT(void)     PL_put_variable(term_t t);
-- foreign import ccall "SWI-Prolog.h PL_put_variable" c_PL_put_variable :: CULong -> IO ()
plPutVariable :: Term -> IO ()
plPutVariable = c_PL_put_variable

-- PL_EXPORT(void)     PL_put_atom_chars(term_t t, const char *chars);
-- foreign import ccall "SWI-Prolog.h PL_put_atom_chars" c_PL_put_atom_chars :: CULong -> CString -> IO ()
plPutAtomChars :: Term -> String -> IO ()
plPutAtomChars term_ str_ = do
    cstr_ <- newCString str_
    c_PL_put_atom_chars term_ cstr_
    free cstr_

-- foreign import ccall "SWI-Prolog.h PL_put_list_chars" c_PL_put_list_chars:: CULong -> CString -> IO ()
plPutListChars :: Term -> String -> IO ()
plPutListChars term_ str_ = do
    cstr_ <- newCString str_
    c_PL_put_list_chars term_ cstr_
    free cstr_

-- PL_EXPORT(void)     PL_put_term(term_t t1, term_t t2);
-- foreign import ccall "SWI-Prolog.h PL_put_term" c_PL_put_term :: CULong -> CULong -> IO ()
plPutTerm :: Term -> Term -> IO ()
plPutTerm = c_PL_put_term

-- foreign import ccall "SWI-Prolog.h PL_chars_to_term" c_PL_chars_to_term :: CString -> CULong -> IO CInt
plCharsToTerm :: String -> Term -> IO ()
plCharsToTerm str_ term_ = do
    cstr_ <- newCString str_
    _ <- c_PL_chars_to_term cstr_ term_
    free cstr_
