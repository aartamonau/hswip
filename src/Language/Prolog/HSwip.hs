-- If you get an error /[FATAL ERROR: Could not find system resources]/
-- check out the manual <http://www.swi-prolog.org/FAQ/FindResources.html>
--
-- @
--  main = do
--      prologInit [\"-q\",\"-nosignals\"]
--      prologCall \"asserta(a(b,c)).\"
--      prologCall \"asserta(parent(pam, bob)).\"
--      prologCall \"asserta(parent(tom, bob)).\"
--      prologCall \"asserta(parent(tom, liz)).\"
--      prologCall \"asserta(parent(bob, ann)).\"
--      prologCall \"asserta(parent(bob, pat)).\"
--      prologCall \"asserta(parent(pat, jim)).\"
--      res <- prologCall \"parent(X,Y).\"
--      print res
--
--  Output:
--  [[(\"X\",\"pat\"),(\"Y\",\"jim\")],[(\"X\",\"bob\"),(\"Y\",\"pat\")],[(\"X\",\"bob\"),(\"Y\",\"ann\")],[(\"X\",\"tom\"),(\"Y\",\"liz\")],[(\"X\",\"tom\"),(\"Y\",\"bob\")],[(\"X\",\"pam\"),(\"Y\",\"bob\")]]
-- @
module Language.Prolog.HSwip ( module Language.Prolog.HSwip
                             , module Language.Prolog.HSwip.LowLevel) where

import Control.Monad
import Control.Monad.RWS
import Control.Monad.Writer
import Language.Prolog.HSwip.LowLevel
import System.Environment


-- | Prolog interpreter initialisation
prologInit :: [String] -> IO ()
prologInit args_ = do
    progName_ <- getProgName
    _ <- plInitialise $ progName_:args_
    fid_ <- plOpenForeignFrame
    swiplLoad <- plNewTermRef
    plCharsToTerm "asserta(hrun(GoalString,BindingList) :- \
                  \    (atom_chars(A,GoalString), \
                  \    atom_to_term(A,Goal,BindingList), \
                  \    call(Goal)))." swiplLoad
    plCall swiplLoad
    plCloseForeignFrame fid_

-- | The main prolog function
prologCall :: String                    -- ^ query string
           -> IO [[(String, String)]]   -- ^ query result
prologCall query_ = do
    fid_ <- plOpenForeignFrame

    swiplArgs <- plNewTermRefs 2
    let swiplGoalCharList = swiplArgs
    let swiplBindingList = swiplArgs + 1
    plPutListChars swiplGoalCharList query_

    swiplPredicate <- plPredicate "hrun" 2

    qid_ <- plOpenQuery swiplPredicate swiplArgs

    (_, _, res) <- runRWST getSolutions (qid_, swiplBindingList) ()

    plCloseQuery qid_
    plCloseForeignFrame fid_

    return res

    where
        getSolutions :: RWST (Qid, Term) [[(String, String)]] () IO ()
        getSolutions = do
            (qid_, term_) <- ask
            b_ <- liftIO $ plNextSolution qid_
            when b_ $ do
                (_, res) <- liftIO $ runWriterT (getSolution term_)
                tell [res]
                getSolutions

        getSolution :: Term -> WriterT [(String, String)] IO ()
        getSolution term_ = do
            head_ <- liftIO plNewTermRef
            t_ <- liftIO $ plCopyTermRef term_
            nextVariable head_ t_

        nextVariable :: Term -> Term -> WriterT [(String, String)] IO ()
        nextVariable head_ term_ = do
            asdf <- liftIO $ plGetList term_ head_ term_
            when (asdf /= 0) $ do
                res <- liftIO $ processResponse head_
                tell [res]
                nextVariable head_ term_

        -- takes =(X,a)
        processResponse :: Term -> IO (String, String)
        processResponse term_ = do
            termType_ <- plTermType term_
            case termType_ of
                TTerm -> do
                    a_ <- plNewTermRef
                    b_ <- plNewTermRef
                    (_, _) <- plGetNameArity term_
                    plGetArg 1 term_ a_
                    res1 <- plGetChars a_
                    plGetArg 2 term_ b_
                    res2 <- plGetChars b_
                    return (res1, res2)
                _ -> undefined

-- | Auxiliary function from swi-prolog manual
prologDisplayTerm :: Term -> IO ()
prologDisplayTerm t_ = do
    termType_ <- plTermType t_
    case termType_ of
        TVariable -> display1 t_
        TAtom -> display1 t_
        TInteger -> display1 t_
        TFloat -> display1 t_
        TString -> do
            s <- plGetString t_
            putStr $ "\"" ++ s ++ "\""
        TTerm -> do
            a_ <- plNewTermRef
            (name_, arity_) <- plGetNameArity t_
            putStr $ name_ ++ "("
            forM_ [1 .. arity_] $ \ n -> do
                plGetArg n t_ a_
                when (n > 1) $ putStr ", "
                prologDisplayTerm a_
            putStr ")"
    where
        display1 :: Term -> IO ()
        display1 term_ = plGetChars term_ >>= putStr
