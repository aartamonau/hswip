{-# LANGUAGE ForeignFunctionInterface #-}
module Language.Prolog.HSwip.Internals where

import Foreign.C
import Foreign.Ptr

foreign import ccall "SWI-Prolog.h PL_initialise" c_PLInitialise :: CInt -> Ptr (Ptr CChar) -> IO CInt
foreign import ccall "SWI-Prolog.h PL_halt" c_PLHalt :: CInt -> IO ()
foreign import ccall "SWI-Prolog.h PL_open_foreign_frame" c_PL_open_foreign_frame :: IO CULong
foreign import ccall "SWI-Prolog.h PL_close_foreign_frame" c_PL_close_foreign_frame :: CULong -> IO ()
foreign import ccall "SWI-Prolog.h PL_predicate" c_PL_predicate :: Ptr CChar -> CInt -> Ptr CChar -> IO (Ptr ())
foreign import ccall "SWI-Prolog.h PL_open_query" c_PL_open_query :: Ptr () -> CInt -> Ptr () -> CULong -> IO CULong
foreign import ccall "SWI-Prolog.h PL_next_solution" c_PL_next_solution :: CULong -> IO CInt
foreign import ccall "SWI-Prolog.h PL_close_query" c_PL_close_query :: CULong -> IO ()
foreign import ccall "SWI-Prolog.h PL_call" c_PL_call :: CULong -> Ptr () -> IO CInt
foreign import ccall "SWI-Prolog.h PL_exception" c_PL_exception :: CULong -> IO CULong
foreign import ccall "SWI-Prolog.h PL_new_term_refs" c_PL_new_term_refs :: CInt -> IO CULong
foreign import ccall "SWI-Prolog.h PL_new_term_ref" c_PL_new_term_ref :: IO CULong
foreign import ccall "SWI-Prolog.h PL_copy_term_ref" c_PL_copy_term_ref :: CULong -> IO CULong
foreign import ccall "SWI-Prolog.h PL_reset_term_refs" c_PL_reset_term_refs :: CULong -> IO ()
foreign import ccall "SWI-Prolog.h PL_atom_chars" c_PL_atom_chars :: CULong -> IO (Ptr CChar)
foreign import ccall "SWI-Prolog.h PL_get_string" c_PL_get_string :: CULong -> Ptr (Ptr CChar) -> Ptr CInt -> IO CInt
foreign import ccall "SWI-Prolog.h PL_get_chars" c_PL_get_chars :: CULong -> Ptr (Ptr CChar) -> CUInt -> IO CInt
foreign import ccall "SWI-Prolog.h PL_get_arg" c_PL_get_arg :: CInt -> CULong -> CULong -> IO CInt
foreign import ccall "SWI-Prolog.h PL_get_list" c_PL_get_list :: CULong -> CULong -> CULong -> IO CInt
foreign import ccall "SWI-Prolog.h PL_get_name_arity" c_PL_get_name_arity :: CULong -> Ptr CULong -> Ptr CInt -> IO Int
foreign import ccall "SWI-Prolog.h PL_term_type" c_PL_term_type :: CULong -> IO CInt
foreign import ccall "SWI-Prolog.h PL_put_variable" c_PL_put_variable :: CULong -> IO ()
foreign import ccall "SWI-Prolog.h PL_put_atom_chars" c_PL_put_atom_chars :: CULong -> CString -> IO ()
foreign import ccall "SWI-Prolog.h PL_put_list_chars" c_PL_put_list_chars :: CULong -> CString -> IO ()
foreign import ccall "SWI-Prolog.h PL_put_term" c_PL_put_term :: CULong -> CULong -> IO ()
foreign import ccall "SWI-Prolog.h PL_chars_to_term" c_PL_chars_to_term :: CString -> CULong -> IO CInt
