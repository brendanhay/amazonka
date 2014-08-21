-- Module      : Network.AWS.Conduit.Internal
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Conduit.Internal where

import           Control.Monad
import           Control.Monad.Trans (lift)
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import           Data.Conduit
import           Data.IORef

sourcePopper :: Source IO ByteString
               -> ((IO ByteString -> IO ()) -> IO ())
sourcePopper src f = do
    (r, ()) <- src $$+ return ()
    ref     <- newIORef r
    f (go ref)
  where
    go ref = do
        x      <- readIORef ref
        (y, m) <- x $$++ await
        atomicWriteIORef ref y
        case m of
            Nothing -> return BS.empty
            Just bs | BS.null bs -> go ref
                    | otherwise  -> return bs

bodyReaderSource :: Monad m => m ByteString -> ResumableSource m ByteString
bodyReaderSource f = newResumableSource loop
  where
    loop = do
        bs <- lift f
        unless (BS.null bs) $ do
            yield bs
            loop
