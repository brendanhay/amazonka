{-# LANGUAGE FlexibleContexts #-}

-- Module      : Network.AWS.Conduit
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Conduit
    (
    -- * Environment
      Env (..)
    -- ** Lenses
    , envAuth
    , envRegion
    , envManager
    , envLogging

    -- ** Creating the environment
    , newEnv

    -- * Synchronous requests
    -- ** Strict
    , send
    -- ** Streaming
    , stream
    -- ** Pagination
    , paginate

    -- * Request Bodies
    , sourceBody
    , sourceHandle
    , sourceFile
    , sourceFileIO

    -- * Signing URLs
    , presign
    ) where

import           Control.Applicative
import           Control.Monad.Morph
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Crypto.Hash
import qualified Crypto.Hash.Conduit          as Conduit
import           Data.ByteString              (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit
import           Data.Either
import           Data.Int
import qualified Network.AWS                  as AWS
import           Network.AWS                  hiding (paginate)
import           Network.AWS.Conduit.Internal
import           Network.AWS.Data
import           Network.AWS.Types
import           System.IO

stream :: (MonadBaseControl IO m, AWSRequest a)
       => Env
       -> a
       -> m (Either (Er (Sv a)) (Rs a, ResumableSource m ByteString))
stream e rq = with e rq (\rs f -> return (rs , bodyReaderSource f))

paginate :: (MonadBaseControl IO m, AWSPager a)
         => Env
         -> a
         -> ResumableSource m (Either (Er (Sv a)) (Rs a))
paginate e = newResumableSource . go
  where
    go x = do
        (rs, y) <- lift (AWS.paginate e x)
        yield rs
        if isLeft rs
            then return ()
            else maybe (return ()) go y

sourceBody :: Digest SHA256 -> Int64 -> Source IO ByteString -> RqBody
sourceBody h n = RqBody h . RequestBodyStream n . sourcePopper

sourceHandle :: Digest SHA256 -> Int64 -> Handle -> RqBody
sourceHandle h n = sourceBody h n . Conduit.sourceHandle

sourceFile :: Digest SHA256 -> Int64 -> FilePath -> RqBody
sourceFile h n = sourceBody h n . hoist runResourceT . Conduit.sourceFile

sourceFileIO :: FilePath -> IO RqBody
sourceFileIO f = sourceFile
    <$> runResourceT (Conduit.sourceFile f $$ Conduit.sinkHash)
    <*> (fromIntegral <$> withBinaryFile f ReadMode hFileSize)
    <*> pure f
