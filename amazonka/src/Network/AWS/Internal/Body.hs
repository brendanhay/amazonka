{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Network.AWS.Internal.Body
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Internal.Body
    (
    -- * Streaming
    -- ** Requests
      ToBody (..)
    , RqBody
    , sourceBody
    , sourceHandle
    , sourceFile
    , sourceFileIO
    -- ** Responses
    , RsBody
    , _RsBody
    , sinkBody
    ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           "cryptohash" Crypto.Hash
import qualified Crypto.Hash.Conduit          as Conduit
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit
import           Data.Int
import           Network.AWS.Prelude
import           Network.HTTP.Conduit
import           System.IO

-- | Construct a 'RqBody' from a source, manually specifying the
-- SHA256 hash and file size.
sourceBody :: Digest SHA256
           -> Int64
           -> Source (ResourceT IO) ByteString
           -> RqBody
sourceBody h n = RqBody h . requestBodySource n

-- | Construct a 'RqBody' from a 'Handle', manually specifying the
-- SHA256 hash and file size.
sourceHandle :: Digest SHA256 -> Int64 -> Handle -> RqBody
sourceHandle h n = sourceBody h n . Conduit.sourceHandle

-- | Construct a 'RqBody' from a 'FilePath', manually specifying the
-- SHA256 hash and file size.
sourceFile :: Digest SHA256 -> Int64 -> FilePath -> RqBody
sourceFile h n = sourceBody h n . Conduit.sourceFile

FIXME: use http-client's streamFile

-- | Construct a 'RqBody' from a 'FilePath', calculating the SHA256 hash
-- and file size.
--
-- /Note:/ While this function will perform in constant space, it will enumerate the
-- entirety of the file contents _twice_. Firstly to calculate the SHA256 and
-- lastly to stream the contents to the socket during sending.
sourceFileIO :: MonadIO m => FilePath -> m RqBody
sourceFileIO f = liftIO $ sourceFile
    <$> runResourceT (Conduit.sourceFile f $$ Conduit.sinkHash)
    <*> fmap fromIntegral (withBinaryFile f ReadMode hFileSize)
    <*> pure f
