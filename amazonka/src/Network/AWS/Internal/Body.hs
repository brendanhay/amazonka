-- |
-- Module      : Network.AWS.Internal.Body
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Internal.Body
    (
    -- * Streaming Files
    -- ** Request Bodies
      ToBody (..)
    , RqBody
    , sourceBody
    , sourceHandle
    , sourceFile
    , sourceFileIO
    , getFileSize
    -- ** Response Bodies
    , RsBody
    , sinkBody
    , sinkHash
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Morph
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit
import           Data.Int
import           Network.AWS.Prelude
import qualified Network.HTTP.Client          as Client
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

-- | Construct a 'RqBody' from a 'FilePath', calculating the SHA256 hash
-- and file size.
--
-- /Note:/ While this function will perform in constant space, it will enumerate the
-- entirety of the file contents _twice_. Firstly to calculate the SHA256 and
-- lastly to stream the contents to the socket during sending.
sourceFileIO :: MonadIO m => FilePath -> m RqBody
sourceFileIO f = liftIO $
    RqBody <$> runResourceT (Conduit.sourceFile f $$ sinkHash)
           <*> Client.streamFile f

-- | Convenience function for obtaining the size of a file.
getFileSize :: MonadIO m => FilePath -> m Int64
getFileSize f = liftIO $ fromIntegral `liftM` withBinaryFile f ReadMode hFileSize

-- | Connect a 'Sink' to a reponse body.
sinkBody :: MonadResource m => RsBody -> Sink ByteString m a -> m a
sinkBody (RsBody src) sink = hoist liftResourceT src $$+- sink

-- | A cryptonite compatible incremental hash sink.
sinkHash :: Monad m => Consumer ByteString m (Digest SHA256)
sinkHash = sink hashInit
  where
    sink ctx = do
        b <- await
        case b of
            Nothing -> return $! hashFinalize ctx
            Just bs -> sink $! hashUpdate ctx bs
