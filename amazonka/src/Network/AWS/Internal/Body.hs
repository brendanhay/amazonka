-- Module      : Network.AWS.Internal.Body
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Body where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Crypto.Hash
import qualified Crypto.Hash.Conduit          as Conduit
import           Data.ByteString              (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Binary          as Conduit
import           Data.Int
import           Network.AWS.Data
import           Network.HTTP.Conduit
import           System.IO

-- | Safely construct a 'RqBody' from a 'FilePath', calculating the SHA256 hash
-- and file size.
--
-- /Note:/ While this function will perform in constant space, it will read the
-- entirety of the file contents _twice_. Firstly to calculate the SHA256 and
-- lastly to stream the contents to the socket during sending.
sourceFile :: MonadIO m => FilePath -> m RqBody
sourceFile f = liftIO $ sourceBody
    <$> runResourceT (Conduit.sourceFile f $$ Conduit.sinkHash)
    <*> fmap fromIntegral (withBinaryFile f ReadMode hFileSize)
    <*> pure (Conduit.sourceFile f)

-- | Unsafely construct a 'RqBody' from a 'Handle', calculating the SHA256 hash
-- and file size.
sourceHandle :: Digest SHA256 -> Int64 -> Handle -> RqBody
sourceHandle h n = sourceBody h n . Conduit.sourceHandle

-- | Unsafely construct a 'RqBody' from a source, manually specifying the
-- SHA256 hash and file size.
sourceBody :: Digest SHA256
           -> Int64
           -> Source (ResourceT IO) ByteString
           -> RqBody
sourceBody h n = RqBody h . requestBodySource n
