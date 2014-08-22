{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Internal.Data.Body
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Data.Body
    (
    -- * Response
      RsBody      (..)

    -- * Request
    , RqBody      (..)

    -- * Classes
    , ToBody      (..)
    ) where

import           Crypto.Hash
import           Data.Aeson
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Lazy.Char8     as LBS8
import           Data.Conduit
import           Data.Monoid
import           Data.String
import qualified Data.Text                      as Text
import           Network.AWS.Internal.Data.Text
import           Network.HTTP.Conduit

data RsBody where
    RsBody :: Monad m => ResumableSource m ByteString -> RsBody

instance ToText RsBody where
    toText = const "RsBody <body>"

instance Show RsBody where
    show = Text.unpack . toText

data RqBody = RqBody
    { _bdyHash :: Digest SHA256
    , _bdyBody :: RequestBody
    }

instance ToText RqBody where
    toText (RqBody h _) = "RqBody " <> toText h <> " <body>"

instance Show RqBody where
    show = Text.unpack . toText

instance IsString RqBody where
    fromString = toBody . LBS8.pack

class ToBody a where
    toBody :: a -> RqBody
    toBody = const (RqBody (hash "") (RequestBodyLBS mempty))

instance ToBody RqBody where
    toBody = id

instance ToBody LBS.ByteString where
    toBody lbs = RqBody (hashlazy lbs) (RequestBodyLBS lbs)

instance ToBody ByteString where
    toBody = toBody . LBS.fromStrict

instance ToBody Value where
    toBody = toBody . encode

-- sourceBody :: Digest SHA256 -> Int64 -> Source IO ByteString -> RqBody
-- sourceBody h n = RqBody h . RequestBodyStream n . sourcePopper

-- sourceHandle :: Digest SHA256 -> Int64 -> Handle -> RqBody
-- sourceHandle h n = sourceBody h n . Conduit.sourceHandle

-- sourceFile :: Digest SHA256 -> Int64 -> FilePath -> RqBody
-- sourceFile h n = sourceBody h n . hoist runResourceT . Conduit.sourceFile

-- sourceFileIO :: MonadIO m => FilePath ->  RqBody
-- sourceFileIO f = sourceFile
--     <$> runResourceT (Conduit.sourceFile f $$ Conduit.sinkHash)
--     <*> (fromIntegral <$> withBinaryFile f ReadMode hFileSize)
--     <*> pure f
