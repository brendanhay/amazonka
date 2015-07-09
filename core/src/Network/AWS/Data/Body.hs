{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

-- |
-- Module      : Network.AWS.Data.Body
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Body where

import           Control.Lens
import           Control.Monad.Morph
import           Control.Monad.Trans.Resource
import           Crypto.Hash
import           Data.Aeson
import qualified Data.ByteString.Char8        as BS8
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Char8   as LBS8
import           Data.Conduit
import           Data.Data                    (Data, Typeable)
import           Data.Monoid
import           Data.String
import           GHC.Generics                 (Generic)
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.XML         (encodeXML)
import           Network.HTTP.Client
import           Text.XML                     (Element)

newtype RsBody = RsBody (ResumableSource (ResourceT IO) ByteString)

_RsBody :: Iso' RsBody (ResumableSource (ResourceT IO) ByteString)
_RsBody = iso (\(RsBody x) -> x) RsBody

instance Show RsBody where
    show = const "RsBody { ResumableSource (ResourceT IO) ByteString }"

sinkBody :: MonadResource m => RsBody -> Sink ByteString m a -> m a
sinkBody (RsBody src) sink = hoist liftResourceT src $$+- sink

data RqBody = RqBody
    { _bdyHash :: Digest SHA256
    , _bdyBody :: RequestBody
    }

bodyHash :: Getter RqBody ByteString
bodyHash = to (digestToHexByteString . _bdyHash)

instance Show RqBody where
    show b = "RqBody { RequestBody " ++ BS8.unpack (b ^. bodyHash) ++ " }"

instance IsString RqBody where
    fromString = toBody . LBS8.pack

isStreaming :: RqBody -> Bool
isStreaming b =
    case _bdyBody b of
        RequestBodyLBS           {} -> False
        RequestBodyBS            {} -> False
        RequestBodyBuilder       {} -> False
        RequestBodyStream        {} -> True
        RequestBodyStreamChunked {} -> True

class ToBody a where
    toBody :: a -> RqBody

instance ToBody RqBody where
    toBody = id

instance ToBody LBS.ByteString where
    toBody lbs = RqBody (hashlazy lbs) (RequestBodyLBS lbs)

instance ToBody ByteString where
    toBody = toBody . LBS.fromStrict

instance ToBody Value where
    toBody = toBody . encode

instance ToBody Element where
    toBody = toBody . encodeXML
