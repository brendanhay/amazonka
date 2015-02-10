{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.AWS.Data.Internal.Body
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.Body where

import           Control.Lens
import           Control.Monad.Morph
import           Control.Monad.Trans.Resource
import           Crypto.Hash
import           Data.Aeson
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Lazy.Char8           as LBS8
import           Data.Conduit
import           Data.List                            (intersperse)
import           Data.Monoid
import           Data.String
import           Network.AWS.Data.Internal.ByteString
import           Network.HTTP.Client

data RsBody = RsBody (ResumableSource (ResourceT IO) ByteString)

makePrisms ''RsBody

instance ToBuilder RsBody where
    build = const "RsBody { ResumableSource (ResourceT IO) ByteString }"

instance Show RsBody where
    show = LBS8.unpack . buildBS

connectBody :: MonadResource m => RsBody -> Sink ByteString m a -> m a
connectBody (RsBody src) sink = hoist liftResourceT src $$+- sink

data RqBody = RqBody
    { _bdyHash :: Digest SHA256
    , _bdyBody :: RequestBody
    }

makeLenses ''RqBody

bodyHash :: RqBody -> ByteString
bodyHash = digestToHexByteString . _bdyHash

instance ToBuilder RqBody where
    build x@(RqBody _ b) = mconcat $ intersperse "\n"
        [ "    Body {"
        , "      hash    = "  <> build (bodyHash x)
        , "      payload =\n" <> build b
        , "    }"
        ]

instance Show RqBody where
    show = LBS8.unpack . buildBS . _bdyBody

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
    toBody = const (RqBody (hash "") (RequestBodyLBS mempty))

instance ToBody RqBody where
    toBody = id

instance ToBody LBS.ByteString where
    toBody lbs = RqBody (hashlazy lbs) (RequestBodyLBS lbs)

instance ToBody ByteString where
    toBody = toBody . LBS.fromStrict

instance ToBody Value where
    toBody = toBody . encode
