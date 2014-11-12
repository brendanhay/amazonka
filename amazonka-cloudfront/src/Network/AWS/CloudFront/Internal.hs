{-# LANGUAGE DeriveGeneric #-}

-- Module      : Network.AWS.CloudFront.Internal
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudFront.Internal where

import GHC.Generics
import Network.AWS.Data
import Network.AWS.Prelude

data Message = Message
    { _msgType    :: Text
    , _msgCode    :: Text
    , _msgMessage :: Text
    } deriving (Eq, Ord, Show, Generic)

instance FromXML Message

data CloudFrontServiceError = CloudFrontServiceError
    { _errError     :: Message
    , _errRequestId :: Text
    } deriving (Eq, Ord, Show, Generic)

instance FromXML CloudFrontServiceError
