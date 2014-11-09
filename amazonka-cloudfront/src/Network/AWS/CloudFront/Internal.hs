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

-- <ErrorResponse xmlns="http://cloudfront.amazonaws.com/doc/2014-10-21/">
--    <Error>
--       <Type>Sender</Type>
--       <Code>InvalidURI</Code>
--       <Message>Could not parse the specified URI.</Message>
--    </Error>
--    <RequestId>410c2a4b-e435-49c9-8382-3770d80d7d4c</RequestId>
-- </ErrorResponse>
