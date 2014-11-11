{-# LANGUAGE DeriveGeneric #-}

-- Module      : Network.AWS.Error
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Error where

import Data.Text        (Text)
import GHC.Generics
import Network.AWS.Data

data ErrorType
    = Receiver
    | Sender
      deriving (Eq, Ord, Enum, Show, Generic)

instance FromXML ErrorType

data Message = Message
    { _msgType    :: !ErrorType
    , _msgCode    :: Text
    , _msgMessage :: Text
    } deriving (Eq, Ord, Show, Generic)

instance FromXML Message

data RESTError = RESTError
    { _errError     :: Message
    , _errRequestId :: Text
    } deriving (Eq, Show, Generic)

instance FromXML RESTError

-- cloudfront
-- autoscaling

-- <ErrorResponse xmlns="http://cloudfront.amazonaws.com/doc/2014-10-21/">
--    <Error>
--       <Type>Sender</Type>
--       <Code>InvalidURI</Code>
--       <Message>Could not parse the specified URI.</Message>
--    </Error>
--    <RequestId>410c2a4b-e435-49c9-8382-3770d80d7d4c</RequestId>
-- </ErrorResponse>
