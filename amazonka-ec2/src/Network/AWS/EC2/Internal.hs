{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.Internal
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.EC2.Internal where

import GHC.Generics
import Network.AWS.Data
import Network.AWS.Error
import Network.AWS.Prelude

data Message = Message
    { _msgCode    :: ErrorCode
    , _msgMessage :: Text
    } deriving (Eq, Ord, Show, Generic)

instance AWSErrorCode Message where
    awsErrorCode = _msgCode

instance FromXML Message where
    parseXML x = Message
        <$> x .@ "Code"
        <*> x .@ "Message"

msgCode :: Lens' Message ErrorCode
msgCode = lens _msgCode (\s x -> s { _msgCode = x })

msgMessage :: Lens' Message Text
msgMessage = lens _msgMessage (\s x -> s { _msgMessage = x })

data EC2Error = EC2Error
    { _errRequestID :: Text
    , _errErrors    :: List1 "Error" Message
    } deriving (Eq, Ord, Show, Generic)

instance AWSErrorCode EC2Error where
    awsErrorCode e = let List1 (m :| _) = _errErrors e in awsErrorCode m

instance FromXML EC2Error where
    parseXML x = EC2Error
        <$> x .@ "RequestID"
        <*> x .@ "Errors"

errRequestID :: Lens' EC2Error Text
errRequestID = lens _errRequestID (\s x -> s { _errRequestID = x })

errErrors :: Lens' EC2Error (NonEmpty Message)
errErrors = lens _errErrors (\s x -> s { _errErrors = x }) . _List1

-- <Response>
--     <Errors>
--          <Error>
--            <Code>Error code text</Code>
--            <Message>Error message</Message>
--          </Error>
--     </Errors>
--     <RequestID>request ID</RequestID>
-- </Response>

