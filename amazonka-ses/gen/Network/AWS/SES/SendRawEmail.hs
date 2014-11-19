{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.SendRawEmail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sends an email message, with header and content specified by the client.
-- The SendRawEmail action is useful for sending multipart MIME emails. The
-- raw text of the message must comply with Internet email standards;
-- otherwise, the message cannot be sent. You can only send email from
-- verified email addresses and domains. If you have not requested production
-- access to Amazon SES, you must also verify every recipient email address
-- except for the recipients provided by the Amazon SES mailbox simulator. For
-- more information, go to the Amazon SES Developer Guide. The total size of
-- the message cannot exceed 10 MB. This includes any attachments that are
-- part of the message. Amazon SES has a limit on the total number of
-- recipients per message: The combined number of To:, CC: and BCC: email
-- addresses cannot exceed 50. If you need to send an email message to a
-- larger audience, you can divide your recipient list into groups of 50 or
-- fewer, and then call Amazon SES repeatedly to send the message to each
-- group. The To:, CC:, and BCC: headers in the raw message can contain a
-- group list. Note that each recipient in a group list counts towards the
-- 50-recipient limit. For every message that you send, the total number of
-- recipients (To:, CC: and BCC:) is counted against your sending quota - the
-- maximum number of emails you can send in a 24-hour period. For information
-- about your sending quota, go to the Amazon SES Developer Guide.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_SendRawEmail.html>
module Network.AWS.SES.SendRawEmail
    (
    -- * Request
      SendRawEmail
    -- ** Request constructor
    , sendRawEmail
    -- ** Request lenses
    , sreDestinations
    , sreRawMessage
    , sreSource

    -- * Response
    , SendRawEmailResponse
    -- ** Response constructor
    , sendRawEmailResponse
    -- ** Response lenses
    , srerMessageId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types
import qualified GHC.Exts

data SendRawEmail = SendRawEmail
    { _sreDestinations :: [Text]
    , _sreRawMessage   :: RawMessage
    , _sreSource       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'SendRawEmail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sreDestinations' @::@ ['Text']
--
-- * 'sreRawMessage' @::@ 'RawMessage'
--
-- * 'sreSource' @::@ 'Maybe' 'Text'
--
sendRawEmail :: RawMessage -- ^ 'sreRawMessage'
             -> SendRawEmail
sendRawEmail p1 = SendRawEmail
    { _sreRawMessage   = p1
    , _sreSource       = Nothing
    , _sreDestinations = mempty
    }

-- | A list of destinations for the message, consisting of To:, CC:, and BCC:
-- addresses.
sreDestinations :: Lens' SendRawEmail [Text]
sreDestinations = lens _sreDestinations (\s a -> s { _sreDestinations = a })

-- | The raw text of the message. The client is responsible for ensuring the
-- following: Message must contain a header and a body, separated by a blank
-- line. All required header fields must be present. Each part of a
-- multipart MIME message must be formatted properly. MIME content types
-- must be among those supported by Amazon SES. For more information, go to
-- the Amazon SES Developer Guide. Content must be base64-encoded, if MIME
-- requires it.
sreRawMessage :: Lens' SendRawEmail RawMessage
sreRawMessage = lens _sreRawMessage (\s a -> s { _sreRawMessage = a })

-- | The identity's email address. By default, the string must be 7-bit ASCII.
-- If the text must contain any other characters, then you must use MIME
-- encoded-word syntax (RFC 2047) instead of a literal string. MIME
-- encoded-word syntax uses the following form:
-- =?charset?encoding?encoded-text?=. For more information, see RFC 2047.
sreSource :: Lens' SendRawEmail (Maybe Text)
sreSource = lens _sreSource (\s a -> s { _sreSource = a })

newtype SendRawEmailResponse = SendRawEmailResponse
    { _srerMessageId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'SendRawEmailResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srerMessageId' @::@ 'Text'
--
sendRawEmailResponse :: Text -- ^ 'srerMessageId'
                     -> SendRawEmailResponse
sendRawEmailResponse p1 = SendRawEmailResponse
    { _srerMessageId = p1
    }

-- | The unique message identifier returned from the SendRawEmail action.
srerMessageId :: Lens' SendRawEmailResponse Text
srerMessageId = lens _srerMessageId (\s a -> s { _srerMessageId = a })

instance ToPath SendRawEmail where
    toPath = const "/"

instance ToQuery SendRawEmail

instance ToHeaders SendRawEmail

instance AWSRequest SendRawEmail where
    type Sv SendRawEmail = SES
    type Rs SendRawEmail = SendRawEmailResponse

    request  = post "SendRawEmail"
    response = xmlResponse

instance FromXML SendRawEmailResponse where
    parseXML = withElement "SendRawEmailResult" $ \x ->
            <$> x .@ "MessageId"
