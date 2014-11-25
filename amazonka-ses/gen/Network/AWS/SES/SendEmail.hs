{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.SendEmail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Composes an email message based on input data, and then immediately queues
-- the message for sending.
--
-- You can only send email from verified email addresses and domains. If you
-- have not requested production access to Amazon SES, you must also verify
-- every recipient email address except for the recipients provided by the
-- Amazon SES mailbox simulator. For more information, go to the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SESDeveloper Guide>.  The total size of the message cannot exceed 10 MB.
--
-- Amazon SES has a limit on the total number of recipients per message: The
-- combined number of To:, CC: and BCC: email addresses cannot exceed 50. If you
-- need to send an email message to a larger audience, you can divide your
-- recipient list into groups of 50 or fewer, and then call Amazon SES
-- repeatedly to send the message to each group.
--
-- For every message that you send, the total number of recipients (To:, CC:
-- and BCC:) is counted against your /sending quota/ - the maximum number of
-- emails you can send in a 24-hour period. For information about your sending
-- quota, go to the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/manage-sending-limits.html Amazon SES Developer Guide>.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_SendEmail.html>
module Network.AWS.SES.SendEmail
    (
    -- * Request
      SendEmail
    -- ** Request constructor
    , sendEmail
    -- ** Request lenses
    , seDestination
    , seMessage
    , seReplyToAddresses
    , seReturnPath
    , seSource

    -- * Response
    , SendEmailResponse
    -- ** Response constructor
    , sendEmailResponse
    -- ** Response lenses
    , serMessageId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types
import qualified GHC.Exts

data SendEmail = SendEmail
    { _seDestination      :: Destination
    , _seMessage          :: Message
    , _seReplyToAddresses :: List "ToAddresses" Text
    , _seReturnPath       :: Maybe Text
    , _seSource           :: Text
    } deriving (Eq, Show)

-- | 'SendEmail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'seDestination' @::@ 'Destination'
--
-- * 'seMessage' @::@ 'Message'
--
-- * 'seReplyToAddresses' @::@ ['Text']
--
-- * 'seReturnPath' @::@ 'Maybe' 'Text'
--
-- * 'seSource' @::@ 'Text'
--
sendEmail :: Text -- ^ 'seSource'
          -> Destination -- ^ 'seDestination'
          -> Message -- ^ 'seMessage'
          -> SendEmail
sendEmail p1 p2 p3 = SendEmail
    { _seSource           = p1
    , _seDestination      = p2
    , _seMessage          = p3
    , _seReplyToAddresses = mempty
    , _seReturnPath       = Nothing
    }

-- | The destination for this email, composed of To:, CC:, and BCC: fields.
--
seDestination :: Lens' SendEmail Destination
seDestination = lens _seDestination (\s a -> s { _seDestination = a })

-- | The message to be sent.
--
seMessage :: Lens' SendEmail Message
seMessage = lens _seMessage (\s a -> s { _seMessage = a })

-- | The reply-to email address(es) for the message. If the recipient replies to
-- the message, each reply-to address will receive the reply.
--
seReplyToAddresses :: Lens' SendEmail [Text]
seReplyToAddresses =
    lens _seReplyToAddresses (\s a -> s { _seReplyToAddresses = a })
        . _List

-- | The email address to which bounces and complaints are to be forwarded when
-- feedback forwarding is enabled. If the message cannot be delivered to the
-- recipient, then an error message will be returned from the recipient's ISP;
-- this message will then be forwarded to the email address specified by the 'ReturnPath' parameter.
--
seReturnPath :: Lens' SendEmail (Maybe Text)
seReturnPath = lens _seReturnPath (\s a -> s { _seReturnPath = a })

-- | The identity's email address.
--
-- By default, the string must be 7-bit ASCII. If the text must contain any
-- other characters, then you must use MIME encoded-word syntax (RFC 2047)
-- instead of a literal string. MIME encoded-word syntax uses the following
-- form: '=?charset?encoding?encoded-text?='. For more information, see <http://tools.ietf.org/html/rfc2047 RFC 2047>.
--
seSource :: Lens' SendEmail Text
seSource = lens _seSource (\s a -> s { _seSource = a })

newtype SendEmailResponse = SendEmailResponse
    { _serMessageId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'SendEmailResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'serMessageId' @::@ 'Text'
--
sendEmailResponse :: Text -- ^ 'serMessageId'
                  -> SendEmailResponse
sendEmailResponse p1 = SendEmailResponse
    { _serMessageId = p1
    }

-- | The unique message identifier returned from the 'SendEmail' action.
--
serMessageId :: Lens' SendEmailResponse Text
serMessageId = lens _serMessageId (\s a -> s { _serMessageId = a })

instance ToPath SendEmail where
    toPath = const "/"

instance ToQuery SendEmail where
    toQuery SendEmail{..} = mconcat
        [ "Destination"      =? _seDestination
        , "Message"          =? _seMessage
        , "ReplyToAddresses" =? _seReplyToAddresses
        , "ReturnPath"       =? _seReturnPath
        , "Source"           =? _seSource
        ]

instance ToHeaders SendEmail

instance AWSRequest SendEmail where
    type Sv SendEmail = SES
    type Rs SendEmail = SendEmailResponse

    request  = post "SendEmail"
    response = xmlResponse

instance FromXML SendEmailResponse where
    parseXML = withElement "SendEmailResult" $ \x -> SendEmailResponse
        <$> x .@  "MessageId"
