{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SES.SendEmail
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Composes an email message based on input data, and then immediately
-- queues the message for sending.
--
-- You can only send email from verified email addresses and domains. If
-- your account is still in the Amazon SES sandbox, you must also verify
-- every recipient email address except for the recipients provided by the
-- Amazon SES mailbox simulator. For more information, go to the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide>.
--
-- The total size of the message cannot exceed 10 MB.
--
-- Amazon SES has a limit on the total number of recipients per message:
-- The combined number of To:, CC: and BCC: email addresses cannot exceed
-- 50. If you need to send an email message to a larger audience, you can
-- divide your recipient list into groups of 50 or fewer, and then call
-- Amazon SES repeatedly to send the message to each group.
--
-- For every message that you send, the total number of recipients (To:,
-- CC: and BCC:) is counted against your /sending quota/ - the maximum
-- number of emails you can send in a 24-hour period. For information about
-- your sending quota, go to the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/manage-sending-limits.html Amazon SES Developer Guide>.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_SendEmail.html>
module Network.AWS.SES.SendEmail
    (
    -- * Request
      SendEmail
    -- ** Request constructor
    , sendEmail
    -- ** Request lenses
    , seReturnPath
    , seReplyToAddresses
    , seSource
    , seDestination
    , seMessage

    -- * Response
    , SendEmailResponse
    -- ** Response constructor
    , sendEmailResponse
    -- ** Response lenses
    , serStatus
    , serMessageId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types

-- | Represents a request instructing the service to send a single email
-- message.
--
-- This datatype can be used in application code to compose a message
-- consisting of source, destination, message, reply-to, and return-path
-- parts. This object can then be sent using the @SendEmail@ action.
--
-- /See:/ 'sendEmail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'seReturnPath'
--
-- * 'seReplyToAddresses'
--
-- * 'seSource'
--
-- * 'seDestination'
--
-- * 'seMessage'
data SendEmail = SendEmail'
    { _seReturnPath       :: !(Maybe Text)
    , _seReplyToAddresses :: !(Maybe [Text])
    , _seSource           :: !Text
    , _seDestination      :: !Destination
    , _seMessage          :: !Message
    } deriving (Eq,Read,Show)

-- | 'SendEmail' smart constructor.
sendEmail :: Text -> Destination -> Message -> SendEmail
sendEmail pSource pDestination pMessage =
    SendEmail'
    { _seReturnPath = Nothing
    , _seReplyToAddresses = Nothing
    , _seSource = pSource
    , _seDestination = pDestination
    , _seMessage = pMessage
    }

-- | The email address to which bounces and complaints are to be forwarded
-- when feedback forwarding is enabled. If the message cannot be delivered
-- to the recipient, then an error message will be returned from the
-- recipient\'s ISP; this message will then be forwarded to the email
-- address specified by the @ReturnPath@ parameter. The @ReturnPath@
-- parameter is never overwritten. This email address must be either
-- individually verified with Amazon SES, or from a domain that has been
-- verified with Amazon SES.
seReturnPath :: Lens' SendEmail (Maybe Text)
seReturnPath = lens _seReturnPath (\ s a -> s{_seReturnPath = a});

-- | The reply-to email address(es) for the message. If the recipient replies
-- to the message, each reply-to address will receive the reply.
seReplyToAddresses :: Lens' SendEmail [Text]
seReplyToAddresses = lens _seReplyToAddresses (\ s a -> s{_seReplyToAddresses = a}) . _Default;

-- | The identity\'s email address.
--
-- By default, the string must be 7-bit ASCII. If the text must contain any
-- other characters, then you must use MIME encoded-word syntax (RFC 2047)
-- instead of a literal string. MIME encoded-word syntax uses the following
-- form: @=?charset?encoding?encoded-text?=@. For more information, see
-- <http://tools.ietf.org/html/rfc2047 RFC 2047>.
seSource :: Lens' SendEmail Text
seSource = lens _seSource (\ s a -> s{_seSource = a});

-- | The destination for this email, composed of To:, CC:, and BCC: fields.
seDestination :: Lens' SendEmail Destination
seDestination = lens _seDestination (\ s a -> s{_seDestination = a});

-- | The message to be sent.
seMessage :: Lens' SendEmail Message
seMessage = lens _seMessage (\ s a -> s{_seMessage = a});

instance AWSRequest SendEmail where
        type Sv SendEmail = SES
        type Rs SendEmail = SendEmailResponse
        request = post
        response
          = receiveXMLWrapper "SendEmailResult"
              (\ s h x ->
                 SendEmailResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "MessageId"))

instance ToHeaders SendEmail where
        toHeaders = const mempty

instance ToPath SendEmail where
        toPath = const "/"

instance ToQuery SendEmail where
        toQuery SendEmail'{..}
          = mconcat
              ["Action" =: ("SendEmail" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ReturnPath" =: _seReturnPath,
               "ReplyToAddresses" =:
                 toQuery
                   (toQueryList "member" <$> _seReplyToAddresses),
               "Source" =: _seSource,
               "Destination" =: _seDestination,
               "Message" =: _seMessage]

-- | Represents a unique message ID returned from a successful @SendEmail@
-- request.
--
-- /See:/ 'sendEmailResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'serStatus'
--
-- * 'serMessageId'
data SendEmailResponse = SendEmailResponse'
    { _serStatus    :: !Int
    , _serMessageId :: !Text
    } deriving (Eq,Read,Show)

-- | 'SendEmailResponse' smart constructor.
sendEmailResponse :: Int -> Text -> SendEmailResponse
sendEmailResponse pStatus pMessageId =
    SendEmailResponse'
    { _serStatus = pStatus
    , _serMessageId = pMessageId
    }

-- | FIXME: Undocumented member.
serStatus :: Lens' SendEmailResponse Int
serStatus = lens _serStatus (\ s a -> s{_serStatus = a});

-- | The unique message identifier returned from the @SendEmail@ action.
serMessageId :: Lens' SendEmailResponse Text
serMessageId = lens _serMessageId (\ s a -> s{_serMessageId = a});
