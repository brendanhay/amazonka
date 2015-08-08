{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SendEmail
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Composes an email message based on input data, and then immediately
-- queues the message for sending.
--
-- There are several important points to know about @SendEmail@:
--
-- -   You can only send email from verified email addresses and domains;
--     otherwise, you will get an \"Email address not verified\" error. If
--     your account is still in the Amazon SES sandbox, you must also
--     verify every recipient email address except for the recipients
--     provided by the Amazon SES mailbox simulator. For more information,
--     go to the
--     <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide>.
-- -   The total size of the message cannot exceed 10 MB. This includes any
--     attachments that are part of the message.
-- -   Amazon SES has a limit on the total number of recipients per
--     message. The combined number of To:, CC: and BCC: email addresses
--     cannot exceed 50. If you need to send an email message to a larger
--     audience, you can divide your recipient list into groups of 50 or
--     fewer, and then call Amazon SES repeatedly to send the message to
--     each group.
-- -   For every message that you send, the total number of recipients
--     (To:, CC: and BCC:) is counted against your sending quota - the
--     maximum number of emails you can send in a 24-hour period. For
--     information about your sending quota, go to the
--     <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/manage-sending-limits.html Amazon SES Developer Guide>.
--
-- /See:/ <http://docs.aws.amazon.com/ses/latest/APIReference/API_SendEmail.html AWS API Reference> for SendEmail.
module Network.AWS.SES.SendEmail
    (
    -- * Creating a Request
      SendEmail
    , sendEmail
    -- * Request Lenses
    , seReturnPath
    , seSourceARN
    , seReturnPathARN
    , seReplyToAddresses
    , seSource
    , seDestination
    , seMessage

    -- * Destructuring the Response
    , SendEmailResponse
    , sendEmailResponse
    -- * Response Lenses
    , sersStatus
    , sersMessageId
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
-- * 'seSourceARN'
--
-- * 'seReturnPathARN'
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
    , _seSourceARN        :: !(Maybe Text)
    , _seReturnPathARN    :: !(Maybe Text)
    , _seReplyToAddresses :: !(Maybe [Text])
    , _seSource           :: !Text
    , _seDestination      :: !Destination
    , _seMessage          :: !Message
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SendEmail' smart constructor.
sendEmail :: Text -> Destination -> Message -> SendEmail
sendEmail pSource_ pDestination_ pMessage_ =
    SendEmail'
    { _seReturnPath = Nothing
    , _seSourceARN = Nothing
    , _seReturnPathARN = Nothing
    , _seReplyToAddresses = Nothing
    , _seSource = pSource_
    , _seDestination = pDestination_
    , _seMessage = pMessage_
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

-- | This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to send for the email address specified in the @Source@
-- parameter.
--
-- For example, if the owner of @example.com@ (which has ARN
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@) attaches a
-- policy to it that authorizes you to send from @user\@example.com@, then
-- you would specify the @SourceArn@ to be
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@, and the
-- @Source@ to be @user\@example.com@.
--
-- For more information about sending authorization, see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
seSourceARN :: Lens' SendEmail (Maybe Text)
seSourceARN = lens _seSourceARN (\ s a -> s{_seSourceARN = a});

-- | This parameter is used only for sending authorization. It is the ARN of
-- the identity that is associated with the sending authorization policy
-- that permits you to use the email address specified in the @ReturnPath@
-- parameter.
--
-- For example, if the owner of @example.com@ (which has ARN
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@) attaches a
-- policy to it that authorizes you to use @feedback\@example.com@, then
-- you would specify the @ReturnPathArn@ to be
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@, and the
-- @ReturnPath@ to be @feedback\@example.com@.
--
-- For more information about sending authorization, see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
seReturnPathARN :: Lens' SendEmail (Maybe Text)
seReturnPathARN = lens _seReturnPathARN (\ s a -> s{_seReturnPathARN = a});

-- | The reply-to email address(es) for the message. If the recipient replies
-- to the message, each reply-to address will receive the reply.
seReplyToAddresses :: Lens' SendEmail [Text]
seReplyToAddresses = lens _seReplyToAddresses (\ s a -> s{_seReplyToAddresses = a}) . _Default . _Coerce;

-- | The email address that is sending the email. This email address must be
-- either individually verified with Amazon SES, or from a domain that has
-- been verified with Amazon SES. For information about verifying
-- identities, see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide>.
--
-- If you are sending on behalf of another user and have been permitted to
-- do so by a sending authorization policy, then you must also specify the
-- @SourceArn@ parameter. For more information about sending authorization,
-- see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide>.
--
-- In all cases, the email address must be 7-bit ASCII. If the text must
-- contain any other characters, then you must use MIME encoded-word syntax
-- (RFC 2047) instead of a literal string. MIME encoded-word syntax uses
-- the following form: @=?charset?encoding?encoded-text?=@. For more
-- information, see <http://tools.ietf.org/html/rfc2047 RFC 2047>.
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
        request = postQuery
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
               "SourceArn" =: _seSourceARN,
               "ReturnPathArn" =: _seReturnPathARN,
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
-- * 'sersStatus'
--
-- * 'sersMessageId'
data SendEmailResponse = SendEmailResponse'
    { _sersStatus    :: !Int
    , _sersMessageId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SendEmailResponse' smart constructor.
sendEmailResponse :: Int -> Text -> SendEmailResponse
sendEmailResponse pStatus_ pMessageId_ =
    SendEmailResponse'
    { _sersStatus = pStatus_
    , _sersMessageId = pMessageId_
    }

-- | Undocumented member.
sersStatus :: Lens' SendEmailResponse Int
sersStatus = lens _sersStatus (\ s a -> s{_sersStatus = a});

-- | The unique message identifier returned from the @SendEmail@ action.
sersMessageId :: Lens' SendEmailResponse Text
sersMessageId = lens _sersMessageId (\ s a -> s{_sersMessageId = a});
