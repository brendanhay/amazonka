{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SendEmail
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Composes an email message based on input data, and then immediately queues the message for sending.
--
--
-- There are several important points to know about @SendEmail@ :
--
--     * You can only send email from verified email addresses and domains; otherwise, you will get an "Email address not verified" error. If your account is still in the Amazon SES sandbox, you must also verify every recipient email address except for the recipients provided by the Amazon SES mailbox simulator. For more information, go to the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> .
--
--     * The total size of the message cannot exceed 10 MB. This includes any attachments that are part of the message.
--
--     * You must provide at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If any email address you provide is invalid, Amazon SES rejects the entire email.
--
--     * Amazon SES has a limit on the total number of recipients per message. The combined number of To:, CC: and BCC: email addresses cannot exceed 50. If you need to send an email message to a larger audience, you can divide your recipient list into groups of 50 or fewer, and then call Amazon SES repeatedly to send the message to each group.
--
--     * For every message that you send, the total number of recipients (To:, CC: and BCC:) is counted against your sending quota - the maximum number of emails you can send in a 24-hour period. For information about your sending quota, go to the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/manage-sending-limits.html Amazon SES Developer Guide> .
--
--
--
module Network.AWS.SES.SendEmail
    (
    -- * Creating a Request
      sendEmail
    , SendEmail
    -- * Request Lenses
    , seReturnPath
    , seConfigurationSetName
    , seSourceARN
    , seReturnPathARN
    , seTags
    , seReplyToAddresses
    , seSource
    , seDestination
    , seMessage

    -- * Destructuring the Response
    , sendEmailResponse
    , SendEmailResponse
    -- * Response Lenses
    , sersResponseStatus
    , sersMessageId
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | Represents a request to send a single formatted email using Amazon SES. For more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-formatted.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'sendEmail' smart constructor.
data SendEmail = SendEmail'
    { _seReturnPath           :: !(Maybe Text)
    , _seConfigurationSetName :: !(Maybe Text)
    , _seSourceARN            :: !(Maybe Text)
    , _seReturnPathARN        :: !(Maybe Text)
    , _seTags                 :: !(Maybe [MessageTag])
    , _seReplyToAddresses     :: !(Maybe [Text])
    , _seSource               :: !Text
    , _seDestination          :: !Destination
    , _seMessage              :: !Message
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SendEmail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seReturnPath' - The email address to which bounces and complaints are to be forwarded when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
--
-- * 'seConfigurationSetName' - The name of the configuration set to use when you send an email using @SendEmail@ .
--
-- * 'seSourceARN' - This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter. For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ . For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- * 'seReturnPathARN' - This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter. For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ . For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- * 'seTags' - A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
--
-- * 'seReplyToAddresses' - The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
--
-- * 'seSource' - The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> . If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> . In all cases, the email address must be 7-bit ASCII. If the text must contain any other characters, then you must use MIME encoded-word syntax (RFC 2047) instead of a literal string. MIME encoded-word syntax uses the following form: @=?charset?encoding?encoded-text?=@ . For more information, see <http://tools.ietf.org/html/rfc2047 RFC 2047> .
--
-- * 'seDestination' - The destination for this email, composed of To:, CC:, and BCC: fields.
--
-- * 'seMessage' - The message to be sent.
sendEmail
    :: Text -- ^ 'seSource'
    -> Destination -- ^ 'seDestination'
    -> Message -- ^ 'seMessage'
    -> SendEmail
sendEmail pSource_ pDestination_ pMessage_ =
    SendEmail'
    { _seReturnPath = Nothing
    , _seConfigurationSetName = Nothing
    , _seSourceARN = Nothing
    , _seReturnPathARN = Nothing
    , _seTags = Nothing
    , _seReplyToAddresses = Nothing
    , _seSource = pSource_
    , _seDestination = pDestination_
    , _seMessage = pMessage_
    }

-- | The email address to which bounces and complaints are to be forwarded when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
seReturnPath :: Lens' SendEmail (Maybe Text)
seReturnPath = lens _seReturnPath (\ s a -> s{_seReturnPath = a});

-- | The name of the configuration set to use when you send an email using @SendEmail@ .
seConfigurationSetName :: Lens' SendEmail (Maybe Text)
seConfigurationSetName = lens _seConfigurationSetName (\ s a -> s{_seConfigurationSetName = a});

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter. For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ . For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
seSourceARN :: Lens' SendEmail (Maybe Text)
seSourceARN = lens _seSourceARN (\ s a -> s{_seSourceARN = a});

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter. For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ . For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
seReturnPathARN :: Lens' SendEmail (Maybe Text)
seReturnPathARN = lens _seReturnPathARN (\ s a -> s{_seReturnPathARN = a});

-- | A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
seTags :: Lens' SendEmail [MessageTag]
seTags = lens _seTags (\ s a -> s{_seTags = a}) . _Default . _Coerce;

-- | The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
seReplyToAddresses :: Lens' SendEmail [Text]
seReplyToAddresses = lens _seReplyToAddresses (\ s a -> s{_seReplyToAddresses = a}) . _Default . _Coerce;

-- | The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> . If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> . In all cases, the email address must be 7-bit ASCII. If the text must contain any other characters, then you must use MIME encoded-word syntax (RFC 2047) instead of a literal string. MIME encoded-word syntax uses the following form: @=?charset?encoding?encoded-text?=@ . For more information, see <http://tools.ietf.org/html/rfc2047 RFC 2047> .
seSource :: Lens' SendEmail Text
seSource = lens _seSource (\ s a -> s{_seSource = a});

-- | The destination for this email, composed of To:, CC:, and BCC: fields.
seDestination :: Lens' SendEmail Destination
seDestination = lens _seDestination (\ s a -> s{_seDestination = a});

-- | The message to be sent.
seMessage :: Lens' SendEmail Message
seMessage = lens _seMessage (\ s a -> s{_seMessage = a});

instance AWSRequest SendEmail where
        type Rs SendEmail = SendEmailResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "SendEmailResult"
              (\ s h x ->
                 SendEmailResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "MessageId"))

instance Hashable SendEmail

instance NFData SendEmail

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
               "ConfigurationSetName" =: _seConfigurationSetName,
               "SourceArn" =: _seSourceARN,
               "ReturnPathArn" =: _seReturnPathARN,
               "Tags" =: toQuery (toQueryList "member" <$> _seTags),
               "ReplyToAddresses" =:
                 toQuery
                   (toQueryList "member" <$> _seReplyToAddresses),
               "Source" =: _seSource,
               "Destination" =: _seDestination,
               "Message" =: _seMessage]

-- | Represents a unique message ID.
--
--
--
-- /See:/ 'sendEmailResponse' smart constructor.
data SendEmailResponse = SendEmailResponse'
    { _sersResponseStatus :: !Int
    , _sersMessageId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SendEmailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sersResponseStatus' - -- | The response status code.
--
-- * 'sersMessageId' - The unique message identifier returned from the @SendEmail@ action.
sendEmailResponse
    :: Int -- ^ 'sersResponseStatus'
    -> Text -- ^ 'sersMessageId'
    -> SendEmailResponse
sendEmailResponse pResponseStatus_ pMessageId_ =
    SendEmailResponse'
    { _sersResponseStatus = pResponseStatus_
    , _sersMessageId = pMessageId_
    }

-- | -- | The response status code.
sersResponseStatus :: Lens' SendEmailResponse Int
sersResponseStatus = lens _sersResponseStatus (\ s a -> s{_sersResponseStatus = a});

-- | The unique message identifier returned from the @SendEmail@ action.
sersMessageId :: Lens' SendEmailResponse Text
sersMessageId = lens _sersMessageId (\ s a -> s{_sersMessageId = a});

instance NFData SendEmailResponse
