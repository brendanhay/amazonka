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
-- Module      : Network.AWS.SES.SendTemplatedEmail
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Composes an email message using an email template and immediately queues it for sending.
--
--
-- In order to send email using the @SendTemplatedEmail@ operation, your call to the API must meet the following requirements:
--
--     * The call must refer to an existing email template. You can create email templates using the 'CreateTemplate' operation.
--
--     * The message must be sent from a verified email address or domain.
--
--     * If your account is still in the Amazon SES sandbox, you may only send to verified addresses or domains, or to email addresses associated with the Amazon SES Mailbox Simulator. For more information, see <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Verifying Email Addresses and Domains> in the /Amazon SES Developer Guide./
--
--     * The total size of the message, including attachments, must be less than 10 MB.
--
--     * Calls to the @SendTemplatedEmail@ operation may only include one @Destination@ parameter. A destination is a set of recipients who will receive the same version of the email. The @Destination@ parameter can include up to 50 recipients, across the To:, CC: and BCC: fields.
--
--     * The @Destination@ parameter must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format /UserName@[SubDomain.]Domain.TopLevelDomain/ ), the entire message will be rejected, even if the message contains other recipients that are valid.
--
--
--
module Network.AWS.SES.SendTemplatedEmail
    (
    -- * Creating a Request
      sendTemplatedEmail
    , SendTemplatedEmail
    -- * Request Lenses
    , steReturnPath
    , steConfigurationSetName
    , steSourceARN
    , steReturnPathARN
    , steTemplateARN
    , steTags
    , steReplyToAddresses
    , steSource
    , steDestination
    , steTemplate
    , steTemplateData

    -- * Destructuring the Response
    , sendTemplatedEmailResponse
    , SendTemplatedEmailResponse
    -- * Response Lenses
    , stersResponseStatus
    , stersMessageId
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to send a templated email using Amazon SES. For more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'sendTemplatedEmail' smart constructor.
data SendTemplatedEmail = SendTemplatedEmail'
  { _steReturnPath           :: !(Maybe Text)
  , _steConfigurationSetName :: !(Maybe Text)
  , _steSourceARN            :: !(Maybe Text)
  , _steReturnPathARN        :: !(Maybe Text)
  , _steTemplateARN          :: !(Maybe Text)
  , _steTags                 :: !(Maybe [MessageTag])
  , _steReplyToAddresses     :: !(Maybe [Text])
  , _steSource               :: !Text
  , _steDestination          :: !Destination
  , _steTemplate             :: !Text
  , _steTemplateData         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendTemplatedEmail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'steReturnPath' - The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
--
-- * 'steConfigurationSetName' - The name of the configuration set to use when you send an email using @SendTemplatedEmail@ .
--
-- * 'steSourceARN' - This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter. For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ . For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- * 'steReturnPathARN' - This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter. For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ . For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- * 'steTemplateARN' - The ARN of the template to use when sending this email.
--
-- * 'steTags' - A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendTemplatedEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
--
-- * 'steReplyToAddresses' - The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
--
-- * 'steSource' - The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> . If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- * 'steDestination' - The destination for this email, composed of To:, CC:, and BCC: fields. A Destination can include up to 50 recipients across these three fields.
--
-- * 'steTemplate' - The template to use when sending this email.
--
-- * 'steTemplateData' - A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
sendTemplatedEmail
    :: Text -- ^ 'steSource'
    -> Destination -- ^ 'steDestination'
    -> Text -- ^ 'steTemplate'
    -> Text -- ^ 'steTemplateData'
    -> SendTemplatedEmail
sendTemplatedEmail pSource_ pDestination_ pTemplate_ pTemplateData_ =
  SendTemplatedEmail'
    { _steReturnPath = Nothing
    , _steConfigurationSetName = Nothing
    , _steSourceARN = Nothing
    , _steReturnPathARN = Nothing
    , _steTemplateARN = Nothing
    , _steTags = Nothing
    , _steReplyToAddresses = Nothing
    , _steSource = pSource_
    , _steDestination = pDestination_
    , _steTemplate = pTemplate_
    , _steTemplateData = pTemplateData_
    }


-- | The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
steReturnPath :: Lens' SendTemplatedEmail (Maybe Text)
steReturnPath = lens _steReturnPath (\ s a -> s{_steReturnPath = a})

-- | The name of the configuration set to use when you send an email using @SendTemplatedEmail@ .
steConfigurationSetName :: Lens' SendTemplatedEmail (Maybe Text)
steConfigurationSetName = lens _steConfigurationSetName (\ s a -> s{_steConfigurationSetName = a})

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter. For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ . For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
steSourceARN :: Lens' SendTemplatedEmail (Maybe Text)
steSourceARN = lens _steSourceARN (\ s a -> s{_steSourceARN = a})

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter. For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ . For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
steReturnPathARN :: Lens' SendTemplatedEmail (Maybe Text)
steReturnPathARN = lens _steReturnPathARN (\ s a -> s{_steReturnPathARN = a})

-- | The ARN of the template to use when sending this email.
steTemplateARN :: Lens' SendTemplatedEmail (Maybe Text)
steTemplateARN = lens _steTemplateARN (\ s a -> s{_steTemplateARN = a})

-- | A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendTemplatedEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
steTags :: Lens' SendTemplatedEmail [MessageTag]
steTags = lens _steTags (\ s a -> s{_steTags = a}) . _Default . _Coerce

-- | The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
steReplyToAddresses :: Lens' SendTemplatedEmail [Text]
steReplyToAddresses = lens _steReplyToAddresses (\ s a -> s{_steReplyToAddresses = a}) . _Default . _Coerce

-- | The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> . If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
steSource :: Lens' SendTemplatedEmail Text
steSource = lens _steSource (\ s a -> s{_steSource = a})

-- | The destination for this email, composed of To:, CC:, and BCC: fields. A Destination can include up to 50 recipients across these three fields.
steDestination :: Lens' SendTemplatedEmail Destination
steDestination = lens _steDestination (\ s a -> s{_steDestination = a})

-- | The template to use when sending this email.
steTemplate :: Lens' SendTemplatedEmail Text
steTemplate = lens _steTemplate (\ s a -> s{_steTemplate = a})

-- | A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
steTemplateData :: Lens' SendTemplatedEmail Text
steTemplateData = lens _steTemplateData (\ s a -> s{_steTemplateData = a})

instance AWSRequest SendTemplatedEmail where
        type Rs SendTemplatedEmail =
             SendTemplatedEmailResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "SendTemplatedEmailResult"
              (\ s h x ->
                 SendTemplatedEmailResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "MessageId"))

instance Hashable SendTemplatedEmail where

instance NFData SendTemplatedEmail where

instance ToHeaders SendTemplatedEmail where
        toHeaders = const mempty

instance ToPath SendTemplatedEmail where
        toPath = const "/"

instance ToQuery SendTemplatedEmail where
        toQuery SendTemplatedEmail'{..}
          = mconcat
              ["Action" =: ("SendTemplatedEmail" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ReturnPath" =: _steReturnPath,
               "ConfigurationSetName" =: _steConfigurationSetName,
               "SourceArn" =: _steSourceARN,
               "ReturnPathArn" =: _steReturnPathARN,
               "TemplateArn" =: _steTemplateARN,
               "Tags" =:
                 toQuery (toQueryList "member" <$> _steTags),
               "ReplyToAddresses" =:
                 toQuery
                   (toQueryList "member" <$> _steReplyToAddresses),
               "Source" =: _steSource,
               "Destination" =: _steDestination,
               "Template" =: _steTemplate,
               "TemplateData" =: _steTemplateData]

-- | /See:/ 'sendTemplatedEmailResponse' smart constructor.
data SendTemplatedEmailResponse = SendTemplatedEmailResponse'
  { _stersResponseStatus :: !Int
  , _stersMessageId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendTemplatedEmailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stersResponseStatus' - -- | The response status code.
--
-- * 'stersMessageId' - The unique message identifier returned from the @SendTemplatedEmail@ action.
sendTemplatedEmailResponse
    :: Int -- ^ 'stersResponseStatus'
    -> Text -- ^ 'stersMessageId'
    -> SendTemplatedEmailResponse
sendTemplatedEmailResponse pResponseStatus_ pMessageId_ =
  SendTemplatedEmailResponse'
    {_stersResponseStatus = pResponseStatus_, _stersMessageId = pMessageId_}


-- | -- | The response status code.
stersResponseStatus :: Lens' SendTemplatedEmailResponse Int
stersResponseStatus = lens _stersResponseStatus (\ s a -> s{_stersResponseStatus = a})

-- | The unique message identifier returned from the @SendTemplatedEmail@ action.
stersMessageId :: Lens' SendTemplatedEmailResponse Text
stersMessageId = lens _stersMessageId (\ s a -> s{_stersMessageId = a})

instance NFData SendTemplatedEmailResponse where
