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
-- Module      : Network.AWS.SES.SendBulkTemplatedEmail
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Composes an email message to multiple destinations. The message body is created using an email template.
--
--
-- In order to send email using the @SendBulkTemplatedEmail@ operation, your call to the API must meet the following requirements:
--
--     * The call must refer to an existing email template. You can create email templates using the 'CreateTemplate' operation.
--
--     * The message must be sent from a verified email address or domain.
--
--     * If your account is still in the Amazon SES sandbox, you may only send to verified addresses or domains, or to email addresses associated with the Amazon SES Mailbox Simulator. For more information, see <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Verifying Email Addresses and Domains> in the /Amazon SES Developer Guide./
--
--     * The total size of the message, including attachments, must be less than 10 MB.
--
--     * Each @Destination@ parameter must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format /UserName@[SubDomain.]Domain.TopLevelDomain/ ), the entire message will be rejected, even if the message contains other recipients that are valid.
--
--
--
module Network.AWS.SES.SendBulkTemplatedEmail
    (
    -- * Creating a Request
      sendBulkTemplatedEmail
    , SendBulkTemplatedEmail
    -- * Request Lenses
    , sbteReturnPath
    , sbteConfigurationSetName
    , sbteSourceARN
    , sbteDefaultTags
    , sbteReturnPathARN
    , sbteTemplateARN
    , sbteDefaultTemplateData
    , sbteReplyToAddresses
    , sbteSource
    , sbteTemplate
    , sbteDestinations

    -- * Destructuring the Response
    , sendBulkTemplatedEmailResponse
    , SendBulkTemplatedEmailResponse
    -- * Response Lenses
    , sbtersResponseStatus
    , sbtersStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to send a templated email to multiple destinations using Amazon SES. For more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'sendBulkTemplatedEmail' smart constructor.
data SendBulkTemplatedEmail = SendBulkTemplatedEmail'
  { _sbteReturnPath           :: !(Maybe Text)
  , _sbteConfigurationSetName :: !(Maybe Text)
  , _sbteSourceARN            :: !(Maybe Text)
  , _sbteDefaultTags          :: !(Maybe [MessageTag])
  , _sbteReturnPathARN        :: !(Maybe Text)
  , _sbteTemplateARN          :: !(Maybe Text)
  , _sbteDefaultTemplateData  :: !(Maybe Text)
  , _sbteReplyToAddresses     :: !(Maybe [Text])
  , _sbteSource               :: !Text
  , _sbteTemplate             :: !Text
  , _sbteDestinations         :: ![BulkEmailDestination]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendBulkTemplatedEmail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbteReturnPath' - The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
--
-- * 'sbteConfigurationSetName' - The name of the configuration set to use when you send an email using @SendBulkTemplatedEmail@ .
--
-- * 'sbteSourceARN' - This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter. For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ . For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- * 'sbteDefaultTags' - A list of tags, in the form of name/value pairs, to apply to an email that you send to a destination using @SendBulkTemplatedEmail@ .
--
-- * 'sbteReturnPathARN' - This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter. For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ . For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- * 'sbteTemplateARN' - The ARN of the template to use when sending this email.
--
-- * 'sbteDefaultTemplateData' - A list of replacement values to apply to the template when replacement data is not specified in a Destination object. These values act as a default or fallback option when no other data is available. The template data is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
--
-- * 'sbteReplyToAddresses' - The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
--
-- * 'sbteSource' - The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> . If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- * 'sbteTemplate' - The template to use when sending this email.
--
-- * 'sbteDestinations' - One or more @Destination@ objects. All of the recipients in a @Destination@ will receive the same version of the email. You can specify up to 50 @Destination@ objects within a @Destinations@ array.
sendBulkTemplatedEmail
    :: Text -- ^ 'sbteSource'
    -> Text -- ^ 'sbteTemplate'
    -> SendBulkTemplatedEmail
sendBulkTemplatedEmail pSource_ pTemplate_ =
  SendBulkTemplatedEmail'
    { _sbteReturnPath = Nothing
    , _sbteConfigurationSetName = Nothing
    , _sbteSourceARN = Nothing
    , _sbteDefaultTags = Nothing
    , _sbteReturnPathARN = Nothing
    , _sbteTemplateARN = Nothing
    , _sbteDefaultTemplateData = Nothing
    , _sbteReplyToAddresses = Nothing
    , _sbteSource = pSource_
    , _sbteTemplate = pTemplate_
    , _sbteDestinations = mempty
    }


-- | The email address that bounces and complaints will be forwarded to when feedback forwarding is enabled. If the message cannot be delivered to the recipient, then an error message will be returned from the recipient's ISP; this message will then be forwarded to the email address specified by the @ReturnPath@ parameter. The @ReturnPath@ parameter is never overwritten. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
sbteReturnPath :: Lens' SendBulkTemplatedEmail (Maybe Text)
sbteReturnPath = lens _sbteReturnPath (\ s a -> s{_sbteReturnPath = a})

-- | The name of the configuration set to use when you send an email using @SendBulkTemplatedEmail@ .
sbteConfigurationSetName :: Lens' SendBulkTemplatedEmail (Maybe Text)
sbteConfigurationSetName = lens _sbteConfigurationSetName (\ s a -> s{_sbteConfigurationSetName = a})

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to send for the email address specified in the @Source@ parameter. For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to send from @user@example.com@ , then you would specify the @SourceArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @Source@ to be @user@example.com@ . For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
sbteSourceARN :: Lens' SendBulkTemplatedEmail (Maybe Text)
sbteSourceARN = lens _sbteSourceARN (\ s a -> s{_sbteSourceARN = a})

-- | A list of tags, in the form of name/value pairs, to apply to an email that you send to a destination using @SendBulkTemplatedEmail@ .
sbteDefaultTags :: Lens' SendBulkTemplatedEmail [MessageTag]
sbteDefaultTags = lens _sbteDefaultTags (\ s a -> s{_sbteDefaultTags = a}) . _Default . _Coerce

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to use the email address specified in the @ReturnPath@ parameter. For example, if the owner of @example.com@ (which has ARN @arn:aws:ses:us-east-1:123456789012:identity/example.com@ ) attaches a policy to it that authorizes you to use @feedback@example.com@ , then you would specify the @ReturnPathArn@ to be @arn:aws:ses:us-east-1:123456789012:identity/example.com@ , and the @ReturnPath@ to be @feedback@example.com@ . For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
sbteReturnPathARN :: Lens' SendBulkTemplatedEmail (Maybe Text)
sbteReturnPathARN = lens _sbteReturnPathARN (\ s a -> s{_sbteReturnPathARN = a})

-- | The ARN of the template to use when sending this email.
sbteTemplateARN :: Lens' SendBulkTemplatedEmail (Maybe Text)
sbteTemplateARN = lens _sbteTemplateARN (\ s a -> s{_sbteTemplateARN = a})

-- | A list of replacement values to apply to the template when replacement data is not specified in a Destination object. These values act as a default or fallback option when no other data is available. The template data is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
sbteDefaultTemplateData :: Lens' SendBulkTemplatedEmail (Maybe Text)
sbteDefaultTemplateData = lens _sbteDefaultTemplateData (\ s a -> s{_sbteDefaultTemplateData = a})

-- | The reply-to email address(es) for the message. If the recipient replies to the message, each reply-to address will receive the reply.
sbteReplyToAddresses :: Lens' SendBulkTemplatedEmail [Text]
sbteReplyToAddresses = lens _sbteReplyToAddresses (\ s a -> s{_sbteReplyToAddresses = a}) . _Default . _Coerce

-- | The email address that is sending the email. This email address must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES. For information about verifying identities, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> . If you are sending on behalf of another user and have been permitted to do so by a sending authorization policy, then you must also specify the @SourceArn@ parameter. For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
sbteSource :: Lens' SendBulkTemplatedEmail Text
sbteSource = lens _sbteSource (\ s a -> s{_sbteSource = a})

-- | The template to use when sending this email.
sbteTemplate :: Lens' SendBulkTemplatedEmail Text
sbteTemplate = lens _sbteTemplate (\ s a -> s{_sbteTemplate = a})

-- | One or more @Destination@ objects. All of the recipients in a @Destination@ will receive the same version of the email. You can specify up to 50 @Destination@ objects within a @Destinations@ array.
sbteDestinations :: Lens' SendBulkTemplatedEmail [BulkEmailDestination]
sbteDestinations = lens _sbteDestinations (\ s a -> s{_sbteDestinations = a}) . _Coerce

instance AWSRequest SendBulkTemplatedEmail where
        type Rs SendBulkTemplatedEmail =
             SendBulkTemplatedEmailResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "SendBulkTemplatedEmailResult"
              (\ s h x ->
                 SendBulkTemplatedEmailResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "Status" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable SendBulkTemplatedEmail where

instance NFData SendBulkTemplatedEmail where

instance ToHeaders SendBulkTemplatedEmail where
        toHeaders = const mempty

instance ToPath SendBulkTemplatedEmail where
        toPath = const "/"

instance ToQuery SendBulkTemplatedEmail where
        toQuery SendBulkTemplatedEmail'{..}
          = mconcat
              ["Action" =:
                 ("SendBulkTemplatedEmail" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ReturnPath" =: _sbteReturnPath,
               "ConfigurationSetName" =: _sbteConfigurationSetName,
               "SourceArn" =: _sbteSourceARN,
               "DefaultTags" =:
                 toQuery (toQueryList "member" <$> _sbteDefaultTags),
               "ReturnPathArn" =: _sbteReturnPathARN,
               "TemplateArn" =: _sbteTemplateARN,
               "DefaultTemplateData" =: _sbteDefaultTemplateData,
               "ReplyToAddresses" =:
                 toQuery
                   (toQueryList "member" <$> _sbteReplyToAddresses),
               "Source" =: _sbteSource, "Template" =: _sbteTemplate,
               "Destinations" =:
                 toQueryList "member" _sbteDestinations]

-- | /See:/ 'sendBulkTemplatedEmailResponse' smart constructor.
data SendBulkTemplatedEmailResponse = SendBulkTemplatedEmailResponse'
  { _sbtersResponseStatus :: !Int
  , _sbtersStatus         :: ![BulkEmailDestinationStatus]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendBulkTemplatedEmailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbtersResponseStatus' - -- | The response status code.
--
-- * 'sbtersStatus' - The unique message identifier returned from the @SendBulkTemplatedEmail@ action.
sendBulkTemplatedEmailResponse
    :: Int -- ^ 'sbtersResponseStatus'
    -> SendBulkTemplatedEmailResponse
sendBulkTemplatedEmailResponse pResponseStatus_ =
  SendBulkTemplatedEmailResponse'
    {_sbtersResponseStatus = pResponseStatus_, _sbtersStatus = mempty}


-- | -- | The response status code.
sbtersResponseStatus :: Lens' SendBulkTemplatedEmailResponse Int
sbtersResponseStatus = lens _sbtersResponseStatus (\ s a -> s{_sbtersResponseStatus = a})

-- | The unique message identifier returned from the @SendBulkTemplatedEmail@ action.
sbtersStatus :: Lens' SendBulkTemplatedEmailResponse [BulkEmailDestinationStatus]
sbtersStatus = lens _sbtersStatus (\ s a -> s{_sbtersStatus = a}) . _Coerce

instance NFData SendBulkTemplatedEmailResponse where
