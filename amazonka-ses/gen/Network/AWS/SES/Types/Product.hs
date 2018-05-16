{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.Sum

-- | When included in a receipt rule, this action adds a header to the received email.
--
--
-- For information about adding a header using a receipt rule, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-add-header.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'addHeaderAction' smart constructor.
data AddHeaderAction = AddHeaderAction'
  { _ahaHeaderName  :: !Text
  , _ahaHeaderValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddHeaderAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ahaHeaderName' - The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
--
-- * 'ahaHeaderValue' - Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
addHeaderAction
    :: Text -- ^ 'ahaHeaderName'
    -> Text -- ^ 'ahaHeaderValue'
    -> AddHeaderAction
addHeaderAction pHeaderName_ pHeaderValue_ =
  AddHeaderAction'
    {_ahaHeaderName = pHeaderName_, _ahaHeaderValue = pHeaderValue_}


-- | The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
ahaHeaderName :: Lens' AddHeaderAction Text
ahaHeaderName = lens _ahaHeaderName (\ s a -> s{_ahaHeaderName = a})

-- | Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
ahaHeaderValue :: Lens' AddHeaderAction Text
ahaHeaderValue = lens _ahaHeaderValue (\ s a -> s{_ahaHeaderValue = a})

instance FromXML AddHeaderAction where
        parseXML x
          = AddHeaderAction' <$>
              (x .@ "HeaderName") <*> (x .@ "HeaderValue")

instance Hashable AddHeaderAction where

instance NFData AddHeaderAction where

instance ToQuery AddHeaderAction where
        toQuery AddHeaderAction'{..}
          = mconcat
              ["HeaderName" =: _ahaHeaderName,
               "HeaderValue" =: _ahaHeaderValue]

-- | Represents the body of the message. You can specify text, HTML, or both. If you use both, then the message should display correctly in the widest variety of email clients.
--
--
--
-- /See:/ 'body' smart constructor.
data Body = Body'
  { _bText :: !(Maybe Content)
  , _bHTML :: !(Maybe Content)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Body' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bText' - The content of the message, in text format. Use this for text-based email clients, or clients on high-latency networks (such as mobile devices).
--
-- * 'bHTML' - The content of the message, in HTML format. Use this for email clients that can process HTML. You can include clickable links, formatted text, and much more in an HTML message.
body
    :: Body
body = Body' {_bText = Nothing, _bHTML = Nothing}


-- | The content of the message, in text format. Use this for text-based email clients, or clients on high-latency networks (such as mobile devices).
bText :: Lens' Body (Maybe Content)
bText = lens _bText (\ s a -> s{_bText = a})

-- | The content of the message, in HTML format. Use this for email clients that can process HTML. You can include clickable links, formatted text, and much more in an HTML message.
bHTML :: Lens' Body (Maybe Content)
bHTML = lens _bHTML (\ s a -> s{_bHTML = a})

instance Hashable Body where

instance NFData Body where

instance ToQuery Body where
        toQuery Body'{..}
          = mconcat ["Text" =: _bText, "Html" =: _bHTML]

-- | When included in a receipt rule, this action rejects the received email by returning a bounce response to the sender and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
--
-- For information about sending a bounce message in response to a received email, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-bounce.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'bounceAction' smart constructor.
data BounceAction = BounceAction'
  { _baTopicARN      :: !(Maybe Text)
  , _baStatusCode    :: !(Maybe Text)
  , _baSmtpReplyCode :: !Text
  , _baMessage       :: !Text
  , _baSender        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BounceAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the bounce action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <http://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- * 'baStatusCode' - The SMTP enhanced status code, as defined by <https://tools.ietf.org/html/rfc3463 RFC 3463> .
--
-- * 'baSmtpReplyCode' - The SMTP reply code, as defined by <https://tools.ietf.org/html/rfc5321 RFC 5321> .
--
-- * 'baMessage' - Human-readable text to include in the bounce message.
--
-- * 'baSender' - The email address of the sender of the bounced email. This is the address from which the bounce message will be sent.
bounceAction
    :: Text -- ^ 'baSmtpReplyCode'
    -> Text -- ^ 'baMessage'
    -> Text -- ^ 'baSender'
    -> BounceAction
bounceAction pSmtpReplyCode_ pMessage_ pSender_ =
  BounceAction'
    { _baTopicARN = Nothing
    , _baStatusCode = Nothing
    , _baSmtpReplyCode = pSmtpReplyCode_
    , _baMessage = pMessage_
    , _baSender = pSender_
    }


-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the bounce action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <http://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
baTopicARN :: Lens' BounceAction (Maybe Text)
baTopicARN = lens _baTopicARN (\ s a -> s{_baTopicARN = a})

-- | The SMTP enhanced status code, as defined by <https://tools.ietf.org/html/rfc3463 RFC 3463> .
baStatusCode :: Lens' BounceAction (Maybe Text)
baStatusCode = lens _baStatusCode (\ s a -> s{_baStatusCode = a})

-- | The SMTP reply code, as defined by <https://tools.ietf.org/html/rfc5321 RFC 5321> .
baSmtpReplyCode :: Lens' BounceAction Text
baSmtpReplyCode = lens _baSmtpReplyCode (\ s a -> s{_baSmtpReplyCode = a})

-- | Human-readable text to include in the bounce message.
baMessage :: Lens' BounceAction Text
baMessage = lens _baMessage (\ s a -> s{_baMessage = a})

-- | The email address of the sender of the bounced email. This is the address from which the bounce message will be sent.
baSender :: Lens' BounceAction Text
baSender = lens _baSender (\ s a -> s{_baSender = a})

instance FromXML BounceAction where
        parseXML x
          = BounceAction' <$>
              (x .@? "TopicArn") <*> (x .@? "StatusCode") <*>
                (x .@ "SmtpReplyCode")
                <*> (x .@ "Message")
                <*> (x .@ "Sender")

instance Hashable BounceAction where

instance NFData BounceAction where

instance ToQuery BounceAction where
        toQuery BounceAction'{..}
          = mconcat
              ["TopicArn" =: _baTopicARN,
               "StatusCode" =: _baStatusCode,
               "SmtpReplyCode" =: _baSmtpReplyCode,
               "Message" =: _baMessage, "Sender" =: _baSender]

-- | Recipient-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.
--
--
-- For information about receiving email through Amazon SES, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'bouncedRecipientInfo' smart constructor.
data BouncedRecipientInfo = BouncedRecipientInfo'
  { _briBounceType         :: !(Maybe BounceType)
  , _briRecipientDsnFields :: !(Maybe RecipientDsnFields)
  , _briRecipientARN       :: !(Maybe Text)
  , _briRecipient          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BouncedRecipientInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'briBounceType' - The reason for the bounce. You must provide either this parameter or @RecipientDsnFields@ .
--
-- * 'briRecipientDsnFields' - Recipient-related DSN fields, most of which would normally be filled in automatically when provided with a @BounceType@ . You must provide either this parameter or @BounceType@ .
--
-- * 'briRecipientARN' - This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to receive email for the recipient of the bounced email. For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- * 'briRecipient' - The email address of the recipient of the bounced email.
bouncedRecipientInfo
    :: Text -- ^ 'briRecipient'
    -> BouncedRecipientInfo
bouncedRecipientInfo pRecipient_ =
  BouncedRecipientInfo'
    { _briBounceType = Nothing
    , _briRecipientDsnFields = Nothing
    , _briRecipientARN = Nothing
    , _briRecipient = pRecipient_
    }


-- | The reason for the bounce. You must provide either this parameter or @RecipientDsnFields@ .
briBounceType :: Lens' BouncedRecipientInfo (Maybe BounceType)
briBounceType = lens _briBounceType (\ s a -> s{_briBounceType = a})

-- | Recipient-related DSN fields, most of which would normally be filled in automatically when provided with a @BounceType@ . You must provide either this parameter or @BounceType@ .
briRecipientDsnFields :: Lens' BouncedRecipientInfo (Maybe RecipientDsnFields)
briRecipientDsnFields = lens _briRecipientDsnFields (\ s a -> s{_briRecipientDsnFields = a})

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to receive email for the recipient of the bounced email. For more information about sending authorization, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
briRecipientARN :: Lens' BouncedRecipientInfo (Maybe Text)
briRecipientARN = lens _briRecipientARN (\ s a -> s{_briRecipientARN = a})

-- | The email address of the recipient of the bounced email.
briRecipient :: Lens' BouncedRecipientInfo Text
briRecipient = lens _briRecipient (\ s a -> s{_briRecipient = a})

instance Hashable BouncedRecipientInfo where

instance NFData BouncedRecipientInfo where

instance ToQuery BouncedRecipientInfo where
        toQuery BouncedRecipientInfo'{..}
          = mconcat
              ["BounceType" =: _briBounceType,
               "RecipientDsnFields" =: _briRecipientDsnFields,
               "RecipientArn" =: _briRecipientARN,
               "Recipient" =: _briRecipient]

-- | An array that contains one or more Destinations, as well as the tags and replacement data associated with each of those Destinations.
--
--
--
-- /See:/ 'bulkEmailDestination' smart constructor.
data BulkEmailDestination = BulkEmailDestination'
  { _bedReplacementTemplateData :: !(Maybe Text)
  , _bedReplacementTags         :: !(Maybe [MessageTag])
  , _bedDestination             :: !Destination
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BulkEmailDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bedReplacementTemplateData' - A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
--
-- * 'bedReplacementTags' - A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendBulkTemplatedEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
--
-- * 'bedDestination' - Undocumented member.
bulkEmailDestination
    :: Destination -- ^ 'bedDestination'
    -> BulkEmailDestination
bulkEmailDestination pDestination_ =
  BulkEmailDestination'
    { _bedReplacementTemplateData = Nothing
    , _bedReplacementTags = Nothing
    , _bedDestination = pDestination_
    }


-- | A list of replacement values to apply to the template. This parameter is a JSON object, typically consisting of key-value pairs in which the keys correspond to replacement tags in the email template.
bedReplacementTemplateData :: Lens' BulkEmailDestination (Maybe Text)
bedReplacementTemplateData = lens _bedReplacementTemplateData (\ s a -> s{_bedReplacementTemplateData = a})

-- | A list of tags, in the form of name/value pairs, to apply to an email that you send using @SendBulkTemplatedEmail@ . Tags correspond to characteristics of the email that you define, so that you can publish email sending events.
bedReplacementTags :: Lens' BulkEmailDestination [MessageTag]
bedReplacementTags = lens _bedReplacementTags (\ s a -> s{_bedReplacementTags = a}) . _Default . _Coerce

-- | Undocumented member.
bedDestination :: Lens' BulkEmailDestination Destination
bedDestination = lens _bedDestination (\ s a -> s{_bedDestination = a})

instance Hashable BulkEmailDestination where

instance NFData BulkEmailDestination where

instance ToQuery BulkEmailDestination where
        toQuery BulkEmailDestination'{..}
          = mconcat
              ["ReplacementTemplateData" =:
                 _bedReplacementTemplateData,
               "ReplacementTags" =:
                 toQuery
                   (toQueryList "member" <$> _bedReplacementTags),
               "Destination" =: _bedDestination]

-- | An object that contains the response from the @SendBulkTemplatedEmail@ operation.
--
--
--
-- /See:/ 'bulkEmailDestinationStatus' smart constructor.
data BulkEmailDestinationStatus = BulkEmailDestinationStatus'
  { _bedsStatus    :: !(Maybe BulkEmailStatus)
  , _bedsError     :: !(Maybe Text)
  , _bedsMessageId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BulkEmailDestinationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bedsStatus' - The status of a message sent using the @SendBulkTemplatedEmail@ operation. Possible values for this parameter include:     * @Success@ : Amazon SES accepted the message, and will attempt to deliver it to the recipients.     * @MessageRejected@ : The message was rejected because it contained a virus.     * @MailFromDomainNotVerified@ : The sender's email address or domain was not verified.     * @ConfigurationSetDoesNotExist@ : The configuration set you specified does not exist.     * @TemplateDoesNotExist@ : The template you specified does not exist.     * @AccountSuspended@ : Your account has been shut down because of issues related to your email sending practices.     * @AccountThrottled@ : The number of emails you can send has been reduced because your account has exceeded its allocated sending limit.     * @AccountDailyQuotaExceeded@ : You have reached or exceeded the maximum number of emails you can send from your account in a 24-hour period.     * @InvalidSendingPoolName@ : The configuration set you specified refers to an IP pool that does not exist.     * @AccountSendingPaused@ : Email sending for the Amazon SES account was disabled using the 'UpdateAccountSendingEnabled' operation.     * @ConfigurationSetSendingPaused@ : Email sending for this configuration set was disabled using the 'UpdateConfigurationSetSendingEnabled' operation.     * @InvalidParameterValue@ : One or more of the parameters you specified when calling this operation was invalid. See the error message for additional information.     * @TransientFailure@ : Amazon SES was unable to process your request because of a temporary issue.     * @Failed@ : Amazon SES was unable to process your request. See the error message for additional information.
--
-- * 'bedsError' - A description of an error that prevented a message being sent using the @SendBulkTemplatedEmail@ operation.
--
-- * 'bedsMessageId' - The unique message identifier returned from the @SendBulkTemplatedEmail@ operation.
bulkEmailDestinationStatus
    :: BulkEmailDestinationStatus
bulkEmailDestinationStatus =
  BulkEmailDestinationStatus'
    {_bedsStatus = Nothing, _bedsError = Nothing, _bedsMessageId = Nothing}


-- | The status of a message sent using the @SendBulkTemplatedEmail@ operation. Possible values for this parameter include:     * @Success@ : Amazon SES accepted the message, and will attempt to deliver it to the recipients.     * @MessageRejected@ : The message was rejected because it contained a virus.     * @MailFromDomainNotVerified@ : The sender's email address or domain was not verified.     * @ConfigurationSetDoesNotExist@ : The configuration set you specified does not exist.     * @TemplateDoesNotExist@ : The template you specified does not exist.     * @AccountSuspended@ : Your account has been shut down because of issues related to your email sending practices.     * @AccountThrottled@ : The number of emails you can send has been reduced because your account has exceeded its allocated sending limit.     * @AccountDailyQuotaExceeded@ : You have reached or exceeded the maximum number of emails you can send from your account in a 24-hour period.     * @InvalidSendingPoolName@ : The configuration set you specified refers to an IP pool that does not exist.     * @AccountSendingPaused@ : Email sending for the Amazon SES account was disabled using the 'UpdateAccountSendingEnabled' operation.     * @ConfigurationSetSendingPaused@ : Email sending for this configuration set was disabled using the 'UpdateConfigurationSetSendingEnabled' operation.     * @InvalidParameterValue@ : One or more of the parameters you specified when calling this operation was invalid. See the error message for additional information.     * @TransientFailure@ : Amazon SES was unable to process your request because of a temporary issue.     * @Failed@ : Amazon SES was unable to process your request. See the error message for additional information.
bedsStatus :: Lens' BulkEmailDestinationStatus (Maybe BulkEmailStatus)
bedsStatus = lens _bedsStatus (\ s a -> s{_bedsStatus = a})

-- | A description of an error that prevented a message being sent using the @SendBulkTemplatedEmail@ operation.
bedsError :: Lens' BulkEmailDestinationStatus (Maybe Text)
bedsError = lens _bedsError (\ s a -> s{_bedsError = a})

-- | The unique message identifier returned from the @SendBulkTemplatedEmail@ operation.
bedsMessageId :: Lens' BulkEmailDestinationStatus (Maybe Text)
bedsMessageId = lens _bedsMessageId (\ s a -> s{_bedsMessageId = a})

instance FromXML BulkEmailDestinationStatus where
        parseXML x
          = BulkEmailDestinationStatus' <$>
              (x .@? "Status") <*> (x .@? "Error") <*>
                (x .@? "MessageId")

instance Hashable BulkEmailDestinationStatus where

instance NFData BulkEmailDestinationStatus where

-- | Contains information associated with an Amazon CloudWatch event destination to which email sending events are published.
--
--
-- Event destinations, such as Amazon CloudWatch, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'cloudWatchDestination' smart constructor.
newtype CloudWatchDestination = CloudWatchDestination'
  { _cwdDimensionConfigurations :: [CloudWatchDimensionConfiguration]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudWatchDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwdDimensionConfigurations' - A list of dimensions upon which to categorize your emails when you publish email sending events to Amazon CloudWatch.
cloudWatchDestination
    :: CloudWatchDestination
cloudWatchDestination =
  CloudWatchDestination' {_cwdDimensionConfigurations = mempty}


-- | A list of dimensions upon which to categorize your emails when you publish email sending events to Amazon CloudWatch.
cwdDimensionConfigurations :: Lens' CloudWatchDestination [CloudWatchDimensionConfiguration]
cwdDimensionConfigurations = lens _cwdDimensionConfigurations (\ s a -> s{_cwdDimensionConfigurations = a}) . _Coerce

instance FromXML CloudWatchDestination where
        parseXML x
          = CloudWatchDestination' <$>
              (x .@? "DimensionConfigurations" .!@ mempty >>=
                 parseXMLList "member")

instance Hashable CloudWatchDestination where

instance NFData CloudWatchDestination where

instance ToQuery CloudWatchDestination where
        toQuery CloudWatchDestination'{..}
          = mconcat
              ["DimensionConfigurations" =:
                 toQueryList "member" _cwdDimensionConfigurations]

-- | Contains the dimension configuration to use when you publish email sending events to Amazon CloudWatch.
--
--
-- For information about publishing email sending events to Amazon CloudWatch, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'cloudWatchDimensionConfiguration' smart constructor.
data CloudWatchDimensionConfiguration = CloudWatchDimensionConfiguration'
  { _cwdcDimensionName         :: !Text
  , _cwdcDimensionValueSource  :: !DimensionValueSource
  , _cwdcDefaultDimensionValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudWatchDimensionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwdcDimensionName' - The name of an Amazon CloudWatch dimension associated with an email sending metric. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 256 characters.
--
-- * 'cwdcDimensionValueSource' - The place where Amazon SES finds the value of a dimension to publish to Amazon CloudWatch. If you want Amazon SES to use the message tags that you specify using an @X-SES-MESSAGE-TAGS@ header or a parameter to the @SendEmail@ /@SendRawEmail@ API, choose @messageTag@ . If you want Amazon SES to use your own email headers, choose @emailHeader@ .
--
-- * 'cwdcDefaultDimensionValue' - The default value of the dimension that is published to Amazon CloudWatch if you do not provide the value of the dimension when you send an email. The default value must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 256 characters.
cloudWatchDimensionConfiguration
    :: Text -- ^ 'cwdcDimensionName'
    -> DimensionValueSource -- ^ 'cwdcDimensionValueSource'
    -> Text -- ^ 'cwdcDefaultDimensionValue'
    -> CloudWatchDimensionConfiguration
cloudWatchDimensionConfiguration pDimensionName_ pDimensionValueSource_ pDefaultDimensionValue_ =
  CloudWatchDimensionConfiguration'
    { _cwdcDimensionName = pDimensionName_
    , _cwdcDimensionValueSource = pDimensionValueSource_
    , _cwdcDefaultDimensionValue = pDefaultDimensionValue_
    }


-- | The name of an Amazon CloudWatch dimension associated with an email sending metric. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 256 characters.
cwdcDimensionName :: Lens' CloudWatchDimensionConfiguration Text
cwdcDimensionName = lens _cwdcDimensionName (\ s a -> s{_cwdcDimensionName = a})

-- | The place where Amazon SES finds the value of a dimension to publish to Amazon CloudWatch. If you want Amazon SES to use the message tags that you specify using an @X-SES-MESSAGE-TAGS@ header or a parameter to the @SendEmail@ /@SendRawEmail@ API, choose @messageTag@ . If you want Amazon SES to use your own email headers, choose @emailHeader@ .
cwdcDimensionValueSource :: Lens' CloudWatchDimensionConfiguration DimensionValueSource
cwdcDimensionValueSource = lens _cwdcDimensionValueSource (\ s a -> s{_cwdcDimensionValueSource = a})

-- | The default value of the dimension that is published to Amazon CloudWatch if you do not provide the value of the dimension when you send an email. The default value must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 256 characters.
cwdcDefaultDimensionValue :: Lens' CloudWatchDimensionConfiguration Text
cwdcDefaultDimensionValue = lens _cwdcDefaultDimensionValue (\ s a -> s{_cwdcDefaultDimensionValue = a})

instance FromXML CloudWatchDimensionConfiguration
         where
        parseXML x
          = CloudWatchDimensionConfiguration' <$>
              (x .@ "DimensionName") <*>
                (x .@ "DimensionValueSource")
                <*> (x .@ "DefaultDimensionValue")

instance Hashable CloudWatchDimensionConfiguration
         where

instance NFData CloudWatchDimensionConfiguration
         where

instance ToQuery CloudWatchDimensionConfiguration
         where
        toQuery CloudWatchDimensionConfiguration'{..}
          = mconcat
              ["DimensionName" =: _cwdcDimensionName,
               "DimensionValueSource" =: _cwdcDimensionValueSource,
               "DefaultDimensionValue" =:
                 _cwdcDefaultDimensionValue]

-- | The name of the configuration set.
--
--
-- Configuration sets let you create groups of rules that you can apply to the emails you send using Amazon SES. For more information about using configuration sets, see <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/using-configuration-sets.html Using Amazon SES Configuration Sets> in the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/ Amazon SES Developer Guide> .
--
--
-- /See:/ 'configurationSet' smart constructor.
newtype ConfigurationSet = ConfigurationSet'
  { _csName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfigurationSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csName' - The name of the configuration set. The name must meet the following requirements:     * Contain only letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain 64 characters or fewer.
configurationSet
    :: Text -- ^ 'csName'
    -> ConfigurationSet
configurationSet pName_ = ConfigurationSet' {_csName = pName_}


-- | The name of the configuration set. The name must meet the following requirements:     * Contain only letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain 64 characters or fewer.
csName :: Lens' ConfigurationSet Text
csName = lens _csName (\ s a -> s{_csName = a})

instance FromXML ConfigurationSet where
        parseXML x = ConfigurationSet' <$> (x .@ "Name")

instance Hashable ConfigurationSet where

instance NFData ConfigurationSet where

instance ToQuery ConfigurationSet where
        toQuery ConfigurationSet'{..}
          = mconcat ["Name" =: _csName]

-- | Represents textual data, plus an optional character set specification.
--
--
-- By default, the text must be 7-bit ASCII, due to the constraints of the SMTP protocol. If the text must contain any other characters, then you must also specify a character set. Examples include UTF-8, ISO-8859-1, and Shift_JIS.
--
--
-- /See:/ 'content' smart constructor.
data Content = Content'
  { _cCharset :: !(Maybe Text)
  , _cData    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Content' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCharset' - The character set of the content.
--
-- * 'cData' - The textual data of the content.
content
    :: Text -- ^ 'cData'
    -> Content
content pData_ = Content' {_cCharset = Nothing, _cData = pData_}


-- | The character set of the content.
cCharset :: Lens' Content (Maybe Text)
cCharset = lens _cCharset (\ s a -> s{_cCharset = a})

-- | The textual data of the content.
cData :: Lens' Content Text
cData = lens _cData (\ s a -> s{_cData = a})

instance Hashable Content where

instance NFData Content where

instance ToQuery Content where
        toQuery Content'{..}
          = mconcat ["Charset" =: _cCharset, "Data" =: _cData]

-- | Contains information about a custom verification email template.
--
--
--
-- /See:/ 'customVerificationEmailTemplate' smart constructor.
data CustomVerificationEmailTemplate = CustomVerificationEmailTemplate'
  { _cvetFromEmailAddress      :: !(Maybe Text)
  , _cvetTemplateName          :: !(Maybe Text)
  , _cvetFailureRedirectionURL :: !(Maybe Text)
  , _cvetTemplateSubject       :: !(Maybe Text)
  , _cvetSuccessRedirectionURL :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CustomVerificationEmailTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvetFromEmailAddress' - The email address that the custom verification email is sent from.
--
-- * 'cvetTemplateName' - The name of the custom verification email template.
--
-- * 'cvetFailureRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
--
-- * 'cvetTemplateSubject' - The subject line of the custom verification email.
--
-- * 'cvetSuccessRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
customVerificationEmailTemplate
    :: CustomVerificationEmailTemplate
customVerificationEmailTemplate =
  CustomVerificationEmailTemplate'
    { _cvetFromEmailAddress = Nothing
    , _cvetTemplateName = Nothing
    , _cvetFailureRedirectionURL = Nothing
    , _cvetTemplateSubject = Nothing
    , _cvetSuccessRedirectionURL = Nothing
    }


-- | The email address that the custom verification email is sent from.
cvetFromEmailAddress :: Lens' CustomVerificationEmailTemplate (Maybe Text)
cvetFromEmailAddress = lens _cvetFromEmailAddress (\ s a -> s{_cvetFromEmailAddress = a})

-- | The name of the custom verification email template.
cvetTemplateName :: Lens' CustomVerificationEmailTemplate (Maybe Text)
cvetTemplateName = lens _cvetTemplateName (\ s a -> s{_cvetTemplateName = a})

-- | The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
cvetFailureRedirectionURL :: Lens' CustomVerificationEmailTemplate (Maybe Text)
cvetFailureRedirectionURL = lens _cvetFailureRedirectionURL (\ s a -> s{_cvetFailureRedirectionURL = a})

-- | The subject line of the custom verification email.
cvetTemplateSubject :: Lens' CustomVerificationEmailTemplate (Maybe Text)
cvetTemplateSubject = lens _cvetTemplateSubject (\ s a -> s{_cvetTemplateSubject = a})

-- | The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
cvetSuccessRedirectionURL :: Lens' CustomVerificationEmailTemplate (Maybe Text)
cvetSuccessRedirectionURL = lens _cvetSuccessRedirectionURL (\ s a -> s{_cvetSuccessRedirectionURL = a})

instance FromXML CustomVerificationEmailTemplate
         where
        parseXML x
          = CustomVerificationEmailTemplate' <$>
              (x .@? "FromEmailAddress") <*> (x .@? "TemplateName")
                <*> (x .@? "FailureRedirectionURL")
                <*> (x .@? "TemplateSubject")
                <*> (x .@? "SuccessRedirectionURL")

instance Hashable CustomVerificationEmailTemplate
         where

instance NFData CustomVerificationEmailTemplate where

-- | Represents the destination of the message, consisting of To:, CC:, and BCC: fields.
--
--
--
-- /See:/ 'destination' smart constructor.
data Destination = Destination'
  { _dBCCAddresses :: !(Maybe [Text])
  , _dCCAddresses  :: !(Maybe [Text])
  , _dToAddresses  :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dBCCAddresses' - The BCC: field(s) of the message.
--
-- * 'dCCAddresses' - The CC: field(s) of the message.
--
-- * 'dToAddresses' - The To: field(s) of the message.
destination
    :: Destination
destination =
  Destination'
    {_dBCCAddresses = Nothing, _dCCAddresses = Nothing, _dToAddresses = Nothing}


-- | The BCC: field(s) of the message.
dBCCAddresses :: Lens' Destination [Text]
dBCCAddresses = lens _dBCCAddresses (\ s a -> s{_dBCCAddresses = a}) . _Default . _Coerce

-- | The CC: field(s) of the message.
dCCAddresses :: Lens' Destination [Text]
dCCAddresses = lens _dCCAddresses (\ s a -> s{_dCCAddresses = a}) . _Default . _Coerce

-- | The To: field(s) of the message.
dToAddresses :: Lens' Destination [Text]
dToAddresses = lens _dToAddresses (\ s a -> s{_dToAddresses = a}) . _Default . _Coerce

instance Hashable Destination where

instance NFData Destination where

instance ToQuery Destination where
        toQuery Destination'{..}
          = mconcat
              ["BccAddresses" =:
                 toQuery (toQueryList "member" <$> _dBCCAddresses),
               "CcAddresses" =:
                 toQuery (toQueryList "member" <$> _dCCAddresses),
               "ToAddresses" =:
                 toQuery (toQueryList "member" <$> _dToAddresses)]

-- | Contains information about the event destination that the specified email sending events will be published to.
--
--
-- Event destinations are associated with configuration sets, which enable you to publish email sending events to Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS). For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'eventDestination' smart constructor.
data EventDestination = EventDestination'
  { _edEnabled                    :: !(Maybe Bool)
  , _edKinesisFirehoseDestination :: !(Maybe KinesisFirehoseDestination)
  , _edCloudWatchDestination      :: !(Maybe CloudWatchDestination)
  , _edSNSDestination             :: !(Maybe SNSDestination)
  , _edName                       :: !Text
  , _edMatchingEventTypes         :: ![EventType]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edEnabled' - Sets whether Amazon SES publishes events to this destination when you send an email with the associated configuration set. Set to @true@ to enable publishing to this destination; set to @false@ to prevent publishing to this destination. The default value is @false@ .
--
-- * 'edKinesisFirehoseDestination' - An object that contains the delivery stream ARN and the IAM role ARN associated with an Amazon Kinesis Firehose event destination.
--
-- * 'edCloudWatchDestination' - An object that contains the names, default values, and sources of the dimensions associated with an Amazon CloudWatch event destination.
--
-- * 'edSNSDestination' - An object that contains the topic ARN associated with an Amazon Simple Notification Service (Amazon SNS) event destination.
--
-- * 'edName' - The name of the event destination. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 64 characters.
--
-- * 'edMatchingEventTypes' - The type of email sending events to publish to the event destination.
eventDestination
    :: Text -- ^ 'edName'
    -> EventDestination
eventDestination pName_ =
  EventDestination'
    { _edEnabled = Nothing
    , _edKinesisFirehoseDestination = Nothing
    , _edCloudWatchDestination = Nothing
    , _edSNSDestination = Nothing
    , _edName = pName_
    , _edMatchingEventTypes = mempty
    }


-- | Sets whether Amazon SES publishes events to this destination when you send an email with the associated configuration set. Set to @true@ to enable publishing to this destination; set to @false@ to prevent publishing to this destination. The default value is @false@ .
edEnabled :: Lens' EventDestination (Maybe Bool)
edEnabled = lens _edEnabled (\ s a -> s{_edEnabled = a})

-- | An object that contains the delivery stream ARN and the IAM role ARN associated with an Amazon Kinesis Firehose event destination.
edKinesisFirehoseDestination :: Lens' EventDestination (Maybe KinesisFirehoseDestination)
edKinesisFirehoseDestination = lens _edKinesisFirehoseDestination (\ s a -> s{_edKinesisFirehoseDestination = a})

-- | An object that contains the names, default values, and sources of the dimensions associated with an Amazon CloudWatch event destination.
edCloudWatchDestination :: Lens' EventDestination (Maybe CloudWatchDestination)
edCloudWatchDestination = lens _edCloudWatchDestination (\ s a -> s{_edCloudWatchDestination = a})

-- | An object that contains the topic ARN associated with an Amazon Simple Notification Service (Amazon SNS) event destination.
edSNSDestination :: Lens' EventDestination (Maybe SNSDestination)
edSNSDestination = lens _edSNSDestination (\ s a -> s{_edSNSDestination = a})

-- | The name of the event destination. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 64 characters.
edName :: Lens' EventDestination Text
edName = lens _edName (\ s a -> s{_edName = a})

-- | The type of email sending events to publish to the event destination.
edMatchingEventTypes :: Lens' EventDestination [EventType]
edMatchingEventTypes = lens _edMatchingEventTypes (\ s a -> s{_edMatchingEventTypes = a}) . _Coerce

instance FromXML EventDestination where
        parseXML x
          = EventDestination' <$>
              (x .@? "Enabled") <*>
                (x .@? "KinesisFirehoseDestination")
                <*> (x .@? "CloudWatchDestination")
                <*> (x .@? "SNSDestination")
                <*> (x .@ "Name")
                <*>
                (x .@? "MatchingEventTypes" .!@ mempty >>=
                   parseXMLList "member")

instance Hashable EventDestination where

instance NFData EventDestination where

instance ToQuery EventDestination where
        toQuery EventDestination'{..}
          = mconcat
              ["Enabled" =: _edEnabled,
               "KinesisFirehoseDestination" =:
                 _edKinesisFirehoseDestination,
               "CloudWatchDestination" =: _edCloudWatchDestination,
               "SNSDestination" =: _edSNSDestination,
               "Name" =: _edName,
               "MatchingEventTypes" =:
                 toQueryList "member" _edMatchingEventTypes]

-- | Additional X-headers to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.
--
--
-- For information about receiving email through Amazon SES, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'extensionField' smart constructor.
data ExtensionField = ExtensionField'
  { _efName  :: !Text
  , _efValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExtensionField' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efName' - The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
--
-- * 'efValue' - The value of the header to add. Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
extensionField
    :: Text -- ^ 'efName'
    -> Text -- ^ 'efValue'
    -> ExtensionField
extensionField pName_ pValue_ =
  ExtensionField' {_efName = pName_, _efValue = pValue_}


-- | The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
efName :: Lens' ExtensionField Text
efName = lens _efName (\ s a -> s{_efName = a})

-- | The value of the header to add. Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
efValue :: Lens' ExtensionField Text
efValue = lens _efValue (\ s a -> s{_efValue = a})

instance Hashable ExtensionField where

instance NFData ExtensionField where

instance ToQuery ExtensionField where
        toQuery ExtensionField'{..}
          = mconcat ["Name" =: _efName, "Value" =: _efValue]

-- | Represents the DKIM attributes of a verified email address or a domain.
--
--
--
-- /See:/ 'identityDkimAttributes' smart constructor.
data IdentityDkimAttributes = IdentityDkimAttributes'
  { _idaDkimTokens             :: !(Maybe [Text])
  , _idaDkimEnabled            :: !Bool
  , _idaDkimVerificationStatus :: !VerificationStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IdentityDkimAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idaDkimTokens' - A set of character strings that represent the domain's identity. Using these tokens, you will need to create DNS CNAME records that point to DKIM public keys hosted by Amazon SES. Amazon Web Services will eventually detect that you have updated your DNS records; this detection process may take up to 72 hours. Upon successful detection, Amazon SES will be able to DKIM-sign email originating from that domain. (This only applies to domain identities, not email address identities.) For more information about creating DNS records using DKIM tokens, go to the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html Amazon SES Developer Guide> .
--
-- * 'idaDkimEnabled' - True if DKIM signing is enabled for email sent from the identity; false otherwise. The default value is true.
--
-- * 'idaDkimVerificationStatus' - Describes whether Amazon SES has successfully verified the DKIM DNS records (tokens) published in the domain name's DNS. (This only applies to domain identities, not email address identities.)
identityDkimAttributes
    :: Bool -- ^ 'idaDkimEnabled'
    -> VerificationStatus -- ^ 'idaDkimVerificationStatus'
    -> IdentityDkimAttributes
identityDkimAttributes pDkimEnabled_ pDkimVerificationStatus_ =
  IdentityDkimAttributes'
    { _idaDkimTokens = Nothing
    , _idaDkimEnabled = pDkimEnabled_
    , _idaDkimVerificationStatus = pDkimVerificationStatus_
    }


-- | A set of character strings that represent the domain's identity. Using these tokens, you will need to create DNS CNAME records that point to DKIM public keys hosted by Amazon SES. Amazon Web Services will eventually detect that you have updated your DNS records; this detection process may take up to 72 hours. Upon successful detection, Amazon SES will be able to DKIM-sign email originating from that domain. (This only applies to domain identities, not email address identities.) For more information about creating DNS records using DKIM tokens, go to the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html Amazon SES Developer Guide> .
idaDkimTokens :: Lens' IdentityDkimAttributes [Text]
idaDkimTokens = lens _idaDkimTokens (\ s a -> s{_idaDkimTokens = a}) . _Default . _Coerce

-- | True if DKIM signing is enabled for email sent from the identity; false otherwise. The default value is true.
idaDkimEnabled :: Lens' IdentityDkimAttributes Bool
idaDkimEnabled = lens _idaDkimEnabled (\ s a -> s{_idaDkimEnabled = a})

-- | Describes whether Amazon SES has successfully verified the DKIM DNS records (tokens) published in the domain name's DNS. (This only applies to domain identities, not email address identities.)
idaDkimVerificationStatus :: Lens' IdentityDkimAttributes VerificationStatus
idaDkimVerificationStatus = lens _idaDkimVerificationStatus (\ s a -> s{_idaDkimVerificationStatus = a})

instance FromXML IdentityDkimAttributes where
        parseXML x
          = IdentityDkimAttributes' <$>
              (x .@? "DkimTokens" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@ "DkimEnabled")
                <*> (x .@ "DkimVerificationStatus")

instance Hashable IdentityDkimAttributes where

instance NFData IdentityDkimAttributes where

-- | Represents the custom MAIL FROM domain attributes of a verified identity (email address or domain).
--
--
--
-- /See:/ 'identityMailFromDomainAttributes' smart constructor.
data IdentityMailFromDomainAttributes = IdentityMailFromDomainAttributes'
  { _imfdaMailFromDomain       :: !Text
  , _imfdaMailFromDomainStatus :: !CustomMailFromStatus
  , _imfdaBehaviorOnMXFailure  :: !BehaviorOnMXFailure
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IdentityMailFromDomainAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imfdaMailFromDomain' - The custom MAIL FROM domain that the identity is configured to use.
--
-- * 'imfdaMailFromDomainStatus' - The state that indicates whether Amazon SES has successfully read the MX record required for custom MAIL FROM domain setup. If the state is @Success@ , Amazon SES uses the specified custom MAIL FROM domain when the verified identity sends an email. All other states indicate that Amazon SES takes the action described by @BehaviorOnMXFailure@ .
--
-- * 'imfdaBehaviorOnMXFailure' - The action that Amazon SES takes if it cannot successfully read the required MX record when you send an email. A value of @UseDefaultValue@ indicates that if Amazon SES cannot read the required MX record, it uses amazonses.com (or a subdomain of that) as the MAIL FROM domain. A value of @RejectMessage@ indicates that if Amazon SES cannot read the required MX record, Amazon SES returns a @MailFromDomainNotVerified@ error and does not send the email. The custom MAIL FROM setup states that result in this behavior are @Pending@ , @Failed@ , and @TemporaryFailure@ .
identityMailFromDomainAttributes
    :: Text -- ^ 'imfdaMailFromDomain'
    -> CustomMailFromStatus -- ^ 'imfdaMailFromDomainStatus'
    -> BehaviorOnMXFailure -- ^ 'imfdaBehaviorOnMXFailure'
    -> IdentityMailFromDomainAttributes
identityMailFromDomainAttributes pMailFromDomain_ pMailFromDomainStatus_ pBehaviorOnMXFailure_ =
  IdentityMailFromDomainAttributes'
    { _imfdaMailFromDomain = pMailFromDomain_
    , _imfdaMailFromDomainStatus = pMailFromDomainStatus_
    , _imfdaBehaviorOnMXFailure = pBehaviorOnMXFailure_
    }


-- | The custom MAIL FROM domain that the identity is configured to use.
imfdaMailFromDomain :: Lens' IdentityMailFromDomainAttributes Text
imfdaMailFromDomain = lens _imfdaMailFromDomain (\ s a -> s{_imfdaMailFromDomain = a})

-- | The state that indicates whether Amazon SES has successfully read the MX record required for custom MAIL FROM domain setup. If the state is @Success@ , Amazon SES uses the specified custom MAIL FROM domain when the verified identity sends an email. All other states indicate that Amazon SES takes the action described by @BehaviorOnMXFailure@ .
imfdaMailFromDomainStatus :: Lens' IdentityMailFromDomainAttributes CustomMailFromStatus
imfdaMailFromDomainStatus = lens _imfdaMailFromDomainStatus (\ s a -> s{_imfdaMailFromDomainStatus = a})

-- | The action that Amazon SES takes if it cannot successfully read the required MX record when you send an email. A value of @UseDefaultValue@ indicates that if Amazon SES cannot read the required MX record, it uses amazonses.com (or a subdomain of that) as the MAIL FROM domain. A value of @RejectMessage@ indicates that if Amazon SES cannot read the required MX record, Amazon SES returns a @MailFromDomainNotVerified@ error and does not send the email. The custom MAIL FROM setup states that result in this behavior are @Pending@ , @Failed@ , and @TemporaryFailure@ .
imfdaBehaviorOnMXFailure :: Lens' IdentityMailFromDomainAttributes BehaviorOnMXFailure
imfdaBehaviorOnMXFailure = lens _imfdaBehaviorOnMXFailure (\ s a -> s{_imfdaBehaviorOnMXFailure = a})

instance FromXML IdentityMailFromDomainAttributes
         where
        parseXML x
          = IdentityMailFromDomainAttributes' <$>
              (x .@ "MailFromDomain") <*>
                (x .@ "MailFromDomainStatus")
                <*> (x .@ "BehaviorOnMXFailure")

instance Hashable IdentityMailFromDomainAttributes
         where

instance NFData IdentityMailFromDomainAttributes
         where

-- | Represents the notification attributes of an identity, including whether an identity has Amazon Simple Notification Service (Amazon SNS) topics set for bounce, complaint, and/or delivery notifications, and whether feedback forwarding is enabled for bounce and complaint notifications.
--
--
--
-- /See:/ 'identityNotificationAttributes' smart constructor.
data IdentityNotificationAttributes = IdentityNotificationAttributes'
  { _inaHeadersInDeliveryNotificationsEnabled  :: !(Maybe Bool)
  , _inaHeadersInComplaintNotificationsEnabled :: !(Maybe Bool)
  , _inaHeadersInBounceNotificationsEnabled    :: !(Maybe Bool)
  , _inaBounceTopic                            :: !Text
  , _inaComplaintTopic                         :: !Text
  , _inaDeliveryTopic                          :: !Text
  , _inaForwardingEnabled                      :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IdentityNotificationAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'inaHeadersInDeliveryNotificationsEnabled' - Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Delivery@ . A value of @true@ specifies that Amazon SES will include headers in delivery notifications, and a value of @false@ specifies that Amazon SES will not include headers in delivery notifications.
--
-- * 'inaHeadersInComplaintNotificationsEnabled' - Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Complaint@ . A value of @true@ specifies that Amazon SES will include headers in complaint notifications, and a value of @false@ specifies that Amazon SES will not include headers in complaint notifications.
--
-- * 'inaHeadersInBounceNotificationsEnabled' - Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Bounce@ . A value of @true@ specifies that Amazon SES will include headers in bounce notifications, and a value of @false@ specifies that Amazon SES will not include headers in bounce notifications.
--
-- * 'inaBounceTopic' - The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish bounce notifications.
--
-- * 'inaComplaintTopic' - The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish complaint notifications.
--
-- * 'inaDeliveryTopic' - The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish delivery notifications.
--
-- * 'inaForwardingEnabled' - Describes whether Amazon SES will forward bounce and complaint notifications as email. @true@ indicates that Amazon SES will forward bounce and complaint notifications as email, while @false@ indicates that bounce and complaint notifications will be published only to the specified bounce and complaint Amazon SNS topics.
identityNotificationAttributes
    :: Text -- ^ 'inaBounceTopic'
    -> Text -- ^ 'inaComplaintTopic'
    -> Text -- ^ 'inaDeliveryTopic'
    -> Bool -- ^ 'inaForwardingEnabled'
    -> IdentityNotificationAttributes
identityNotificationAttributes pBounceTopic_ pComplaintTopic_ pDeliveryTopic_ pForwardingEnabled_ =
  IdentityNotificationAttributes'
    { _inaHeadersInDeliveryNotificationsEnabled = Nothing
    , _inaHeadersInComplaintNotificationsEnabled = Nothing
    , _inaHeadersInBounceNotificationsEnabled = Nothing
    , _inaBounceTopic = pBounceTopic_
    , _inaComplaintTopic = pComplaintTopic_
    , _inaDeliveryTopic = pDeliveryTopic_
    , _inaForwardingEnabled = pForwardingEnabled_
    }


-- | Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Delivery@ . A value of @true@ specifies that Amazon SES will include headers in delivery notifications, and a value of @false@ specifies that Amazon SES will not include headers in delivery notifications.
inaHeadersInDeliveryNotificationsEnabled :: Lens' IdentityNotificationAttributes (Maybe Bool)
inaHeadersInDeliveryNotificationsEnabled = lens _inaHeadersInDeliveryNotificationsEnabled (\ s a -> s{_inaHeadersInDeliveryNotificationsEnabled = a})

-- | Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Complaint@ . A value of @true@ specifies that Amazon SES will include headers in complaint notifications, and a value of @false@ specifies that Amazon SES will not include headers in complaint notifications.
inaHeadersInComplaintNotificationsEnabled :: Lens' IdentityNotificationAttributes (Maybe Bool)
inaHeadersInComplaintNotificationsEnabled = lens _inaHeadersInComplaintNotificationsEnabled (\ s a -> s{_inaHeadersInComplaintNotificationsEnabled = a})

-- | Describes whether Amazon SES includes the original email headers in Amazon SNS notifications of type @Bounce@ . A value of @true@ specifies that Amazon SES will include headers in bounce notifications, and a value of @false@ specifies that Amazon SES will not include headers in bounce notifications.
inaHeadersInBounceNotificationsEnabled :: Lens' IdentityNotificationAttributes (Maybe Bool)
inaHeadersInBounceNotificationsEnabled = lens _inaHeadersInBounceNotificationsEnabled (\ s a -> s{_inaHeadersInBounceNotificationsEnabled = a})

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish bounce notifications.
inaBounceTopic :: Lens' IdentityNotificationAttributes Text
inaBounceTopic = lens _inaBounceTopic (\ s a -> s{_inaBounceTopic = a})

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish complaint notifications.
inaComplaintTopic :: Lens' IdentityNotificationAttributes Text
inaComplaintTopic = lens _inaComplaintTopic (\ s a -> s{_inaComplaintTopic = a})

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic where Amazon SES will publish delivery notifications.
inaDeliveryTopic :: Lens' IdentityNotificationAttributes Text
inaDeliveryTopic = lens _inaDeliveryTopic (\ s a -> s{_inaDeliveryTopic = a})

-- | Describes whether Amazon SES will forward bounce and complaint notifications as email. @true@ indicates that Amazon SES will forward bounce and complaint notifications as email, while @false@ indicates that bounce and complaint notifications will be published only to the specified bounce and complaint Amazon SNS topics.
inaForwardingEnabled :: Lens' IdentityNotificationAttributes Bool
inaForwardingEnabled = lens _inaForwardingEnabled (\ s a -> s{_inaForwardingEnabled = a})

instance FromXML IdentityNotificationAttributes where
        parseXML x
          = IdentityNotificationAttributes' <$>
              (x .@? "HeadersInDeliveryNotificationsEnabled") <*>
                (x .@? "HeadersInComplaintNotificationsEnabled")
                <*> (x .@? "HeadersInBounceNotificationsEnabled")
                <*> (x .@ "BounceTopic")
                <*> (x .@ "ComplaintTopic")
                <*> (x .@ "DeliveryTopic")
                <*> (x .@ "ForwardingEnabled")

instance Hashable IdentityNotificationAttributes
         where

instance NFData IdentityNotificationAttributes where

-- | Represents the verification attributes of a single identity.
--
--
--
-- /See:/ 'identityVerificationAttributes' smart constructor.
data IdentityVerificationAttributes = IdentityVerificationAttributes'
  { _ivaVerificationToken  :: !(Maybe Text)
  , _ivaVerificationStatus :: !VerificationStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IdentityVerificationAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ivaVerificationToken' - The verification token for a domain identity. Null for email address identities.
--
-- * 'ivaVerificationStatus' - The verification status of the identity: "Pending", "Success", "Failed", or "TemporaryFailure".
identityVerificationAttributes
    :: VerificationStatus -- ^ 'ivaVerificationStatus'
    -> IdentityVerificationAttributes
identityVerificationAttributes pVerificationStatus_ =
  IdentityVerificationAttributes'
    { _ivaVerificationToken = Nothing
    , _ivaVerificationStatus = pVerificationStatus_
    }


-- | The verification token for a domain identity. Null for email address identities.
ivaVerificationToken :: Lens' IdentityVerificationAttributes (Maybe Text)
ivaVerificationToken = lens _ivaVerificationToken (\ s a -> s{_ivaVerificationToken = a})

-- | The verification status of the identity: "Pending", "Success", "Failed", or "TemporaryFailure".
ivaVerificationStatus :: Lens' IdentityVerificationAttributes VerificationStatus
ivaVerificationStatus = lens _ivaVerificationStatus (\ s a -> s{_ivaVerificationStatus = a})

instance FromXML IdentityVerificationAttributes where
        parseXML x
          = IdentityVerificationAttributes' <$>
              (x .@? "VerificationToken") <*>
                (x .@ "VerificationStatus")

instance Hashable IdentityVerificationAttributes
         where

instance NFData IdentityVerificationAttributes where

-- | Contains the delivery stream ARN and the IAM role ARN associated with an Amazon Kinesis Firehose event destination.
--
--
-- Event destinations, such as Amazon Kinesis Firehose, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'kinesisFirehoseDestination' smart constructor.
data KinesisFirehoseDestination = KinesisFirehoseDestination'
  { _kfdIAMRoleARN        :: !Text
  , _kfdDeliveryStreamARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisFirehoseDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kfdIAMRoleARN' - The ARN of the IAM role under which Amazon SES publishes email sending events to the Amazon Kinesis Firehose stream.
--
-- * 'kfdDeliveryStreamARN' - The ARN of the Amazon Kinesis Firehose stream that email sending events should be published to.
kinesisFirehoseDestination
    :: Text -- ^ 'kfdIAMRoleARN'
    -> Text -- ^ 'kfdDeliveryStreamARN'
    -> KinesisFirehoseDestination
kinesisFirehoseDestination pIAMRoleARN_ pDeliveryStreamARN_ =
  KinesisFirehoseDestination'
    {_kfdIAMRoleARN = pIAMRoleARN_, _kfdDeliveryStreamARN = pDeliveryStreamARN_}


-- | The ARN of the IAM role under which Amazon SES publishes email sending events to the Amazon Kinesis Firehose stream.
kfdIAMRoleARN :: Lens' KinesisFirehoseDestination Text
kfdIAMRoleARN = lens _kfdIAMRoleARN (\ s a -> s{_kfdIAMRoleARN = a})

-- | The ARN of the Amazon Kinesis Firehose stream that email sending events should be published to.
kfdDeliveryStreamARN :: Lens' KinesisFirehoseDestination Text
kfdDeliveryStreamARN = lens _kfdDeliveryStreamARN (\ s a -> s{_kfdDeliveryStreamARN = a})

instance FromXML KinesisFirehoseDestination where
        parseXML x
          = KinesisFirehoseDestination' <$>
              (x .@ "IAMRoleARN") <*> (x .@ "DeliveryStreamARN")

instance Hashable KinesisFirehoseDestination where

instance NFData KinesisFirehoseDestination where

instance ToQuery KinesisFirehoseDestination where
        toQuery KinesisFirehoseDestination'{..}
          = mconcat
              ["IAMRoleARN" =: _kfdIAMRoleARN,
               "DeliveryStreamARN" =: _kfdDeliveryStreamARN]

-- | When included in a receipt rule, this action calls an AWS Lambda function and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
--
-- To enable Amazon SES to call your AWS Lambda function or to publish to an Amazon SNS topic of another account, Amazon SES must have permission to access those resources. For information about giving permissions, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
--
-- For information about using AWS Lambda actions in receipt rules, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-lambda.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'lambdaAction' smart constructor.
data LambdaAction = LambdaAction'
  { _laInvocationType :: !(Maybe InvocationType)
  , _laTopicARN       :: !(Maybe Text)
  , _laFunctionARN    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laInvocationType' - The invocation type of the AWS Lambda function. An invocation type of @RequestResponse@ means that the execution of the function will immediately result in a response, and a value of @Event@ means that the function will be invoked asynchronously. The default value is @Event@ . For information about AWS Lambda invocation types, see the <http://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html AWS Lambda Developer Guide> . /Important:/ There is a 30-second timeout on @RequestResponse@ invocations. You should use @Event@ invocation in most cases. Use @RequestResponse@ only when you want to make a mail flow decision, such as whether to stop the receipt rule or the receipt rule set.
--
-- * 'laTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the Lambda action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <http://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- * 'laFunctionARN' - The Amazon Resource Name (ARN) of the AWS Lambda function. An example of an AWS Lambda function ARN is @arn:aws:lambda:us-west-2:account-id:function:MyFunction@ . For more information about AWS Lambda, see the <http://docs.aws.amazon.com/lambda/latest/dg/welcome.html AWS Lambda Developer Guide> .
lambdaAction
    :: Text -- ^ 'laFunctionARN'
    -> LambdaAction
lambdaAction pFunctionARN_ =
  LambdaAction'
    { _laInvocationType = Nothing
    , _laTopicARN = Nothing
    , _laFunctionARN = pFunctionARN_
    }


-- | The invocation type of the AWS Lambda function. An invocation type of @RequestResponse@ means that the execution of the function will immediately result in a response, and a value of @Event@ means that the function will be invoked asynchronously. The default value is @Event@ . For information about AWS Lambda invocation types, see the <http://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html AWS Lambda Developer Guide> . /Important:/ There is a 30-second timeout on @RequestResponse@ invocations. You should use @Event@ invocation in most cases. Use @RequestResponse@ only when you want to make a mail flow decision, such as whether to stop the receipt rule or the receipt rule set.
laInvocationType :: Lens' LambdaAction (Maybe InvocationType)
laInvocationType = lens _laInvocationType (\ s a -> s{_laInvocationType = a})

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the Lambda action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <http://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
laTopicARN :: Lens' LambdaAction (Maybe Text)
laTopicARN = lens _laTopicARN (\ s a -> s{_laTopicARN = a})

-- | The Amazon Resource Name (ARN) of the AWS Lambda function. An example of an AWS Lambda function ARN is @arn:aws:lambda:us-west-2:account-id:function:MyFunction@ . For more information about AWS Lambda, see the <http://docs.aws.amazon.com/lambda/latest/dg/welcome.html AWS Lambda Developer Guide> .
laFunctionARN :: Lens' LambdaAction Text
laFunctionARN = lens _laFunctionARN (\ s a -> s{_laFunctionARN = a})

instance FromXML LambdaAction where
        parseXML x
          = LambdaAction' <$>
              (x .@? "InvocationType") <*> (x .@? "TopicArn") <*>
                (x .@ "FunctionArn")

instance Hashable LambdaAction where

instance NFData LambdaAction where

instance ToQuery LambdaAction where
        toQuery LambdaAction'{..}
          = mconcat
              ["InvocationType" =: _laInvocationType,
               "TopicArn" =: _laTopicARN,
               "FunctionArn" =: _laFunctionARN]

-- | Represents the message to be sent, composed of a subject and a body.
--
--
--
-- /See:/ 'message' smart constructor.
data Message = Message'
  { _mSubject :: !Content
  , _mBody    :: !Body
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mSubject' - The subject of the message: A short summary of the content, which will appear in the recipient's inbox.
--
-- * 'mBody' - The message body.
message
    :: Content -- ^ 'mSubject'
    -> Body -- ^ 'mBody'
    -> Message
message pSubject_ pBody_ = Message' {_mSubject = pSubject_, _mBody = pBody_}


-- | The subject of the message: A short summary of the content, which will appear in the recipient's inbox.
mSubject :: Lens' Message Content
mSubject = lens _mSubject (\ s a -> s{_mSubject = a})

-- | The message body.
mBody :: Lens' Message Body
mBody = lens _mBody (\ s a -> s{_mBody = a})

instance Hashable Message where

instance NFData Message where

instance ToQuery Message where
        toQuery Message'{..}
          = mconcat ["Subject" =: _mSubject, "Body" =: _mBody]

-- | Message-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.
--
--
-- For information about receiving email through Amazon SES, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'messageDsn' smart constructor.
data MessageDsn = MessageDsn'
  { _mdArrivalDate     :: !(Maybe ISO8601)
  , _mdExtensionFields :: !(Maybe [ExtensionField])
  , _mdReportingMta    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MessageDsn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdArrivalDate' - When the message was received by the reporting mail transfer agent (MTA), in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
--
-- * 'mdExtensionFields' - Additional X-headers to include in the DSN.
--
-- * 'mdReportingMta' - The reporting MTA that attempted to deliver the message, formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464> (@mta-name-type; mta-name@ ). The default value is @dns; inbound-smtp.[region].amazonaws.com@ .
messageDsn
    :: Text -- ^ 'mdReportingMta'
    -> MessageDsn
messageDsn pReportingMta_ =
  MessageDsn'
    { _mdArrivalDate = Nothing
    , _mdExtensionFields = Nothing
    , _mdReportingMta = pReportingMta_
    }


-- | When the message was received by the reporting mail transfer agent (MTA), in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
mdArrivalDate :: Lens' MessageDsn (Maybe UTCTime)
mdArrivalDate = lens _mdArrivalDate (\ s a -> s{_mdArrivalDate = a}) . mapping _Time

-- | Additional X-headers to include in the DSN.
mdExtensionFields :: Lens' MessageDsn [ExtensionField]
mdExtensionFields = lens _mdExtensionFields (\ s a -> s{_mdExtensionFields = a}) . _Default . _Coerce

-- | The reporting MTA that attempted to deliver the message, formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464> (@mta-name-type; mta-name@ ). The default value is @dns; inbound-smtp.[region].amazonaws.com@ .
mdReportingMta :: Lens' MessageDsn Text
mdReportingMta = lens _mdReportingMta (\ s a -> s{_mdReportingMta = a})

instance Hashable MessageDsn where

instance NFData MessageDsn where

instance ToQuery MessageDsn where
        toQuery MessageDsn'{..}
          = mconcat
              ["ArrivalDate" =: _mdArrivalDate,
               "ExtensionFields" =:
                 toQuery
                   (toQueryList "member" <$> _mdExtensionFields),
               "ReportingMta" =: _mdReportingMta]

-- | Contains the name and value of a tag that you can provide to @SendEmail@ or @SendRawEmail@ to apply to an email.
--
--
-- Message tags, which you use with configuration sets, enable you to publish email sending events. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'messageTag' smart constructor.
data MessageTag = MessageTag'
  { _mtName  :: !Text
  , _mtValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MessageTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtName' - The name of the tag. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 256 characters.
--
-- * 'mtValue' - The value of the tag. The value must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 256 characters.
messageTag
    :: Text -- ^ 'mtName'
    -> Text -- ^ 'mtValue'
    -> MessageTag
messageTag pName_ pValue_ = MessageTag' {_mtName = pName_, _mtValue = pValue_}


-- | The name of the tag. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 256 characters.
mtName :: Lens' MessageTag Text
mtName = lens _mtName (\ s a -> s{_mtName = a})

-- | The value of the tag. The value must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 256 characters.
mtValue :: Lens' MessageTag Text
mtValue = lens _mtValue (\ s a -> s{_mtValue = a})

instance Hashable MessageTag where

instance NFData MessageTag where

instance ToQuery MessageTag where
        toQuery MessageTag'{..}
          = mconcat ["Name" =: _mtName, "Value" =: _mtValue]

-- | Represents the raw data of the message.
--
--
--
-- /See:/ 'rawMessage' smart constructor.
newtype RawMessage = RawMessage'
  { _rmData :: Base64
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RawMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmData' - The raw data of the message. This data needs to base64-encoded if you are accessing Amazon SES directly through the HTTPS interface. If you are accessing Amazon SES using an AWS SDK, the SDK takes care of the base 64-encoding for you. In all cases, the client must ensure that the message format complies with Internet email standards regarding email header fields, MIME types, and MIME encoding. The To:, CC:, and BCC: headers in the raw message can contain a group list. If you are using @SendRawEmail@ with sending authorization, you can include X-headers in the raw message to specify the "Source," "From," and "Return-Path" addresses. For more information, see the documentation for @SendRawEmail@ .  /Important:/ Do not include these X-headers in the DKIM signature, because they are removed by Amazon SES before sending the email. For more information, go to the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html Amazon SES Developer Guide> .-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
rawMessage
    :: ByteString -- ^ 'rmData'
    -> RawMessage
rawMessage pData_ = RawMessage' {_rmData = _Base64 # pData_}


-- | The raw data of the message. This data needs to base64-encoded if you are accessing Amazon SES directly through the HTTPS interface. If you are accessing Amazon SES using an AWS SDK, the SDK takes care of the base 64-encoding for you. In all cases, the client must ensure that the message format complies with Internet email standards regarding email header fields, MIME types, and MIME encoding. The To:, CC:, and BCC: headers in the raw message can contain a group list. If you are using @SendRawEmail@ with sending authorization, you can include X-headers in the raw message to specify the "Source," "From," and "Return-Path" addresses. For more information, see the documentation for @SendRawEmail@ .  /Important:/ Do not include these X-headers in the DKIM signature, because they are removed by Amazon SES before sending the email. For more information, go to the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html Amazon SES Developer Guide> .-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
rmData :: Lens' RawMessage ByteString
rmData = lens _rmData (\ s a -> s{_rmData = a}) . _Base64

instance Hashable RawMessage where

instance NFData RawMessage where

instance ToQuery RawMessage where
        toQuery RawMessage'{..} = mconcat ["Data" =: _rmData]

-- | An action that Amazon SES can take when it receives an email on behalf of one or more email addresses or domains that you own. An instance of this data type can represent only one action.
--
--
-- For information about setting up receipt rules, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'receiptAction' smart constructor.
data ReceiptAction = ReceiptAction'
  { _raAddHeaderAction :: !(Maybe AddHeaderAction)
  , _raSNSAction       :: !(Maybe SNSAction)
  , _raWorkmailAction  :: !(Maybe WorkmailAction)
  , _raBounceAction    :: !(Maybe BounceAction)
  , _raLambdaAction    :: !(Maybe LambdaAction)
  , _raStopAction      :: !(Maybe StopAction)
  , _raS3Action        :: !(Maybe S3Action)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReceiptAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raAddHeaderAction' - Adds a header to the received email.
--
-- * 'raSNSAction' - Publishes the email content within a notification to Amazon SNS.
--
-- * 'raWorkmailAction' - Calls Amazon WorkMail and, optionally, publishes a notification to Amazon SNS.
--
-- * 'raBounceAction' - Rejects the received email by returning a bounce response to the sender and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
-- * 'raLambdaAction' - Calls an AWS Lambda function, and optionally, publishes a notification to Amazon SNS.
--
-- * 'raStopAction' - Terminates the evaluation of the receipt rule set and optionally publishes a notification to Amazon SNS.
--
-- * 'raS3Action' - Saves the received message to an Amazon Simple Storage Service (Amazon S3) bucket and, optionally, publishes a notification to Amazon SNS.
receiptAction
    :: ReceiptAction
receiptAction =
  ReceiptAction'
    { _raAddHeaderAction = Nothing
    , _raSNSAction = Nothing
    , _raWorkmailAction = Nothing
    , _raBounceAction = Nothing
    , _raLambdaAction = Nothing
    , _raStopAction = Nothing
    , _raS3Action = Nothing
    }


-- | Adds a header to the received email.
raAddHeaderAction :: Lens' ReceiptAction (Maybe AddHeaderAction)
raAddHeaderAction = lens _raAddHeaderAction (\ s a -> s{_raAddHeaderAction = a})

-- | Publishes the email content within a notification to Amazon SNS.
raSNSAction :: Lens' ReceiptAction (Maybe SNSAction)
raSNSAction = lens _raSNSAction (\ s a -> s{_raSNSAction = a})

-- | Calls Amazon WorkMail and, optionally, publishes a notification to Amazon SNS.
raWorkmailAction :: Lens' ReceiptAction (Maybe WorkmailAction)
raWorkmailAction = lens _raWorkmailAction (\ s a -> s{_raWorkmailAction = a})

-- | Rejects the received email by returning a bounce response to the sender and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
raBounceAction :: Lens' ReceiptAction (Maybe BounceAction)
raBounceAction = lens _raBounceAction (\ s a -> s{_raBounceAction = a})

-- | Calls an AWS Lambda function, and optionally, publishes a notification to Amazon SNS.
raLambdaAction :: Lens' ReceiptAction (Maybe LambdaAction)
raLambdaAction = lens _raLambdaAction (\ s a -> s{_raLambdaAction = a})

-- | Terminates the evaluation of the receipt rule set and optionally publishes a notification to Amazon SNS.
raStopAction :: Lens' ReceiptAction (Maybe StopAction)
raStopAction = lens _raStopAction (\ s a -> s{_raStopAction = a})

-- | Saves the received message to an Amazon Simple Storage Service (Amazon S3) bucket and, optionally, publishes a notification to Amazon SNS.
raS3Action :: Lens' ReceiptAction (Maybe S3Action)
raS3Action = lens _raS3Action (\ s a -> s{_raS3Action = a})

instance FromXML ReceiptAction where
        parseXML x
          = ReceiptAction' <$>
              (x .@? "AddHeaderAction") <*> (x .@? "SNSAction") <*>
                (x .@? "WorkmailAction")
                <*> (x .@? "BounceAction")
                <*> (x .@? "LambdaAction")
                <*> (x .@? "StopAction")
                <*> (x .@? "S3Action")

instance Hashable ReceiptAction where

instance NFData ReceiptAction where

instance ToQuery ReceiptAction where
        toQuery ReceiptAction'{..}
          = mconcat
              ["AddHeaderAction" =: _raAddHeaderAction,
               "SNSAction" =: _raSNSAction,
               "WorkmailAction" =: _raWorkmailAction,
               "BounceAction" =: _raBounceAction,
               "LambdaAction" =: _raLambdaAction,
               "StopAction" =: _raStopAction,
               "S3Action" =: _raS3Action]

-- | A receipt IP address filter enables you to specify whether to accept or reject mail originating from an IP address or range of IP addresses.
--
--
-- For information about setting up IP address filters, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'receiptFilter' smart constructor.
data ReceiptFilter = ReceiptFilter'
  { _rfName     :: !Text
  , _rfIPFilter :: !ReceiptIPFilter
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReceiptFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfName' - The name of the IP address filter. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Start and end with a letter or number.     * Contain less than 64 characters.
--
-- * 'rfIPFilter' - A structure that provides the IP addresses to block or allow, and whether to block or allow incoming mail from them.
receiptFilter
    :: Text -- ^ 'rfName'
    -> ReceiptIPFilter -- ^ 'rfIPFilter'
    -> ReceiptFilter
receiptFilter pName_ pIPFilter_ =
  ReceiptFilter' {_rfName = pName_, _rfIPFilter = pIPFilter_}


-- | The name of the IP address filter. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Start and end with a letter or number.     * Contain less than 64 characters.
rfName :: Lens' ReceiptFilter Text
rfName = lens _rfName (\ s a -> s{_rfName = a})

-- | A structure that provides the IP addresses to block or allow, and whether to block or allow incoming mail from them.
rfIPFilter :: Lens' ReceiptFilter ReceiptIPFilter
rfIPFilter = lens _rfIPFilter (\ s a -> s{_rfIPFilter = a})

instance FromXML ReceiptFilter where
        parseXML x
          = ReceiptFilter' <$>
              (x .@ "Name") <*> (x .@ "IpFilter")

instance Hashable ReceiptFilter where

instance NFData ReceiptFilter where

instance ToQuery ReceiptFilter where
        toQuery ReceiptFilter'{..}
          = mconcat
              ["Name" =: _rfName, "IpFilter" =: _rfIPFilter]

-- | A receipt IP address filter enables you to specify whether to accept or reject mail originating from an IP address or range of IP addresses.
--
--
-- For information about setting up IP address filters, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'receiptIPFilter' smart constructor.
data ReceiptIPFilter = ReceiptIPFilter'
  { _rifPolicy :: !ReceiptFilterPolicy
  , _rifCidr   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReceiptIPFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rifPolicy' - Indicates whether to block or allow incoming mail from the specified IP addresses.
--
-- * 'rifCidr' - A single IP address or a range of IP addresses that you want to block or allow, specified in Classless Inter-Domain Routing (CIDR) notation. An example of a single email address is 10.0.0.1. An example of a range of IP addresses is 10.0.0.1/24. For more information about CIDR notation, see <https://tools.ietf.org/html/rfc2317 RFC 2317> .
receiptIPFilter
    :: ReceiptFilterPolicy -- ^ 'rifPolicy'
    -> Text -- ^ 'rifCidr'
    -> ReceiptIPFilter
receiptIPFilter pPolicy_ pCidr_ =
  ReceiptIPFilter' {_rifPolicy = pPolicy_, _rifCidr = pCidr_}


-- | Indicates whether to block or allow incoming mail from the specified IP addresses.
rifPolicy :: Lens' ReceiptIPFilter ReceiptFilterPolicy
rifPolicy = lens _rifPolicy (\ s a -> s{_rifPolicy = a})

-- | A single IP address or a range of IP addresses that you want to block or allow, specified in Classless Inter-Domain Routing (CIDR) notation. An example of a single email address is 10.0.0.1. An example of a range of IP addresses is 10.0.0.1/24. For more information about CIDR notation, see <https://tools.ietf.org/html/rfc2317 RFC 2317> .
rifCidr :: Lens' ReceiptIPFilter Text
rifCidr = lens _rifCidr (\ s a -> s{_rifCidr = a})

instance FromXML ReceiptIPFilter where
        parseXML x
          = ReceiptIPFilter' <$>
              (x .@ "Policy") <*> (x .@ "Cidr")

instance Hashable ReceiptIPFilter where

instance NFData ReceiptIPFilter where

instance ToQuery ReceiptIPFilter where
        toQuery ReceiptIPFilter'{..}
          = mconcat
              ["Policy" =: _rifPolicy, "Cidr" =: _rifCidr]

-- | Receipt rules enable you to specify which actions Amazon SES should take when it receives mail on behalf of one or more email addresses or domains that you own.
--
--
-- Each receipt rule defines a set of email addresses or domains that it applies to. If the email addresses or domains match at least one recipient address of the message, Amazon SES executes all of the receipt rule's actions on the message.
--
-- For information about setting up receipt rules, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'receiptRule' smart constructor.
data ReceiptRule = ReceiptRule'
  { _rrScanEnabled :: !(Maybe Bool)
  , _rrEnabled     :: !(Maybe Bool)
  , _rrActions     :: !(Maybe [ReceiptAction])
  , _rrRecipients  :: !(Maybe [Text])
  , _rrTLSPolicy   :: !(Maybe TLSPolicy)
  , _rrName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReceiptRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrScanEnabled' - If @true@ , then messages that this receipt rule applies to are scanned for spam and viruses. The default value is @false@ .
--
-- * 'rrEnabled' - If @true@ , the receipt rule is active. The default value is @false@ .
--
-- * 'rrActions' - An ordered list of actions to perform on messages that match at least one of the recipient email addresses or domains specified in the receipt rule.
--
-- * 'rrRecipients' - The recipient domains and email addresses that the receipt rule applies to. If this field is not specified, this rule will match all recipients under all verified domains.
--
-- * 'rrTLSPolicy' - Specifies whether Amazon SES should require that incoming email is delivered over a connection encrypted with Transport Layer Security (TLS). If this parameter is set to @Require@ , Amazon SES will bounce emails that are not received over TLS. The default is @Optional@ .
--
-- * 'rrName' - The name of the receipt rule. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Start and end with a letter or number.     * Contain less than 64 characters.
receiptRule
    :: Text -- ^ 'rrName'
    -> ReceiptRule
receiptRule pName_ =
  ReceiptRule'
    { _rrScanEnabled = Nothing
    , _rrEnabled = Nothing
    , _rrActions = Nothing
    , _rrRecipients = Nothing
    , _rrTLSPolicy = Nothing
    , _rrName = pName_
    }


-- | If @true@ , then messages that this receipt rule applies to are scanned for spam and viruses. The default value is @false@ .
rrScanEnabled :: Lens' ReceiptRule (Maybe Bool)
rrScanEnabled = lens _rrScanEnabled (\ s a -> s{_rrScanEnabled = a})

-- | If @true@ , the receipt rule is active. The default value is @false@ .
rrEnabled :: Lens' ReceiptRule (Maybe Bool)
rrEnabled = lens _rrEnabled (\ s a -> s{_rrEnabled = a})

-- | An ordered list of actions to perform on messages that match at least one of the recipient email addresses or domains specified in the receipt rule.
rrActions :: Lens' ReceiptRule [ReceiptAction]
rrActions = lens _rrActions (\ s a -> s{_rrActions = a}) . _Default . _Coerce

-- | The recipient domains and email addresses that the receipt rule applies to. If this field is not specified, this rule will match all recipients under all verified domains.
rrRecipients :: Lens' ReceiptRule [Text]
rrRecipients = lens _rrRecipients (\ s a -> s{_rrRecipients = a}) . _Default . _Coerce

-- | Specifies whether Amazon SES should require that incoming email is delivered over a connection encrypted with Transport Layer Security (TLS). If this parameter is set to @Require@ , Amazon SES will bounce emails that are not received over TLS. The default is @Optional@ .
rrTLSPolicy :: Lens' ReceiptRule (Maybe TLSPolicy)
rrTLSPolicy = lens _rrTLSPolicy (\ s a -> s{_rrTLSPolicy = a})

-- | The name of the receipt rule. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Start and end with a letter or number.     * Contain less than 64 characters.
rrName :: Lens' ReceiptRule Text
rrName = lens _rrName (\ s a -> s{_rrName = a})

instance FromXML ReceiptRule where
        parseXML x
          = ReceiptRule' <$>
              (x .@? "ScanEnabled") <*> (x .@? "Enabled") <*>
                (x .@? "Actions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Recipients" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "TlsPolicy")
                <*> (x .@ "Name")

instance Hashable ReceiptRule where

instance NFData ReceiptRule where

instance ToQuery ReceiptRule where
        toQuery ReceiptRule'{..}
          = mconcat
              ["ScanEnabled" =: _rrScanEnabled,
               "Enabled" =: _rrEnabled,
               "Actions" =:
                 toQuery (toQueryList "member" <$> _rrActions),
               "Recipients" =:
                 toQuery (toQueryList "member" <$> _rrRecipients),
               "TlsPolicy" =: _rrTLSPolicy, "Name" =: _rrName]

-- | Information about a receipt rule set.
--
--
-- A receipt rule set is a collection of rules that specify what Amazon SES should do with mail it receives on behalf of your account's verified domains.
--
-- For information about setting up receipt rule sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'receiptRuleSetMetadata' smart constructor.
data ReceiptRuleSetMetadata = ReceiptRuleSetMetadata'
  { _rrsmName             :: !(Maybe Text)
  , _rrsmCreatedTimestamp :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReceiptRuleSetMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsmName' - The name of the receipt rule set. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Start and end with a letter or number.     * Contain less than 64 characters.
--
-- * 'rrsmCreatedTimestamp' - The date and time the receipt rule set was created.
receiptRuleSetMetadata
    :: ReceiptRuleSetMetadata
receiptRuleSetMetadata =
  ReceiptRuleSetMetadata' {_rrsmName = Nothing, _rrsmCreatedTimestamp = Nothing}


-- | The name of the receipt rule set. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Start and end with a letter or number.     * Contain less than 64 characters.
rrsmName :: Lens' ReceiptRuleSetMetadata (Maybe Text)
rrsmName = lens _rrsmName (\ s a -> s{_rrsmName = a})

-- | The date and time the receipt rule set was created.
rrsmCreatedTimestamp :: Lens' ReceiptRuleSetMetadata (Maybe UTCTime)
rrsmCreatedTimestamp = lens _rrsmCreatedTimestamp (\ s a -> s{_rrsmCreatedTimestamp = a}) . mapping _Time

instance FromXML ReceiptRuleSetMetadata where
        parseXML x
          = ReceiptRuleSetMetadata' <$>
              (x .@? "Name") <*> (x .@? "CreatedTimestamp")

instance Hashable ReceiptRuleSetMetadata where

instance NFData ReceiptRuleSetMetadata where

-- | Recipient-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.
--
--
-- For information about receiving email through Amazon SES, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'recipientDsnFields' smart constructor.
data RecipientDsnFields = RecipientDsnFields'
  { _rdfDiagnosticCode  :: !(Maybe Text)
  , _rdfRemoteMta       :: !(Maybe Text)
  , _rdfFinalRecipient  :: !(Maybe Text)
  , _rdfExtensionFields :: !(Maybe [ExtensionField])
  , _rdfLastAttemptDate :: !(Maybe ISO8601)
  , _rdfAction          :: !DsnAction
  , _rdfStatus          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecipientDsnFields' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdfDiagnosticCode' - An extended explanation of what went wrong; this is usually an SMTP response. See <https://tools.ietf.org/html/rfc3463 RFC 3463> for the correct formatting of this parameter.
--
-- * 'rdfRemoteMta' - The MTA to which the remote MTA attempted to deliver the message, formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464> (@mta-name-type; mta-name@ ). This parameter typically applies only to propagating synchronous bounces.
--
-- * 'rdfFinalRecipient' - The email address that the message was ultimately delivered to. This corresponds to the @Final-Recipient@ in the DSN. If not specified, @FinalRecipient@ will be set to the @Recipient@ specified in the @BouncedRecipientInfo@ structure. Either @FinalRecipient@ or the recipient in @BouncedRecipientInfo@ must be a recipient of the original bounced message.
--
-- * 'rdfExtensionFields' - Additional X-headers to include in the DSN.
--
-- * 'rdfLastAttemptDate' - The time the final delivery attempt was made, in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
--
-- * 'rdfAction' - The action performed by the reporting mail transfer agent (MTA) as a result of its attempt to deliver the message to the recipient address. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
--
-- * 'rdfStatus' - The status code that indicates what went wrong. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
recipientDsnFields
    :: DsnAction -- ^ 'rdfAction'
    -> Text -- ^ 'rdfStatus'
    -> RecipientDsnFields
recipientDsnFields pAction_ pStatus_ =
  RecipientDsnFields'
    { _rdfDiagnosticCode = Nothing
    , _rdfRemoteMta = Nothing
    , _rdfFinalRecipient = Nothing
    , _rdfExtensionFields = Nothing
    , _rdfLastAttemptDate = Nothing
    , _rdfAction = pAction_
    , _rdfStatus = pStatus_
    }


-- | An extended explanation of what went wrong; this is usually an SMTP response. See <https://tools.ietf.org/html/rfc3463 RFC 3463> for the correct formatting of this parameter.
rdfDiagnosticCode :: Lens' RecipientDsnFields (Maybe Text)
rdfDiagnosticCode = lens _rdfDiagnosticCode (\ s a -> s{_rdfDiagnosticCode = a})

-- | The MTA to which the remote MTA attempted to deliver the message, formatted as specified in <https://tools.ietf.org/html/rfc3464 RFC 3464> (@mta-name-type; mta-name@ ). This parameter typically applies only to propagating synchronous bounces.
rdfRemoteMta :: Lens' RecipientDsnFields (Maybe Text)
rdfRemoteMta = lens _rdfRemoteMta (\ s a -> s{_rdfRemoteMta = a})

-- | The email address that the message was ultimately delivered to. This corresponds to the @Final-Recipient@ in the DSN. If not specified, @FinalRecipient@ will be set to the @Recipient@ specified in the @BouncedRecipientInfo@ structure. Either @FinalRecipient@ or the recipient in @BouncedRecipientInfo@ must be a recipient of the original bounced message.
rdfFinalRecipient :: Lens' RecipientDsnFields (Maybe Text)
rdfFinalRecipient = lens _rdfFinalRecipient (\ s a -> s{_rdfFinalRecipient = a})

-- | Additional X-headers to include in the DSN.
rdfExtensionFields :: Lens' RecipientDsnFields [ExtensionField]
rdfExtensionFields = lens _rdfExtensionFields (\ s a -> s{_rdfExtensionFields = a}) . _Default . _Coerce

-- | The time the final delivery attempt was made, in <https://www.ietf.org/rfc/rfc0822.txt RFC 822> date-time format.
rdfLastAttemptDate :: Lens' RecipientDsnFields (Maybe UTCTime)
rdfLastAttemptDate = lens _rdfLastAttemptDate (\ s a -> s{_rdfLastAttemptDate = a}) . mapping _Time

-- | The action performed by the reporting mail transfer agent (MTA) as a result of its attempt to deliver the message to the recipient address. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
rdfAction :: Lens' RecipientDsnFields DsnAction
rdfAction = lens _rdfAction (\ s a -> s{_rdfAction = a})

-- | The status code that indicates what went wrong. This is required by <https://tools.ietf.org/html/rfc3464 RFC 3464> .
rdfStatus :: Lens' RecipientDsnFields Text
rdfStatus = lens _rdfStatus (\ s a -> s{_rdfStatus = a})

instance Hashable RecipientDsnFields where

instance NFData RecipientDsnFields where

instance ToQuery RecipientDsnFields where
        toQuery RecipientDsnFields'{..}
          = mconcat
              ["DiagnosticCode" =: _rdfDiagnosticCode,
               "RemoteMta" =: _rdfRemoteMta,
               "FinalRecipient" =: _rdfFinalRecipient,
               "ExtensionFields" =:
                 toQuery
                   (toQueryList "member" <$> _rdfExtensionFields),
               "LastAttemptDate" =: _rdfLastAttemptDate,
               "Action" =: _rdfAction, "Status" =: _rdfStatus]

-- | Contains information about the reputation settings for a configuration set.
--
--
--
-- /See:/ 'reputationOptions' smart constructor.
data ReputationOptions = ReputationOptions'
  { _roLastFreshStart           :: !(Maybe ISO8601)
  , _roReputationMetricsEnabled :: !(Maybe Bool)
  , _roSendingEnabled           :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReputationOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'roLastFreshStart' - The date and time at which the reputation metrics for the configuration set were last reset. Resetting these metrics is known as a /fresh start/ . When you disable email sending for a configuration set using 'UpdateConfigurationSetSendingEnabled' and later re-enable it, the reputation metrics for the configuration set (but not for the entire Amazon SES account) are reset. If email sending for the configuration set has never been disabled and later re-enabled, the value of this attribute is @null@ .
--
-- * 'roReputationMetricsEnabled' - Describes whether or not Amazon SES publishes reputation metrics for the configuration set, such as bounce and complaint rates, to Amazon CloudWatch. If the value is @true@ , reputation metrics are published. If the value is @false@ , reputation metrics are not published. The default value is @false@ .
--
-- * 'roSendingEnabled' - Describes whether email sending is enabled or disabled for the configuration set. If the value is @true@ , then Amazon SES will send emails that use the configuration set. If the value is @false@ , Amazon SES will not send emails that use the configuration set. The default value is @true@ . You can change this setting using 'UpdateConfigurationSetSendingEnabled' .
reputationOptions
    :: ReputationOptions
reputationOptions =
  ReputationOptions'
    { _roLastFreshStart = Nothing
    , _roReputationMetricsEnabled = Nothing
    , _roSendingEnabled = Nothing
    }


-- | The date and time at which the reputation metrics for the configuration set were last reset. Resetting these metrics is known as a /fresh start/ . When you disable email sending for a configuration set using 'UpdateConfigurationSetSendingEnabled' and later re-enable it, the reputation metrics for the configuration set (but not for the entire Amazon SES account) are reset. If email sending for the configuration set has never been disabled and later re-enabled, the value of this attribute is @null@ .
roLastFreshStart :: Lens' ReputationOptions (Maybe UTCTime)
roLastFreshStart = lens _roLastFreshStart (\ s a -> s{_roLastFreshStart = a}) . mapping _Time

-- | Describes whether or not Amazon SES publishes reputation metrics for the configuration set, such as bounce and complaint rates, to Amazon CloudWatch. If the value is @true@ , reputation metrics are published. If the value is @false@ , reputation metrics are not published. The default value is @false@ .
roReputationMetricsEnabled :: Lens' ReputationOptions (Maybe Bool)
roReputationMetricsEnabled = lens _roReputationMetricsEnabled (\ s a -> s{_roReputationMetricsEnabled = a})

-- | Describes whether email sending is enabled or disabled for the configuration set. If the value is @true@ , then Amazon SES will send emails that use the configuration set. If the value is @false@ , Amazon SES will not send emails that use the configuration set. The default value is @true@ . You can change this setting using 'UpdateConfigurationSetSendingEnabled' .
roSendingEnabled :: Lens' ReputationOptions (Maybe Bool)
roSendingEnabled = lens _roSendingEnabled (\ s a -> s{_roSendingEnabled = a})

instance FromXML ReputationOptions where
        parseXML x
          = ReputationOptions' <$>
              (x .@? "LastFreshStart") <*>
                (x .@? "ReputationMetricsEnabled")
                <*> (x .@? "SendingEnabled")

instance Hashable ReputationOptions where

instance NFData ReputationOptions where

-- | When included in a receipt rule, this action saves the received message to an Amazon Simple Storage Service (Amazon S3) bucket and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
--
-- To enable Amazon SES to write emails to your Amazon S3 bucket, use an AWS KMS key to encrypt your emails, or publish to an Amazon SNS topic of another account, Amazon SES must have permission to access those resources. For information about giving permissions, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
--
-- For information about specifying Amazon S3 actions in receipt rules, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-s3.html Amazon SES Developer Guide> .
--
--
-- /See:/ 's3Action' smart constructor.
data S3Action = S3Action'
  { _s3KMSKeyARN       :: !(Maybe Text)
  , _s3TopicARN        :: !(Maybe Text)
  , _s3ObjectKeyPrefix :: !(Maybe Text)
  , _s3BucketName      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Action' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 's3KMSKeyARN' - The customer master key that Amazon SES should use to encrypt your emails before saving them to the Amazon S3 bucket. You can use the default master key or a custom master key you created in AWS KMS as follows:     * To use the default master key, provide an ARN in the form of @arn:aws:kms:REGION:ACCOUNT-ID-WITHOUT-HYPHENS:alias/aws/ses@ . For example, if your AWS account ID is 123456789012 and you want to use the default master key in the US West (Oregon) region, the ARN of the default master key would be @arn:aws:kms:us-west-2:123456789012:alias/aws/ses@ . If you use the default master key, you don't need to perform any extra steps to give Amazon SES permission to use the key.     * To use a custom master key you created in AWS KMS, provide the ARN of the master key and ensure that you add a statement to your key's policy to give Amazon SES permission to use it. For more information about giving permissions, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> . For more information about key policies, see the <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html AWS KMS Developer Guide> . If you do not specify a master key, Amazon SES will not encrypt your emails. /Important:/ Your mail is encrypted by Amazon SES using the Amazon S3 encryption client before the mail is submitted to Amazon S3 for storage. It is not encrypted using Amazon S3 server-side encryption. This means that you must use the Amazon S3 encryption client to decrypt the email after retrieving it from Amazon S3, as the service has no access to use your AWS KMS keys for decryption. This encryption client is currently available with the <http://aws.amazon.com/sdk-for-java/ AWS Java SDK> and <http://aws.amazon.com/sdk-for-ruby/ AWS Ruby SDK> only. For more information about client-side encryption using AWS KMS master keys, see the <AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 Developer Guide> .
--
-- * 's3TopicARN' - The ARN of the Amazon SNS topic to notify when the message is saved to the Amazon S3 bucket. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <http://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- * 's3ObjectKeyPrefix' - The key prefix of the Amazon S3 bucket. The key prefix is similar to a directory name that enables you to store similar data under the same directory in a bucket.
--
-- * 's3BucketName' - The name of the Amazon S3 bucket that incoming email will be saved to.
s3Action
    :: Text -- ^ 's3BucketName'
    -> S3Action
s3Action pBucketName_ =
  S3Action'
    { _s3KMSKeyARN = Nothing
    , _s3TopicARN = Nothing
    , _s3ObjectKeyPrefix = Nothing
    , _s3BucketName = pBucketName_
    }


-- | The customer master key that Amazon SES should use to encrypt your emails before saving them to the Amazon S3 bucket. You can use the default master key or a custom master key you created in AWS KMS as follows:     * To use the default master key, provide an ARN in the form of @arn:aws:kms:REGION:ACCOUNT-ID-WITHOUT-HYPHENS:alias/aws/ses@ . For example, if your AWS account ID is 123456789012 and you want to use the default master key in the US West (Oregon) region, the ARN of the default master key would be @arn:aws:kms:us-west-2:123456789012:alias/aws/ses@ . If you use the default master key, you don't need to perform any extra steps to give Amazon SES permission to use the key.     * To use a custom master key you created in AWS KMS, provide the ARN of the master key and ensure that you add a statement to your key's policy to give Amazon SES permission to use it. For more information about giving permissions, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> . For more information about key policies, see the <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html AWS KMS Developer Guide> . If you do not specify a master key, Amazon SES will not encrypt your emails. /Important:/ Your mail is encrypted by Amazon SES using the Amazon S3 encryption client before the mail is submitted to Amazon S3 for storage. It is not encrypted using Amazon S3 server-side encryption. This means that you must use the Amazon S3 encryption client to decrypt the email after retrieving it from Amazon S3, as the service has no access to use your AWS KMS keys for decryption. This encryption client is currently available with the <http://aws.amazon.com/sdk-for-java/ AWS Java SDK> and <http://aws.amazon.com/sdk-for-ruby/ AWS Ruby SDK> only. For more information about client-side encryption using AWS KMS master keys, see the <AmazonS3/latest/dev/UsingClientSideEncryption.html Amazon S3 Developer Guide> .
s3KMSKeyARN :: Lens' S3Action (Maybe Text)
s3KMSKeyARN = lens _s3KMSKeyARN (\ s a -> s{_s3KMSKeyARN = a})

-- | The ARN of the Amazon SNS topic to notify when the message is saved to the Amazon S3 bucket. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <http://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
s3TopicARN :: Lens' S3Action (Maybe Text)
s3TopicARN = lens _s3TopicARN (\ s a -> s{_s3TopicARN = a})

-- | The key prefix of the Amazon S3 bucket. The key prefix is similar to a directory name that enables you to store similar data under the same directory in a bucket.
s3ObjectKeyPrefix :: Lens' S3Action (Maybe Text)
s3ObjectKeyPrefix = lens _s3ObjectKeyPrefix (\ s a -> s{_s3ObjectKeyPrefix = a})

-- | The name of the Amazon S3 bucket that incoming email will be saved to.
s3BucketName :: Lens' S3Action Text
s3BucketName = lens _s3BucketName (\ s a -> s{_s3BucketName = a})

instance FromXML S3Action where
        parseXML x
          = S3Action' <$>
              (x .@? "KmsKeyArn") <*> (x .@? "TopicArn") <*>
                (x .@? "ObjectKeyPrefix")
                <*> (x .@ "BucketName")

instance Hashable S3Action where

instance NFData S3Action where

instance ToQuery S3Action where
        toQuery S3Action'{..}
          = mconcat
              ["KmsKeyArn" =: _s3KMSKeyARN,
               "TopicArn" =: _s3TopicARN,
               "ObjectKeyPrefix" =: _s3ObjectKeyPrefix,
               "BucketName" =: _s3BucketName]

-- | When included in a receipt rule, this action publishes a notification to Amazon Simple Notification Service (Amazon SNS). This action includes a complete copy of the email content in the Amazon SNS notifications. Amazon SNS notifications for all other actions simply provide information about the email. They do not include the email content itself.
--
--
-- If you own the Amazon SNS topic, you don't need to do anything to give Amazon SES permission to publish emails to it. However, if you don't own the Amazon SNS topic, you need to attach a policy to the topic to give Amazon SES permissions to access it. For information about giving permissions, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
--
-- /Important:/ You can only publish emails that are 150 KB or less (including the header) to Amazon SNS. Larger emails will bounce. If you anticipate emails larger than 150 KB, use the S3 action instead.
--
-- For information about using a receipt rule to publish an Amazon SNS notification, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-sns.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'snsAction' smart constructor.
data SNSAction = SNSAction'
  { _saEncoding :: !(Maybe SNSActionEncoding)
  , _saTopicARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SNSAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saEncoding' - The encoding to use for the email within the Amazon SNS notification. UTF-8 is easier to use, but may not preserve all special characters when a message was encoded with a different encoding format. Base64 preserves all special characters. The default value is UTF-8.
--
-- * 'saTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <http://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
snsAction
    :: Text -- ^ 'saTopicARN'
    -> SNSAction
snsAction pTopicARN_ =
  SNSAction' {_saEncoding = Nothing, _saTopicARN = pTopicARN_}


-- | The encoding to use for the email within the Amazon SNS notification. UTF-8 is easier to use, but may not preserve all special characters when a message was encoded with a different encoding format. Base64 preserves all special characters. The default value is UTF-8.
saEncoding :: Lens' SNSAction (Maybe SNSActionEncoding)
saEncoding = lens _saEncoding (\ s a -> s{_saEncoding = a})

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <http://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
saTopicARN :: Lens' SNSAction Text
saTopicARN = lens _saTopicARN (\ s a -> s{_saTopicARN = a})

instance FromXML SNSAction where
        parseXML x
          = SNSAction' <$>
              (x .@? "Encoding") <*> (x .@ "TopicArn")

instance Hashable SNSAction where

instance NFData SNSAction where

instance ToQuery SNSAction where
        toQuery SNSAction'{..}
          = mconcat
              ["Encoding" =: _saEncoding,
               "TopicArn" =: _saTopicARN]

-- | Contains the topic ARN associated with an Amazon Simple Notification Service (Amazon SNS) event destination.
--
--
-- Event destinations, such as Amazon SNS, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'snsDestination' smart constructor.
newtype SNSDestination = SNSDestination'
  { _sdTopicARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SNSDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdTopicARN' - The ARN of the Amazon SNS topic that email sending events will be published to. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <http://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
snsDestination
    :: Text -- ^ 'sdTopicARN'
    -> SNSDestination
snsDestination pTopicARN_ = SNSDestination' {_sdTopicARN = pTopicARN_}


-- | The ARN of the Amazon SNS topic that email sending events will be published to. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <http://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
sdTopicARN :: Lens' SNSDestination Text
sdTopicARN = lens _sdTopicARN (\ s a -> s{_sdTopicARN = a})

instance FromXML SNSDestination where
        parseXML x = SNSDestination' <$> (x .@ "TopicARN")

instance Hashable SNSDestination where

instance NFData SNSDestination where

instance ToQuery SNSDestination where
        toQuery SNSDestination'{..}
          = mconcat ["TopicARN" =: _sdTopicARN]

-- | Represents sending statistics data. Each @SendDataPoint@ contains statistics for a 15-minute period of sending activity.
--
--
--
-- /See:/ 'sendDataPoint' smart constructor.
data SendDataPoint = SendDataPoint'
  { _sdpRejects          :: !(Maybe Integer)
  , _sdpComplaints       :: !(Maybe Integer)
  , _sdpDeliveryAttempts :: !(Maybe Integer)
  , _sdpBounces          :: !(Maybe Integer)
  , _sdpTimestamp        :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendDataPoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdpRejects' - Number of emails rejected by Amazon SES.
--
-- * 'sdpComplaints' - Number of unwanted emails that were rejected by recipients.
--
-- * 'sdpDeliveryAttempts' - Number of emails that have been sent.
--
-- * 'sdpBounces' - Number of emails that have bounced.
--
-- * 'sdpTimestamp' - Time of the data point.
sendDataPoint
    :: SendDataPoint
sendDataPoint =
  SendDataPoint'
    { _sdpRejects = Nothing
    , _sdpComplaints = Nothing
    , _sdpDeliveryAttempts = Nothing
    , _sdpBounces = Nothing
    , _sdpTimestamp = Nothing
    }


-- | Number of emails rejected by Amazon SES.
sdpRejects :: Lens' SendDataPoint (Maybe Integer)
sdpRejects = lens _sdpRejects (\ s a -> s{_sdpRejects = a})

-- | Number of unwanted emails that were rejected by recipients.
sdpComplaints :: Lens' SendDataPoint (Maybe Integer)
sdpComplaints = lens _sdpComplaints (\ s a -> s{_sdpComplaints = a})

-- | Number of emails that have been sent.
sdpDeliveryAttempts :: Lens' SendDataPoint (Maybe Integer)
sdpDeliveryAttempts = lens _sdpDeliveryAttempts (\ s a -> s{_sdpDeliveryAttempts = a})

-- | Number of emails that have bounced.
sdpBounces :: Lens' SendDataPoint (Maybe Integer)
sdpBounces = lens _sdpBounces (\ s a -> s{_sdpBounces = a})

-- | Time of the data point.
sdpTimestamp :: Lens' SendDataPoint (Maybe UTCTime)
sdpTimestamp = lens _sdpTimestamp (\ s a -> s{_sdpTimestamp = a}) . mapping _Time

instance FromXML SendDataPoint where
        parseXML x
          = SendDataPoint' <$>
              (x .@? "Rejects") <*> (x .@? "Complaints") <*>
                (x .@? "DeliveryAttempts")
                <*> (x .@? "Bounces")
                <*> (x .@? "Timestamp")

instance Hashable SendDataPoint where

instance NFData SendDataPoint where

-- | When included in a receipt rule, this action terminates the evaluation of the receipt rule set and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
--
-- For information about setting a stop action in a receipt rule, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-stop.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'stopAction' smart constructor.
data StopAction = StopAction'
  { _sTopicARN :: !(Maybe Text)
  , _sScope    :: !StopScope
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the stop action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <http://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- * 'sScope' - The name of the RuleSet that is being stopped.
stopAction
    :: StopScope -- ^ 'sScope'
    -> StopAction
stopAction pScope_ = StopAction' {_sTopicARN = Nothing, _sScope = pScope_}


-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the stop action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <http://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
sTopicARN :: Lens' StopAction (Maybe Text)
sTopicARN = lens _sTopicARN (\ s a -> s{_sTopicARN = a})

-- | The name of the RuleSet that is being stopped.
sScope :: Lens' StopAction StopScope
sScope = lens _sScope (\ s a -> s{_sScope = a})

instance FromXML StopAction where
        parseXML x
          = StopAction' <$>
              (x .@? "TopicArn") <*> (x .@ "Scope")

instance Hashable StopAction where

instance NFData StopAction where

instance ToQuery StopAction where
        toQuery StopAction'{..}
          = mconcat
              ["TopicArn" =: _sTopicARN, "Scope" =: _sScope]

-- | The content of the email, composed of a subject line, an HTML part, and a text-only part.
--
--
--
-- /See:/ 'template' smart constructor.
data Template = Template'
  { _tTextPart     :: !(Maybe Text)
  , _tSubjectPart  :: !(Maybe Text)
  , _tHTMLPart     :: !(Maybe Text)
  , _tTemplateName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Template' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTextPart' - The email body that will be visible to recipients whose email clients do not display HTML.
--
-- * 'tSubjectPart' - The subject line of the email.
--
-- * 'tHTMLPart' - The HTML body of the email.
--
-- * 'tTemplateName' - The name of the template. You will refer to this name when you send email using the @SendTemplatedEmail@ or @SendBulkTemplatedEmail@ operations.
template
    :: Text -- ^ 'tTemplateName'
    -> Template
template pTemplateName_ =
  Template'
    { _tTextPart = Nothing
    , _tSubjectPart = Nothing
    , _tHTMLPart = Nothing
    , _tTemplateName = pTemplateName_
    }


-- | The email body that will be visible to recipients whose email clients do not display HTML.
tTextPart :: Lens' Template (Maybe Text)
tTextPart = lens _tTextPart (\ s a -> s{_tTextPart = a})

-- | The subject line of the email.
tSubjectPart :: Lens' Template (Maybe Text)
tSubjectPart = lens _tSubjectPart (\ s a -> s{_tSubjectPart = a})

-- | The HTML body of the email.
tHTMLPart :: Lens' Template (Maybe Text)
tHTMLPart = lens _tHTMLPart (\ s a -> s{_tHTMLPart = a})

-- | The name of the template. You will refer to this name when you send email using the @SendTemplatedEmail@ or @SendBulkTemplatedEmail@ operations.
tTemplateName :: Lens' Template Text
tTemplateName = lens _tTemplateName (\ s a -> s{_tTemplateName = a})

instance FromXML Template where
        parseXML x
          = Template' <$>
              (x .@? "TextPart") <*> (x .@? "SubjectPart") <*>
                (x .@? "HtmlPart")
                <*> (x .@ "TemplateName")

instance Hashable Template where

instance NFData Template where

instance ToQuery Template where
        toQuery Template'{..}
          = mconcat
              ["TextPart" =: _tTextPart,
               "SubjectPart" =: _tSubjectPart,
               "HtmlPart" =: _tHTMLPart,
               "TemplateName" =: _tTemplateName]

-- | Contains information about an email template.
--
--
--
-- /See:/ 'templateMetadata' smart constructor.
data TemplateMetadata = TemplateMetadata'
  { _tmName             :: !(Maybe Text)
  , _tmCreatedTimestamp :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TemplateMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmName' - The name of the template.
--
-- * 'tmCreatedTimestamp' - The time and date the template was created.
templateMetadata
    :: TemplateMetadata
templateMetadata =
  TemplateMetadata' {_tmName = Nothing, _tmCreatedTimestamp = Nothing}


-- | The name of the template.
tmName :: Lens' TemplateMetadata (Maybe Text)
tmName = lens _tmName (\ s a -> s{_tmName = a})

-- | The time and date the template was created.
tmCreatedTimestamp :: Lens' TemplateMetadata (Maybe UTCTime)
tmCreatedTimestamp = lens _tmCreatedTimestamp (\ s a -> s{_tmCreatedTimestamp = a}) . mapping _Time

instance FromXML TemplateMetadata where
        parseXML x
          = TemplateMetadata' <$>
              (x .@? "Name") <*> (x .@? "CreatedTimestamp")

instance Hashable TemplateMetadata where

instance NFData TemplateMetadata where

-- | A domain that is used to redirect email recipients to an Amazon SES-operated domain. This domain captures open and click events generated by Amazon SES emails.
--
--
-- For more information, see <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html Configuring Custom Domains to Handle Open and Click Tracking> in the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'trackingOptions' smart constructor.
newtype TrackingOptions = TrackingOptions'
  { _toCustomRedirectDomain :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TrackingOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'toCustomRedirectDomain' - The custom subdomain that will be used to redirect email recipients to the Amazon SES event tracking domain.
trackingOptions
    :: TrackingOptions
trackingOptions = TrackingOptions' {_toCustomRedirectDomain = Nothing}


-- | The custom subdomain that will be used to redirect email recipients to the Amazon SES event tracking domain.
toCustomRedirectDomain :: Lens' TrackingOptions (Maybe Text)
toCustomRedirectDomain = lens _toCustomRedirectDomain (\ s a -> s{_toCustomRedirectDomain = a})

instance FromXML TrackingOptions where
        parseXML x
          = TrackingOptions' <$> (x .@? "CustomRedirectDomain")

instance Hashable TrackingOptions where

instance NFData TrackingOptions where

instance ToQuery TrackingOptions where
        toQuery TrackingOptions'{..}
          = mconcat
              ["CustomRedirectDomain" =: _toCustomRedirectDomain]

-- | When included in a receipt rule, this action calls Amazon WorkMail and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS). You will typically not use this action directly because Amazon WorkMail adds the rule automatically during its setup procedure.
--
--
-- For information using a receipt rule to call Amazon WorkMail, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-workmail.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'workmailAction' smart constructor.
data WorkmailAction = WorkmailAction'
  { _waTopicARN        :: !(Maybe Text)
  , _waOrganizationARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WorkmailAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'waTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the WorkMail action is called. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <http://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- * 'waOrganizationARN' - The ARN of the Amazon WorkMail organization. An example of an Amazon WorkMail organization ARN is @arn:aws:workmail:us-west-2:123456789012:organization/m-68755160c4cb4e29a2b2f8fb58f359d7@ . For information about Amazon WorkMail organizations, see the <http://docs.aws.amazon.com/workmail/latest/adminguide/organizations_overview.html Amazon WorkMail Administrator Guide> .
workmailAction
    :: Text -- ^ 'waOrganizationARN'
    -> WorkmailAction
workmailAction pOrganizationARN_ =
  WorkmailAction'
    {_waTopicARN = Nothing, _waOrganizationARN = pOrganizationARN_}


-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the WorkMail action is called. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <http://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
waTopicARN :: Lens' WorkmailAction (Maybe Text)
waTopicARN = lens _waTopicARN (\ s a -> s{_waTopicARN = a})

-- | The ARN of the Amazon WorkMail organization. An example of an Amazon WorkMail organization ARN is @arn:aws:workmail:us-west-2:123456789012:organization/m-68755160c4cb4e29a2b2f8fb58f359d7@ . For information about Amazon WorkMail organizations, see the <http://docs.aws.amazon.com/workmail/latest/adminguide/organizations_overview.html Amazon WorkMail Administrator Guide> .
waOrganizationARN :: Lens' WorkmailAction Text
waOrganizationARN = lens _waOrganizationARN (\ s a -> s{_waOrganizationARN = a})

instance FromXML WorkmailAction where
        parseXML x
          = WorkmailAction' <$>
              (x .@? "TopicArn") <*> (x .@ "OrganizationArn")

instance Hashable WorkmailAction where

instance NFData WorkmailAction where

instance ToQuery WorkmailAction where
        toQuery WorkmailAction'{..}
          = mconcat
              ["TopicArn" =: _waTopicARN,
               "OrganizationArn" =: _waOrganizationARN]
