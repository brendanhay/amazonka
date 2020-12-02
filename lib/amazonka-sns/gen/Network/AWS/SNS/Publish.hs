{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Publish
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a message to an Amazon SNS topic, a text message (SMS message) directly to a phone number, or a message to a mobile platform endpoint (when you specify the @TargetArn@ ).
--
--
-- If you send a message to a topic, Amazon SNS delivers the message to each endpoint that is subscribed to the topic. The format of the message depends on the notification protocol for each subscribed endpoint.
--
-- When a @messageId@ is returned, the message has been saved and Amazon SNS will attempt to deliver it shortly.
--
-- To use the @Publish@ action for sending a message to a mobile endpoint, such as an app on a Kindle device or mobile phone, you must specify the EndpointArn for the TargetArn parameter. The EndpointArn is returned when making a call with the @CreatePlatformEndpoint@ action.
--
-- For more information about formatting messages, see <https://docs.aws.amazon.com/sns/latest/dg/mobile-push-send-custommessage.html Send Custom Platform-Specific Payloads in Messages to Mobile Devices> .
--
-- /Important:/ You can publish messages only to topics and endpoints in the same AWS Region.
module Network.AWS.SNS.Publish
  ( -- * Creating a Request
    publish,
    Publish,

    -- * Request Lenses
    pSubject,
    pTargetARN,
    pMessageAttributes,
    pTopicARN,
    pPhoneNumber,
    pMessageDeduplicationId,
    pMessageStructure,
    pMessageGroupId,
    pMessage,

    -- * Destructuring the Response
    publishResponse,
    PublishResponse,

    -- * Response Lenses
    prsSequenceNumber,
    prsMessageId,
    prsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types

-- | Input for Publish action.
--
--
--
-- /See:/ 'publish' smart constructor.
data Publish = Publish'
  { _pSubject :: !(Maybe Text),
    _pTargetARN :: !(Maybe Text),
    _pMessageAttributes :: !(Maybe (Map Text (MessageAttributeValue))),
    _pTopicARN :: !(Maybe Text),
    _pPhoneNumber :: !(Maybe Text),
    _pMessageDeduplicationId :: !(Maybe Text),
    _pMessageStructure :: !(Maybe Text),
    _pMessageGroupId :: !(Maybe Text),
    _pMessage :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Publish' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pSubject' - Optional parameter to be used as the "Subject" line when the message is delivered to email endpoints. This field will also be included, if present, in the standard JSON messages delivered to other endpoints. Constraints: Subjects must be ASCII text that begins with a letter, number, or punctuation mark; must not include line breaks or control characters; and must be less than 100 characters long.
--
-- * 'pTargetARN' - If you don't specify a value for the @TargetArn@ parameter, you must specify a value for the @PhoneNumber@ or @TopicArn@ parameters.
--
-- * 'pMessageAttributes' - Message attributes for Publish action.
--
-- * 'pTopicARN' - The topic you want to publish to. If you don't specify a value for the @TopicArn@ parameter, you must specify a value for the @PhoneNumber@ or @TargetArn@ parameters.
--
-- * 'pPhoneNumber' - The phone number to which you want to deliver an SMS message. Use E.164 format. If you don't specify a value for the @PhoneNumber@ parameter, you must specify a value for the @TargetArn@ or @TopicArn@ parameters.
--
-- * 'pMessageDeduplicationId' - This parameter applies only to FIFO (first-in-first-out) topics. The @MessageDeduplicationId@ can contain up to 128 alphanumeric characters (a-z, A-Z, 0-9) and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ . Every message must have a unique @MessageDeduplicationId@ , which is a token used for deduplication of sent messages. If a message with a particular @MessageDeduplicationId@ is sent successfully, any message sent with the same @MessageDeduplicationId@ during the 5-minute deduplication interval is treated as a duplicate.  If the topic has @ContentBasedDeduplication@ set, the system generates a @MessageDeduplicationId@ based on the contents of the message. Your @MessageDeduplicationId@ overrides the generated one.
--
-- * 'pMessageStructure' - Set @MessageStructure@ to @json@ if you want to send a different message for each protocol. For example, using one publish action, you can send a short message to your SMS subscribers and a longer message to your email subscribers. If you set @MessageStructure@ to @json@ , the value of the @Message@ parameter must:      * be a syntactically valid JSON object; and     * contain at least a top-level JSON key of "default" with a value that is a string. You can define other top-level keys that define the message you want to send to a specific transport protocol (e.g., "http"). Valid value: @json@
--
-- * 'pMessageGroupId' - This parameter applies only to FIFO (first-in-first-out) topics. The @MessageGroupId@ can contain up to 128 alphanumeric characters (a-z, A-Z, 0-9) and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ . The @MessageGroupId@ is a tag that specifies that a message belongs to a specific message group. Messages that belong to the same message group are processed in a FIFO manner (however, messages in different message groups might be processed out of order). Every message must include a @MessageGroupId@ .
--
-- * 'pMessage' - The message you want to send. If you are publishing to a topic and you want to send the same message to all transport protocols, include the text of the message as a String value. If you want to send different messages for each transport protocol, set the value of the @MessageStructure@ parameter to @json@ and use a JSON object for the @Message@ parameter.  Constraints:     * With the exception of SMS, messages must be UTF-8 encoded strings and at most 256 KB in size (262,144 bytes, not 262,144 characters).     * For SMS, each message can contain up to 140 characters. This character limit depends on the encoding schema. For example, an SMS message can contain 160 GSM characters, 140 ASCII characters, or 70 UCS-2 characters. If you publish a message that exceeds this size limit, Amazon SNS sends the message as multiple messages, each fitting within the size limit. Messages aren't truncated mid-word but are cut off at whole-word boundaries. The total size limit for a single SMS @Publish@ action is 1,600 characters. JSON-specific constraints:     * Keys in the JSON object that correspond to supported transport protocols must have simple JSON string values.     * The values will be parsed (unescaped) before they are used in outgoing messages.     * Outbound notifications are JSON encoded (meaning that the characters will be reescaped for sending).     * Values have a minimum length of 0 (the empty string, "", is allowed).     * Values have a maximum length bounded by the overall message size (so, including multiple protocols may limit message sizes).     * Non-string values will cause the key to be ignored.     * Keys that do not correspond to supported transport protocols are ignored.     * Duplicate keys are not allowed.     * Failure to parse or validate any key or value in the message will cause the @Publish@ call to return an error (no partial delivery).
publish ::
  -- | 'pMessage'
  Text ->
  Publish
publish pMessage_ =
  Publish'
    { _pSubject = Nothing,
      _pTargetARN = Nothing,
      _pMessageAttributes = Nothing,
      _pTopicARN = Nothing,
      _pPhoneNumber = Nothing,
      _pMessageDeduplicationId = Nothing,
      _pMessageStructure = Nothing,
      _pMessageGroupId = Nothing,
      _pMessage = pMessage_
    }

-- | Optional parameter to be used as the "Subject" line when the message is delivered to email endpoints. This field will also be included, if present, in the standard JSON messages delivered to other endpoints. Constraints: Subjects must be ASCII text that begins with a letter, number, or punctuation mark; must not include line breaks or control characters; and must be less than 100 characters long.
pSubject :: Lens' Publish (Maybe Text)
pSubject = lens _pSubject (\s a -> s {_pSubject = a})

-- | If you don't specify a value for the @TargetArn@ parameter, you must specify a value for the @PhoneNumber@ or @TopicArn@ parameters.
pTargetARN :: Lens' Publish (Maybe Text)
pTargetARN = lens _pTargetARN (\s a -> s {_pTargetARN = a})

-- | Message attributes for Publish action.
pMessageAttributes :: Lens' Publish (HashMap Text (MessageAttributeValue))
pMessageAttributes = lens _pMessageAttributes (\s a -> s {_pMessageAttributes = a}) . _Default . _Map

-- | The topic you want to publish to. If you don't specify a value for the @TopicArn@ parameter, you must specify a value for the @PhoneNumber@ or @TargetArn@ parameters.
pTopicARN :: Lens' Publish (Maybe Text)
pTopicARN = lens _pTopicARN (\s a -> s {_pTopicARN = a})

-- | The phone number to which you want to deliver an SMS message. Use E.164 format. If you don't specify a value for the @PhoneNumber@ parameter, you must specify a value for the @TargetArn@ or @TopicArn@ parameters.
pPhoneNumber :: Lens' Publish (Maybe Text)
pPhoneNumber = lens _pPhoneNumber (\s a -> s {_pPhoneNumber = a})

-- | This parameter applies only to FIFO (first-in-first-out) topics. The @MessageDeduplicationId@ can contain up to 128 alphanumeric characters (a-z, A-Z, 0-9) and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ . Every message must have a unique @MessageDeduplicationId@ , which is a token used for deduplication of sent messages. If a message with a particular @MessageDeduplicationId@ is sent successfully, any message sent with the same @MessageDeduplicationId@ during the 5-minute deduplication interval is treated as a duplicate.  If the topic has @ContentBasedDeduplication@ set, the system generates a @MessageDeduplicationId@ based on the contents of the message. Your @MessageDeduplicationId@ overrides the generated one.
pMessageDeduplicationId :: Lens' Publish (Maybe Text)
pMessageDeduplicationId = lens _pMessageDeduplicationId (\s a -> s {_pMessageDeduplicationId = a})

-- | Set @MessageStructure@ to @json@ if you want to send a different message for each protocol. For example, using one publish action, you can send a short message to your SMS subscribers and a longer message to your email subscribers. If you set @MessageStructure@ to @json@ , the value of the @Message@ parameter must:      * be a syntactically valid JSON object; and     * contain at least a top-level JSON key of "default" with a value that is a string. You can define other top-level keys that define the message you want to send to a specific transport protocol (e.g., "http"). Valid value: @json@
pMessageStructure :: Lens' Publish (Maybe Text)
pMessageStructure = lens _pMessageStructure (\s a -> s {_pMessageStructure = a})

-- | This parameter applies only to FIFO (first-in-first-out) topics. The @MessageGroupId@ can contain up to 128 alphanumeric characters (a-z, A-Z, 0-9) and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ . The @MessageGroupId@ is a tag that specifies that a message belongs to a specific message group. Messages that belong to the same message group are processed in a FIFO manner (however, messages in different message groups might be processed out of order). Every message must include a @MessageGroupId@ .
pMessageGroupId :: Lens' Publish (Maybe Text)
pMessageGroupId = lens _pMessageGroupId (\s a -> s {_pMessageGroupId = a})

-- | The message you want to send. If you are publishing to a topic and you want to send the same message to all transport protocols, include the text of the message as a String value. If you want to send different messages for each transport protocol, set the value of the @MessageStructure@ parameter to @json@ and use a JSON object for the @Message@ parameter.  Constraints:     * With the exception of SMS, messages must be UTF-8 encoded strings and at most 256 KB in size (262,144 bytes, not 262,144 characters).     * For SMS, each message can contain up to 140 characters. This character limit depends on the encoding schema. For example, an SMS message can contain 160 GSM characters, 140 ASCII characters, or 70 UCS-2 characters. If you publish a message that exceeds this size limit, Amazon SNS sends the message as multiple messages, each fitting within the size limit. Messages aren't truncated mid-word but are cut off at whole-word boundaries. The total size limit for a single SMS @Publish@ action is 1,600 characters. JSON-specific constraints:     * Keys in the JSON object that correspond to supported transport protocols must have simple JSON string values.     * The values will be parsed (unescaped) before they are used in outgoing messages.     * Outbound notifications are JSON encoded (meaning that the characters will be reescaped for sending).     * Values have a minimum length of 0 (the empty string, "", is allowed).     * Values have a maximum length bounded by the overall message size (so, including multiple protocols may limit message sizes).     * Non-string values will cause the key to be ignored.     * Keys that do not correspond to supported transport protocols are ignored.     * Duplicate keys are not allowed.     * Failure to parse or validate any key or value in the message will cause the @Publish@ call to return an error (no partial delivery).
pMessage :: Lens' Publish Text
pMessage = lens _pMessage (\s a -> s {_pMessage = a})

instance AWSRequest Publish where
  type Rs Publish = PublishResponse
  request = postQuery sns
  response =
    receiveXMLWrapper
      "PublishResult"
      ( \s h x ->
          PublishResponse'
            <$> (x .@? "SequenceNumber")
            <*> (x .@? "MessageId")
            <*> (pure (fromEnum s))
      )

instance Hashable Publish

instance NFData Publish

instance ToHeaders Publish where
  toHeaders = const mempty

instance ToPath Publish where
  toPath = const "/"

instance ToQuery Publish where
  toQuery Publish' {..} =
    mconcat
      [ "Action" =: ("Publish" :: ByteString),
        "Version" =: ("2010-03-31" :: ByteString),
        "Subject" =: _pSubject,
        "TargetArn" =: _pTargetARN,
        "MessageAttributes"
          =: toQuery
            (toQueryMap "entry" "Name" "Value" <$> _pMessageAttributes),
        "TopicArn" =: _pTopicARN,
        "PhoneNumber" =: _pPhoneNumber,
        "MessageDeduplicationId" =: _pMessageDeduplicationId,
        "MessageStructure" =: _pMessageStructure,
        "MessageGroupId" =: _pMessageGroupId,
        "Message" =: _pMessage
      ]

-- | Response for Publish action.
--
--
--
-- /See:/ 'publishResponse' smart constructor.
data PublishResponse = PublishResponse'
  { _prsSequenceNumber ::
      !(Maybe Text),
    _prsMessageId :: !(Maybe Text),
    _prsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PublishResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prsSequenceNumber' - This response element applies only to FIFO (first-in-first-out) topics.  The sequence number is a large, non-consecutive number that Amazon SNS assigns to each message. The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues to increase for each @MessageGroupId@ .
--
-- * 'prsMessageId' - Unique identifier assigned to the published message. Length Constraint: Maximum 100 characters
--
-- * 'prsResponseStatus' - -- | The response status code.
publishResponse ::
  -- | 'prsResponseStatus'
  Int ->
  PublishResponse
publishResponse pResponseStatus_ =
  PublishResponse'
    { _prsSequenceNumber = Nothing,
      _prsMessageId = Nothing,
      _prsResponseStatus = pResponseStatus_
    }

-- | This response element applies only to FIFO (first-in-first-out) topics.  The sequence number is a large, non-consecutive number that Amazon SNS assigns to each message. The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues to increase for each @MessageGroupId@ .
prsSequenceNumber :: Lens' PublishResponse (Maybe Text)
prsSequenceNumber = lens _prsSequenceNumber (\s a -> s {_prsSequenceNumber = a})

-- | Unique identifier assigned to the published message. Length Constraint: Maximum 100 characters
prsMessageId :: Lens' PublishResponse (Maybe Text)
prsMessageId = lens _prsMessageId (\s a -> s {_prsMessageId = a})

-- | -- | The response status code.
prsResponseStatus :: Lens' PublishResponse Int
prsResponseStatus = lens _prsResponseStatus (\s a -> s {_prsResponseStatus = a})

instance NFData PublishResponse
