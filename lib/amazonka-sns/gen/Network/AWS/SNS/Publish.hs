{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- If you send a message to a topic, Amazon SNS delivers the message to each endpoint that is subscribed to the topic. The format of the message depends on the notification protocol for each subscribed endpoint.
-- When a @messageId@ is returned, the message has been saved and Amazon SNS will attempt to deliver it shortly.
-- To use the @Publish@ action for sending a message to a mobile endpoint, such as an app on a Kindle device or mobile phone, you must specify the EndpointArn for the TargetArn parameter. The EndpointArn is returned when making a call with the @CreatePlatformEndpoint@ action.
-- For more information about formatting messages, see <https://docs.aws.amazon.com/sns/latest/dg/mobile-push-send-custommessage.html Send Custom Platform-Specific Payloads in Messages to Mobile Devices> .
-- /Important:/ You can publish messages only to topics and endpoints in the same AWS Region.
module Network.AWS.SNS.Publish
  ( -- * Creating a request
    Publish (..),
    mkPublish,

    -- ** Request lenses
    pSubject,
    pTargetARN,
    pMessageAttributes,
    pTopicARN,
    pPhoneNumber,
    pMessageDeduplicationId,
    pMessageStructure,
    pMessageGroupId,
    pMessage,

    -- * Destructuring the response
    PublishResponse (..),
    mkPublishResponse,

    -- ** Response lenses
    prsSequenceNumber,
    prsMessageId,
    prsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for Publish action.
--
-- /See:/ 'mkPublish' smart constructor.
data Publish = Publish'
  { subject :: Lude.Maybe Lude.Text,
    targetARN :: Lude.Maybe Lude.Text,
    messageAttributes ::
      Lude.Maybe (Lude.HashMap Lude.Text (MessageAttributeValue)),
    topicARN :: Lude.Maybe Lude.Text,
    phoneNumber :: Lude.Maybe Lude.Text,
    messageDeduplicationId :: Lude.Maybe Lude.Text,
    messageStructure :: Lude.Maybe Lude.Text,
    messageGroupId :: Lude.Maybe Lude.Text,
    message :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Publish' with the minimum fields required to make a request.
--
-- * 'message' - The message you want to send.
--
-- If you are publishing to a topic and you want to send the same message to all transport protocols, include the text of the message as a String value. If you want to send different messages for each transport protocol, set the value of the @MessageStructure@ parameter to @json@ and use a JSON object for the @Message@ parameter.
--
-- Constraints:
--
--     * With the exception of SMS, messages must be UTF-8 encoded strings and at most 256 KB in size (262,144 bytes, not 262,144 characters).
--
--
--     * For SMS, each message can contain up to 140 characters. This character limit depends on the encoding schema. For example, an SMS message can contain 160 GSM characters, 140 ASCII characters, or 70 UCS-2 characters.
-- If you publish a message that exceeds this size limit, Amazon SNS sends the message as multiple messages, each fitting within the size limit. Messages aren't truncated mid-word but are cut off at whole-word boundaries.
-- The total size limit for a single SMS @Publish@ action is 1,600 characters.
--
--
-- JSON-specific constraints:
--
--     * Keys in the JSON object that correspond to supported transport protocols must have simple JSON string values.
--
--
--     * The values will be parsed (unescaped) before they are used in outgoing messages.
--
--
--     * Outbound notifications are JSON encoded (meaning that the characters will be reescaped for sending).
--
--
--     * Values have a minimum length of 0 (the empty string, "", is allowed).
--
--
--     * Values have a maximum length bounded by the overall message size (so, including multiple protocols may limit message sizes).
--
--
--     * Non-string values will cause the key to be ignored.
--
--
--     * Keys that do not correspond to supported transport protocols are ignored.
--
--
--     * Duplicate keys are not allowed.
--
--
--     * Failure to parse or validate any key or value in the message will cause the @Publish@ call to return an error (no partial delivery).
--
--
-- * 'messageAttributes' - Message attributes for Publish action.
-- * 'messageDeduplicationId' - This parameter applies only to FIFO (first-in-first-out) topics. The @MessageDeduplicationId@ can contain up to 128 alphanumeric characters (a-z, A-Z, 0-9) and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ .
--
-- Every message must have a unique @MessageDeduplicationId@ , which is a token used for deduplication of sent messages. If a message with a particular @MessageDeduplicationId@ is sent successfully, any message sent with the same @MessageDeduplicationId@ during the 5-minute deduplication interval is treated as a duplicate.
-- If the topic has @ContentBasedDeduplication@ set, the system generates a @MessageDeduplicationId@ based on the contents of the message. Your @MessageDeduplicationId@ overrides the generated one.
-- * 'messageGroupId' - This parameter applies only to FIFO (first-in-first-out) topics. The @MessageGroupId@ can contain up to 128 alphanumeric characters (a-z, A-Z, 0-9) and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ .
--
-- The @MessageGroupId@ is a tag that specifies that a message belongs to a specific message group. Messages that belong to the same message group are processed in a FIFO manner (however, messages in different message groups might be processed out of order). Every message must include a @MessageGroupId@ .
-- * 'messageStructure' - Set @MessageStructure@ to @json@ if you want to send a different message for each protocol. For example, using one publish action, you can send a short message to your SMS subscribers and a longer message to your email subscribers. If you set @MessageStructure@ to @json@ , the value of the @Message@ parameter must:
--
--
--     * be a syntactically valid JSON object; and
--
--
--     * contain at least a top-level JSON key of "default" with a value that is a string.
--
--
-- You can define other top-level keys that define the message you want to send to a specific transport protocol (e.g., "http").
-- Valid value: @json@
-- * 'phoneNumber' - The phone number to which you want to deliver an SMS message. Use E.164 format.
--
-- If you don't specify a value for the @PhoneNumber@ parameter, you must specify a value for the @TargetArn@ or @TopicArn@ parameters.
-- * 'subject' - Optional parameter to be used as the "Subject" line when the message is delivered to email endpoints. This field will also be included, if present, in the standard JSON messages delivered to other endpoints.
--
-- Constraints: Subjects must be ASCII text that begins with a letter, number, or punctuation mark; must not include line breaks or control characters; and must be less than 100 characters long.
-- * 'targetARN' - If you don't specify a value for the @TargetArn@ parameter, you must specify a value for the @PhoneNumber@ or @TopicArn@ parameters.
-- * 'topicARN' - The topic you want to publish to.
--
-- If you don't specify a value for the @TopicArn@ parameter, you must specify a value for the @PhoneNumber@ or @TargetArn@ parameters.
mkPublish ::
  -- | 'message'
  Lude.Text ->
  Publish
mkPublish pMessage_ =
  Publish'
    { subject = Lude.Nothing,
      targetARN = Lude.Nothing,
      messageAttributes = Lude.Nothing,
      topicARN = Lude.Nothing,
      phoneNumber = Lude.Nothing,
      messageDeduplicationId = Lude.Nothing,
      messageStructure = Lude.Nothing,
      messageGroupId = Lude.Nothing,
      message = pMessage_
    }

-- | Optional parameter to be used as the "Subject" line when the message is delivered to email endpoints. This field will also be included, if present, in the standard JSON messages delivered to other endpoints.
--
-- Constraints: Subjects must be ASCII text that begins with a letter, number, or punctuation mark; must not include line breaks or control characters; and must be less than 100 characters long.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSubject :: Lens.Lens' Publish (Lude.Maybe Lude.Text)
pSubject = Lens.lens (subject :: Publish -> Lude.Maybe Lude.Text) (\s a -> s {subject = a} :: Publish)
{-# DEPRECATED pSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

-- | If you don't specify a value for the @TargetArn@ parameter, you must specify a value for the @PhoneNumber@ or @TopicArn@ parameters.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTargetARN :: Lens.Lens' Publish (Lude.Maybe Lude.Text)
pTargetARN = Lens.lens (targetARN :: Publish -> Lude.Maybe Lude.Text) (\s a -> s {targetARN = a} :: Publish)
{-# DEPRECATED pTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | Message attributes for Publish action.
--
-- /Note:/ Consider using 'messageAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMessageAttributes :: Lens.Lens' Publish (Lude.Maybe (Lude.HashMap Lude.Text (MessageAttributeValue)))
pMessageAttributes = Lens.lens (messageAttributes :: Publish -> Lude.Maybe (Lude.HashMap Lude.Text (MessageAttributeValue))) (\s a -> s {messageAttributes = a} :: Publish)
{-# DEPRECATED pMessageAttributes "Use generic-lens or generic-optics with 'messageAttributes' instead." #-}

-- | The topic you want to publish to.
--
-- If you don't specify a value for the @TopicArn@ parameter, you must specify a value for the @PhoneNumber@ or @TargetArn@ parameters.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTopicARN :: Lens.Lens' Publish (Lude.Maybe Lude.Text)
pTopicARN = Lens.lens (topicARN :: Publish -> Lude.Maybe Lude.Text) (\s a -> s {topicARN = a} :: Publish)
{-# DEPRECATED pTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The phone number to which you want to deliver an SMS message. Use E.164 format.
--
-- If you don't specify a value for the @PhoneNumber@ parameter, you must specify a value for the @TargetArn@ or @TopicArn@ parameters.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPhoneNumber :: Lens.Lens' Publish (Lude.Maybe Lude.Text)
pPhoneNumber = Lens.lens (phoneNumber :: Publish -> Lude.Maybe Lude.Text) (\s a -> s {phoneNumber = a} :: Publish)
{-# DEPRECATED pPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

-- | This parameter applies only to FIFO (first-in-first-out) topics. The @MessageDeduplicationId@ can contain up to 128 alphanumeric characters (a-z, A-Z, 0-9) and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ .
--
-- Every message must have a unique @MessageDeduplicationId@ , which is a token used for deduplication of sent messages. If a message with a particular @MessageDeduplicationId@ is sent successfully, any message sent with the same @MessageDeduplicationId@ during the 5-minute deduplication interval is treated as a duplicate.
-- If the topic has @ContentBasedDeduplication@ set, the system generates a @MessageDeduplicationId@ based on the contents of the message. Your @MessageDeduplicationId@ overrides the generated one.
--
-- /Note:/ Consider using 'messageDeduplicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMessageDeduplicationId :: Lens.Lens' Publish (Lude.Maybe Lude.Text)
pMessageDeduplicationId = Lens.lens (messageDeduplicationId :: Publish -> Lude.Maybe Lude.Text) (\s a -> s {messageDeduplicationId = a} :: Publish)
{-# DEPRECATED pMessageDeduplicationId "Use generic-lens or generic-optics with 'messageDeduplicationId' instead." #-}

-- | Set @MessageStructure@ to @json@ if you want to send a different message for each protocol. For example, using one publish action, you can send a short message to your SMS subscribers and a longer message to your email subscribers. If you set @MessageStructure@ to @json@ , the value of the @Message@ parameter must:
--
--
--     * be a syntactically valid JSON object; and
--
--
--     * contain at least a top-level JSON key of "default" with a value that is a string.
--
--
-- You can define other top-level keys that define the message you want to send to a specific transport protocol (e.g., "http").
-- Valid value: @json@
--
-- /Note:/ Consider using 'messageStructure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMessageStructure :: Lens.Lens' Publish (Lude.Maybe Lude.Text)
pMessageStructure = Lens.lens (messageStructure :: Publish -> Lude.Maybe Lude.Text) (\s a -> s {messageStructure = a} :: Publish)
{-# DEPRECATED pMessageStructure "Use generic-lens or generic-optics with 'messageStructure' instead." #-}

-- | This parameter applies only to FIFO (first-in-first-out) topics. The @MessageGroupId@ can contain up to 128 alphanumeric characters (a-z, A-Z, 0-9) and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ .
--
-- The @MessageGroupId@ is a tag that specifies that a message belongs to a specific message group. Messages that belong to the same message group are processed in a FIFO manner (however, messages in different message groups might be processed out of order). Every message must include a @MessageGroupId@ .
--
-- /Note:/ Consider using 'messageGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMessageGroupId :: Lens.Lens' Publish (Lude.Maybe Lude.Text)
pMessageGroupId = Lens.lens (messageGroupId :: Publish -> Lude.Maybe Lude.Text) (\s a -> s {messageGroupId = a} :: Publish)
{-# DEPRECATED pMessageGroupId "Use generic-lens or generic-optics with 'messageGroupId' instead." #-}

-- | The message you want to send.
--
-- If you are publishing to a topic and you want to send the same message to all transport protocols, include the text of the message as a String value. If you want to send different messages for each transport protocol, set the value of the @MessageStructure@ parameter to @json@ and use a JSON object for the @Message@ parameter.
--
-- Constraints:
--
--     * With the exception of SMS, messages must be UTF-8 encoded strings and at most 256 KB in size (262,144 bytes, not 262,144 characters).
--
--
--     * For SMS, each message can contain up to 140 characters. This character limit depends on the encoding schema. For example, an SMS message can contain 160 GSM characters, 140 ASCII characters, or 70 UCS-2 characters.
-- If you publish a message that exceeds this size limit, Amazon SNS sends the message as multiple messages, each fitting within the size limit. Messages aren't truncated mid-word but are cut off at whole-word boundaries.
-- The total size limit for a single SMS @Publish@ action is 1,600 characters.
--
--
-- JSON-specific constraints:
--
--     * Keys in the JSON object that correspond to supported transport protocols must have simple JSON string values.
--
--
--     * The values will be parsed (unescaped) before they are used in outgoing messages.
--
--
--     * Outbound notifications are JSON encoded (meaning that the characters will be reescaped for sending).
--
--
--     * Values have a minimum length of 0 (the empty string, "", is allowed).
--
--
--     * Values have a maximum length bounded by the overall message size (so, including multiple protocols may limit message sizes).
--
--
--     * Non-string values will cause the key to be ignored.
--
--
--     * Keys that do not correspond to supported transport protocols are ignored.
--
--
--     * Duplicate keys are not allowed.
--
--
--     * Failure to parse or validate any key or value in the message will cause the @Publish@ call to return an error (no partial delivery).
--
--
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMessage :: Lens.Lens' Publish Lude.Text
pMessage = Lens.lens (message :: Publish -> Lude.Text) (\s a -> s {message = a} :: Publish)
{-# DEPRECATED pMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.AWSRequest Publish where
  type Rs Publish = PublishResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "PublishResult"
      ( \s h x ->
          PublishResponse'
            Lude.<$> (x Lude..@? "SequenceNumber")
            Lude.<*> (x Lude..@? "MessageId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders Publish where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath Publish where
  toPath = Lude.const "/"

instance Lude.ToQuery Publish where
  toQuery Publish' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("Publish" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "Subject" Lude.=: subject,
        "TargetArn" Lude.=: targetARN,
        "MessageAttributes"
          Lude.=: Lude.toQuery
            ( Lude.toQueryMap "entry" "Name" "Value"
                Lude.<$> messageAttributes
            ),
        "TopicArn" Lude.=: topicARN,
        "PhoneNumber" Lude.=: phoneNumber,
        "MessageDeduplicationId" Lude.=: messageDeduplicationId,
        "MessageStructure" Lude.=: messageStructure,
        "MessageGroupId" Lude.=: messageGroupId,
        "Message" Lude.=: message
      ]

-- | Response for Publish action.
--
-- /See:/ 'mkPublishResponse' smart constructor.
data PublishResponse = PublishResponse'
  { sequenceNumber ::
      Lude.Maybe Lude.Text,
    messageId :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublishResponse' with the minimum fields required to make a request.
--
-- * 'messageId' - Unique identifier assigned to the published message.
--
-- Length Constraint: Maximum 100 characters
-- * 'responseStatus' - The response status code.
-- * 'sequenceNumber' - This response element applies only to FIFO (first-in-first-out) topics.
--
-- The sequence number is a large, non-consecutive number that Amazon SNS assigns to each message. The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues to increase for each @MessageGroupId@ .
mkPublishResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PublishResponse
mkPublishResponse pResponseStatus_ =
  PublishResponse'
    { sequenceNumber = Lude.Nothing,
      messageId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | This response element applies only to FIFO (first-in-first-out) topics.
--
-- The sequence number is a large, non-consecutive number that Amazon SNS assigns to each message. The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues to increase for each @MessageGroupId@ .
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsSequenceNumber :: Lens.Lens' PublishResponse (Lude.Maybe Lude.Text)
prsSequenceNumber = Lens.lens (sequenceNumber :: PublishResponse -> Lude.Maybe Lude.Text) (\s a -> s {sequenceNumber = a} :: PublishResponse)
{-# DEPRECATED prsSequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead." #-}

-- | Unique identifier assigned to the published message.
--
-- Length Constraint: Maximum 100 characters
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsMessageId :: Lens.Lens' PublishResponse (Lude.Maybe Lude.Text)
prsMessageId = Lens.lens (messageId :: PublishResponse -> Lude.Maybe Lude.Text) (\s a -> s {messageId = a} :: PublishResponse)
{-# DEPRECATED prsMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsResponseStatus :: Lens.Lens' PublishResponse Lude.Int
prsResponseStatus = Lens.lens (responseStatus :: PublishResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PublishResponse)
{-# DEPRECATED prsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
