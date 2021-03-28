{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      Publish (..)
    , mkPublish
    -- ** Request lenses
    , pMessage
    , pMessageAttributes
    , pMessageDeduplicationId
    , pMessageGroupId
    , pMessageStructure
    , pPhoneNumber
    , pSubject
    , pTargetArn
    , pTopicArn

    -- * Destructuring the response
    , PublishResponse (..)
    , mkPublishResponse
    -- ** Response lenses
    , prrsMessageId
    , prrsSequenceNumber
    , prrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for Publish action.
--
-- /See:/ 'mkPublish' smart constructor.
data Publish = Publish'
  { message :: Types.Message
    -- ^ The message you want to send.
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
  , messageAttributes :: Core.Maybe (Core.HashMap Core.Text Types.MessageAttributeValue)
    -- ^ Message attributes for Publish action.
  , messageDeduplicationId :: Core.Maybe Core.Text
    -- ^ This parameter applies only to FIFO (first-in-first-out) topics. The @MessageDeduplicationId@ can contain up to 128 alphanumeric characters (a-z, A-Z, 0-9) and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ .
--
-- Every message must have a unique @MessageDeduplicationId@ , which is a token used for deduplication of sent messages. If a message with a particular @MessageDeduplicationId@ is sent successfully, any message sent with the same @MessageDeduplicationId@ during the 5-minute deduplication interval is treated as a duplicate. 
-- If the topic has @ContentBasedDeduplication@ set, the system generates a @MessageDeduplicationId@ based on the contents of the message. Your @MessageDeduplicationId@ overrides the generated one.
  , messageGroupId :: Core.Maybe Core.Text
    -- ^ This parameter applies only to FIFO (first-in-first-out) topics. The @MessageGroupId@ can contain up to 128 alphanumeric characters (a-z, A-Z, 0-9) and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ .
--
-- The @MessageGroupId@ is a tag that specifies that a message belongs to a specific message group. Messages that belong to the same message group are processed in a FIFO manner (however, messages in different message groups might be processed out of order). Every message must include a @MessageGroupId@ .
  , messageStructure :: Core.Maybe Types.MessageStructure
    -- ^ Set @MessageStructure@ to @json@ if you want to send a different message for each protocol. For example, using one publish action, you can send a short message to your SMS subscribers and a longer message to your email subscribers. If you set @MessageStructure@ to @json@ , the value of the @Message@ parameter must: 
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
  , phoneNumber :: Core.Maybe Core.Text
    -- ^ The phone number to which you want to deliver an SMS message. Use E.164 format.
--
-- If you don't specify a value for the @PhoneNumber@ parameter, you must specify a value for the @TargetArn@ or @TopicArn@ parameters.
  , subject :: Core.Maybe Types.Subject
    -- ^ Optional parameter to be used as the "Subject" line when the message is delivered to email endpoints. This field will also be included, if present, in the standard JSON messages delivered to other endpoints.
--
-- Constraints: Subjects must be ASCII text that begins with a letter, number, or punctuation mark; must not include line breaks or control characters; and must be less than 100 characters long.
  , targetArn :: Core.Maybe Core.Text
    -- ^ If you don't specify a value for the @TargetArn@ parameter, you must specify a value for the @PhoneNumber@ or @TopicArn@ parameters.
  , topicArn :: Core.Maybe Types.TopicARN
    -- ^ The topic you want to publish to.
--
-- If you don't specify a value for the @TopicArn@ parameter, you must specify a value for the @PhoneNumber@ or @TargetArn@ parameters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Publish' value with any optional fields omitted.
mkPublish
    :: Types.Message -- ^ 'message'
    -> Publish
mkPublish message
  = Publish'{message, messageAttributes = Core.Nothing,
             messageDeduplicationId = Core.Nothing,
             messageGroupId = Core.Nothing, messageStructure = Core.Nothing,
             phoneNumber = Core.Nothing, subject = Core.Nothing,
             targetArn = Core.Nothing, topicArn = Core.Nothing}

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
pMessage :: Lens.Lens' Publish Types.Message
pMessage = Lens.field @"message"
{-# INLINEABLE pMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | Message attributes for Publish action.
--
-- /Note:/ Consider using 'messageAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMessageAttributes :: Lens.Lens' Publish (Core.Maybe (Core.HashMap Core.Text Types.MessageAttributeValue))
pMessageAttributes = Lens.field @"messageAttributes"
{-# INLINEABLE pMessageAttributes #-}
{-# DEPRECATED messageAttributes "Use generic-lens or generic-optics with 'messageAttributes' instead"  #-}

-- | This parameter applies only to FIFO (first-in-first-out) topics. The @MessageDeduplicationId@ can contain up to 128 alphanumeric characters (a-z, A-Z, 0-9) and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ .
--
-- Every message must have a unique @MessageDeduplicationId@ , which is a token used for deduplication of sent messages. If a message with a particular @MessageDeduplicationId@ is sent successfully, any message sent with the same @MessageDeduplicationId@ during the 5-minute deduplication interval is treated as a duplicate. 
-- If the topic has @ContentBasedDeduplication@ set, the system generates a @MessageDeduplicationId@ based on the contents of the message. Your @MessageDeduplicationId@ overrides the generated one.
--
-- /Note:/ Consider using 'messageDeduplicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMessageDeduplicationId :: Lens.Lens' Publish (Core.Maybe Core.Text)
pMessageDeduplicationId = Lens.field @"messageDeduplicationId"
{-# INLINEABLE pMessageDeduplicationId #-}
{-# DEPRECATED messageDeduplicationId "Use generic-lens or generic-optics with 'messageDeduplicationId' instead"  #-}

-- | This parameter applies only to FIFO (first-in-first-out) topics. The @MessageGroupId@ can contain up to 128 alphanumeric characters (a-z, A-Z, 0-9) and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ .
--
-- The @MessageGroupId@ is a tag that specifies that a message belongs to a specific message group. Messages that belong to the same message group are processed in a FIFO manner (however, messages in different message groups might be processed out of order). Every message must include a @MessageGroupId@ .
--
-- /Note:/ Consider using 'messageGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMessageGroupId :: Lens.Lens' Publish (Core.Maybe Core.Text)
pMessageGroupId = Lens.field @"messageGroupId"
{-# INLINEABLE pMessageGroupId #-}
{-# DEPRECATED messageGroupId "Use generic-lens or generic-optics with 'messageGroupId' instead"  #-}

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
pMessageStructure :: Lens.Lens' Publish (Core.Maybe Types.MessageStructure)
pMessageStructure = Lens.field @"messageStructure"
{-# INLINEABLE pMessageStructure #-}
{-# DEPRECATED messageStructure "Use generic-lens or generic-optics with 'messageStructure' instead"  #-}

-- | The phone number to which you want to deliver an SMS message. Use E.164 format.
--
-- If you don't specify a value for the @PhoneNumber@ parameter, you must specify a value for the @TargetArn@ or @TopicArn@ parameters.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPhoneNumber :: Lens.Lens' Publish (Core.Maybe Core.Text)
pPhoneNumber = Lens.field @"phoneNumber"
{-# INLINEABLE pPhoneNumber #-}
{-# DEPRECATED phoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead"  #-}

-- | Optional parameter to be used as the "Subject" line when the message is delivered to email endpoints. This field will also be included, if present, in the standard JSON messages delivered to other endpoints.
--
-- Constraints: Subjects must be ASCII text that begins with a letter, number, or punctuation mark; must not include line breaks or control characters; and must be less than 100 characters long.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSubject :: Lens.Lens' Publish (Core.Maybe Types.Subject)
pSubject = Lens.field @"subject"
{-# INLINEABLE pSubject #-}
{-# DEPRECATED subject "Use generic-lens or generic-optics with 'subject' instead"  #-}

-- | If you don't specify a value for the @TargetArn@ parameter, you must specify a value for the @PhoneNumber@ or @TopicArn@ parameters.
--
-- /Note:/ Consider using 'targetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTargetArn :: Lens.Lens' Publish (Core.Maybe Core.Text)
pTargetArn = Lens.field @"targetArn"
{-# INLINEABLE pTargetArn #-}
{-# DEPRECATED targetArn "Use generic-lens or generic-optics with 'targetArn' instead"  #-}

-- | The topic you want to publish to.
--
-- If you don't specify a value for the @TopicArn@ parameter, you must specify a value for the @PhoneNumber@ or @TargetArn@ parameters.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTopicArn :: Lens.Lens' Publish (Core.Maybe Types.TopicARN)
pTopicArn = Lens.field @"topicArn"
{-# INLINEABLE pTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

instance Core.ToQuery Publish where
        toQuery Publish{..}
          = Core.toQueryPair "Action" ("Publish" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<> Core.toQueryPair "Message" message
              Core.<>
              Core.toQueryPair "MessageAttributes"
                (Core.maybe Core.mempty (Core.toQueryMap "entry" "Name" "Value")
                   messageAttributes)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MessageDeduplicationId")
                messageDeduplicationId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MessageGroupId")
                messageGroupId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MessageStructure")
                messageStructure
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PhoneNumber") phoneNumber
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Subject") subject
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TargetArn") targetArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TopicArn") topicArn

instance Core.ToHeaders Publish where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest Publish where
        type Rs Publish = PublishResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "PublishResult"
              (\ s h x ->
                 PublishResponse' Core.<$>
                   (x Core..@? "MessageId") Core.<*> x Core..@? "SequenceNumber"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Response for Publish action.
--
-- /See:/ 'mkPublishResponse' smart constructor.
data PublishResponse = PublishResponse'
  { messageId :: Core.Maybe Types.MessageId
    -- ^ Unique identifier assigned to the published message.
--
-- Length Constraint: Maximum 100 characters
  , sequenceNumber :: Core.Maybe Core.Text
    -- ^ This response element applies only to FIFO (first-in-first-out) topics. 
--
-- The sequence number is a large, non-consecutive number that Amazon SNS assigns to each message. The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues to increase for each @MessageGroupId@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PublishResponse' value with any optional fields omitted.
mkPublishResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PublishResponse
mkPublishResponse responseStatus
  = PublishResponse'{messageId = Core.Nothing,
                     sequenceNumber = Core.Nothing, responseStatus}

-- | Unique identifier assigned to the published message.
--
-- Length Constraint: Maximum 100 characters
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrsMessageId :: Lens.Lens' PublishResponse (Core.Maybe Types.MessageId)
prrsMessageId = Lens.field @"messageId"
{-# INLINEABLE prrsMessageId #-}
{-# DEPRECATED messageId "Use generic-lens or generic-optics with 'messageId' instead"  #-}

-- | This response element applies only to FIFO (first-in-first-out) topics. 
--
-- The sequence number is a large, non-consecutive number that Amazon SNS assigns to each message. The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues to increase for each @MessageGroupId@ .
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrsSequenceNumber :: Lens.Lens' PublishResponse (Core.Maybe Core.Text)
prrsSequenceNumber = Lens.field @"sequenceNumber"
{-# INLINEABLE prrsSequenceNumber #-}
{-# DEPRECATED sequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrsResponseStatus :: Lens.Lens' PublishResponse Core.Int
prrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE prrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
