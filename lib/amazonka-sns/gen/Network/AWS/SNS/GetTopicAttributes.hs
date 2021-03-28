{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.GetTopicAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the properties of a topic. Topic properties returned might differ based on the authorization of the user.
module Network.AWS.SNS.GetTopicAttributes
    (
    -- * Creating a request
      GetTopicAttributes (..)
    , mkGetTopicAttributes
    -- ** Request lenses
    , gtaTopicArn

    -- * Destructuring the response
    , GetTopicAttributesResponse (..)
    , mkGetTopicAttributesResponse
    -- ** Response lenses
    , gtarrsAttributes
    , gtarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for GetTopicAttributes action.
--
-- /See:/ 'mkGetTopicAttributes' smart constructor.
newtype GetTopicAttributes = GetTopicAttributes'
  { topicArn :: Types.TopicArn
    -- ^ The ARN of the topic whose properties you want to get.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTopicAttributes' value with any optional fields omitted.
mkGetTopicAttributes
    :: Types.TopicArn -- ^ 'topicArn'
    -> GetTopicAttributes
mkGetTopicAttributes topicArn = GetTopicAttributes'{topicArn}

-- | The ARN of the topic whose properties you want to get.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtaTopicArn :: Lens.Lens' GetTopicAttributes Types.TopicArn
gtaTopicArn = Lens.field @"topicArn"
{-# INLINEABLE gtaTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

instance Core.ToQuery GetTopicAttributes where
        toQuery GetTopicAttributes{..}
          = Core.toQueryPair "Action" ("GetTopicAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<> Core.toQueryPair "TopicArn" topicArn

instance Core.ToHeaders GetTopicAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetTopicAttributes where
        type Rs GetTopicAttributes = GetTopicAttributesResponse
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
          = Response.receiveXMLWrapper "GetTopicAttributesResult"
              (\ s h x ->
                 GetTopicAttributesResponse' Core.<$>
                   (x Core..@? "Attributes" Core..<@>
                      Core.parseXMLMap "entry" "key" "value")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Response for GetTopicAttributes action.
--
-- /See:/ 'mkGetTopicAttributesResponse' smart constructor.
data GetTopicAttributesResponse = GetTopicAttributesResponse'
  { attributes :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue)
    -- ^ A map of the topic's attributes. Attributes in this map include the following:
--
--
--     * @DeliveryPolicy@ – The JSON serialization of the topic's delivery policy.
--
--
--     * @DisplayName@ – The human-readable name used in the @From@ field for notifications to @email@ and @email-json@ endpoints.
--
--
--     * @Owner@ – The AWS account ID of the topic's owner.
--
--
--     * @Policy@ – The JSON serialization of the topic's access control policy.
--
--
--     * @SubscriptionsConfirmed@ – The number of confirmed subscriptions for the topic.
--
--
--     * @SubscriptionsDeleted@ – The number of deleted subscriptions for the topic.
--
--
--     * @SubscriptionsPending@ – The number of subscriptions pending confirmation for the topic.
--
--
--     * @TopicArn@ – The topic's ARN.
--
--
--     * @EffectiveDeliveryPolicy@ – The JSON serialization of the effective delivery policy, taking system defaults into account.
--
--
-- The following attribute applies only to <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption> :
--
--     * @KmsMasterKeyId@ - The ID of an AWS-managed customer master key (CMK) for Amazon SNS or a custom CMK. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms> . For more examples, see <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId> in the /AWS Key Management Service API Reference/ .
--
--
-- The following attributes apply only to <https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html FIFO topics> :
--
--     * @FifoTopic@ – When this is set to @true@ , a FIFO topic is created.
--
--
--     * @ContentBasedDeduplication@ – Enables content-based deduplication for FIFO topics. 
--
--     * By default, @ContentBasedDeduplication@ is set to @false@ . If you create a FIFO topic and this attribute is @false@ , you must specify a value for the @MessageDeduplicationId@ parameter for the <https://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish> action. 
--
--
--     * When you set @ContentBasedDeduplication@ to @true@ , Amazon SNS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message).
-- (Optional) To override the generated value, you can specify a value for the the @MessageDeduplicationId@ parameter for the @Publish@ action.
--
--
--
--
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTopicAttributesResponse' value with any optional fields omitted.
mkGetTopicAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTopicAttributesResponse
mkGetTopicAttributesResponse responseStatus
  = GetTopicAttributesResponse'{attributes = Core.Nothing,
                                responseStatus}

-- | A map of the topic's attributes. Attributes in this map include the following:
--
--
--     * @DeliveryPolicy@ – The JSON serialization of the topic's delivery policy.
--
--
--     * @DisplayName@ – The human-readable name used in the @From@ field for notifications to @email@ and @email-json@ endpoints.
--
--
--     * @Owner@ – The AWS account ID of the topic's owner.
--
--
--     * @Policy@ – The JSON serialization of the topic's access control policy.
--
--
--     * @SubscriptionsConfirmed@ – The number of confirmed subscriptions for the topic.
--
--
--     * @SubscriptionsDeleted@ – The number of deleted subscriptions for the topic.
--
--
--     * @SubscriptionsPending@ – The number of subscriptions pending confirmation for the topic.
--
--
--     * @TopicArn@ – The topic's ARN.
--
--
--     * @EffectiveDeliveryPolicy@ – The JSON serialization of the effective delivery policy, taking system defaults into account.
--
--
-- The following attribute applies only to <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption> :
--
--     * @KmsMasterKeyId@ - The ID of an AWS-managed customer master key (CMK) for Amazon SNS or a custom CMK. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms> . For more examples, see <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId> in the /AWS Key Management Service API Reference/ .
--
--
-- The following attributes apply only to <https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html FIFO topics> :
--
--     * @FifoTopic@ – When this is set to @true@ , a FIFO topic is created.
--
--
--     * @ContentBasedDeduplication@ – Enables content-based deduplication for FIFO topics. 
--
--     * By default, @ContentBasedDeduplication@ is set to @false@ . If you create a FIFO topic and this attribute is @false@ , you must specify a value for the @MessageDeduplicationId@ parameter for the <https://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish> action. 
--
--
--     * When you set @ContentBasedDeduplication@ to @true@ , Amazon SNS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message).
-- (Optional) To override the generated value, you can specify a value for the the @MessageDeduplicationId@ parameter for the @Publish@ action.
--
--
--
--
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtarrsAttributes :: Lens.Lens' GetTopicAttributesResponse (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
gtarrsAttributes = Lens.field @"attributes"
{-# INLINEABLE gtarrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtarrsResponseStatus :: Lens.Lens' GetTopicAttributesResponse Core.Int
gtarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
