{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.SetTopicAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a topic owner to set an attribute of the topic to a new value.
module Network.AWS.SNS.SetTopicAttributes
    (
    -- * Creating a request
      SetTopicAttributes (..)
    , mkSetTopicAttributes
    -- ** Request lenses
    , staTopicArn
    , staAttributeName
    , staAttributeValue

    -- * Destructuring the response
    , SetTopicAttributesResponse (..)
    , mkSetTopicAttributesResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for SetTopicAttributes action.
--
-- /See:/ 'mkSetTopicAttributes' smart constructor.
data SetTopicAttributes = SetTopicAttributes'
  { topicArn :: Types.TopicArn
    -- ^ The ARN of the topic to modify.
  , attributeName :: Types.AttributeName
    -- ^ A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special request parameters that the @SetTopicAttributes@ action uses:
--
--     * @DeliveryPolicy@ – The policy that defines how Amazon SNS retries failed deliveries to HTTP/S endpoints.
--
--
--     * @DisplayName@ – The display name to use for a topic with SMS subscriptions.
--
--
--     * @Policy@ – The policy that defines who can access your topic. By default, only the topic owner can publish or subscribe to the topic.
--
--
-- The following attribute applies only to <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption> :
--
--     * @KmsMasterKeyId@ – The ID of an AWS-managed customer master key (CMK) for Amazon SNS or a custom CMK. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms> . For more examples, see <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId> in the /AWS Key Management Service API Reference/ . 
--
--
-- The following attribute applies only to <https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html FIFO topics> :
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
  , attributeValue :: Core.Maybe Types.AttributeValue
    -- ^ The new value for the attribute.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetTopicAttributes' value with any optional fields omitted.
mkSetTopicAttributes
    :: Types.TopicArn -- ^ 'topicArn'
    -> Types.AttributeName -- ^ 'attributeName'
    -> SetTopicAttributes
mkSetTopicAttributes topicArn attributeName
  = SetTopicAttributes'{topicArn, attributeName,
                        attributeValue = Core.Nothing}

-- | The ARN of the topic to modify.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staTopicArn :: Lens.Lens' SetTopicAttributes Types.TopicArn
staTopicArn = Lens.field @"topicArn"
{-# INLINEABLE staTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

-- | A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special request parameters that the @SetTopicAttributes@ action uses:
--
--     * @DeliveryPolicy@ – The policy that defines how Amazon SNS retries failed deliveries to HTTP/S endpoints.
--
--
--     * @DisplayName@ – The display name to use for a topic with SMS subscriptions.
--
--
--     * @Policy@ – The policy that defines who can access your topic. By default, only the topic owner can publish or subscribe to the topic.
--
--
-- The following attribute applies only to <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption> :
--
--     * @KmsMasterKeyId@ – The ID of an AWS-managed customer master key (CMK) for Amazon SNS or a custom CMK. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms> . For more examples, see <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId> in the /AWS Key Management Service API Reference/ . 
--
--
-- The following attribute applies only to <https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html FIFO topics> :
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
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staAttributeName :: Lens.Lens' SetTopicAttributes Types.AttributeName
staAttributeName = Lens.field @"attributeName"
{-# INLINEABLE staAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

-- | The new value for the attribute.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staAttributeValue :: Lens.Lens' SetTopicAttributes (Core.Maybe Types.AttributeValue)
staAttributeValue = Lens.field @"attributeValue"
{-# INLINEABLE staAttributeValue #-}
{-# DEPRECATED attributeValue "Use generic-lens or generic-optics with 'attributeValue' instead"  #-}

instance Core.ToQuery SetTopicAttributes where
        toQuery SetTopicAttributes{..}
          = Core.toQueryPair "Action" ("SetTopicAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<> Core.toQueryPair "TopicArn" topicArn
              Core.<> Core.toQueryPair "AttributeName" attributeName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AttributeValue")
                attributeValue

instance Core.ToHeaders SetTopicAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetTopicAttributes where
        type Rs SetTopicAttributes = SetTopicAttributesResponse
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
        parseResponse = Response.receiveNull SetTopicAttributesResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetTopicAttributesResponse' smart constructor.
data SetTopicAttributesResponse = SetTopicAttributesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetTopicAttributesResponse' value with any optional fields omitted.
mkSetTopicAttributesResponse
    :: SetTopicAttributesResponse
mkSetTopicAttributesResponse = SetTopicAttributesResponse'
