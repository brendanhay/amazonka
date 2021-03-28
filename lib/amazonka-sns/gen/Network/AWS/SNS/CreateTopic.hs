{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.CreateTopic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a topic to which notifications can be published. Users can create at most 100,000 standard topics (at most 1,000 FIFO topics). For more information, see <http://aws.amazon.com/sns/ https://aws.amazon.com/sns> . This action is idempotent, so if the requester already owns a topic with the specified name, that topic's ARN is returned without creating a new topic.
module Network.AWS.SNS.CreateTopic
    (
    -- * Creating a request
      CreateTopic (..)
    , mkCreateTopic
    -- ** Request lenses
    , ctName
    , ctAttributes
    , ctTags

    -- * Destructuring the response
    , CreateTopicResponse (..)
    , mkCreateTopicResponse
    -- ** Response lenses
    , ctrrsTopicArn
    , ctrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for CreateTopic action.
--
-- /See:/ 'mkCreateTopic' smart constructor.
data CreateTopic = CreateTopic'
  { name :: Types.TopicName
    -- ^ The name of the topic you want to create.
--
-- Constraints: Topic names must be made up of only uppercase and lowercase ASCII letters, numbers, underscores, and hyphens, and must be between 1 and 256 characters long.
-- For a FIFO (first-in-first-out) topic, the name must end with the @.fifo@ suffix. 
  , attributes :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue)
    -- ^ A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special request parameters that the @CreateTopic@ action uses:
--
--     * @DeliveryPolicy@ – The policy that defines how Amazon SNS retries failed deliveries to HTTP/S endpoints.
--
--
--     * @DisplayName@ – The display name to use for a topic with SMS subscriptions.
--
--
--     * @FifoTopic@ – Set to true to create a FIFO topic.
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
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The list of tags to add to a new topic.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTopic' value with any optional fields omitted.
mkCreateTopic
    :: Types.TopicName -- ^ 'name'
    -> CreateTopic
mkCreateTopic name
  = CreateTopic'{name, attributes = Core.Nothing,
                 tags = Core.Nothing}

-- | The name of the topic you want to create.
--
-- Constraints: Topic names must be made up of only uppercase and lowercase ASCII letters, numbers, underscores, and hyphens, and must be between 1 and 256 characters long.
-- For a FIFO (first-in-first-out) topic, the name must end with the @.fifo@ suffix. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctName :: Lens.Lens' CreateTopic Types.TopicName
ctName = Lens.field @"name"
{-# INLINEABLE ctName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special request parameters that the @CreateTopic@ action uses:
--
--     * @DeliveryPolicy@ – The policy that defines how Amazon SNS retries failed deliveries to HTTP/S endpoints.
--
--
--     * @DisplayName@ – The display name to use for a topic with SMS subscriptions.
--
--
--     * @FifoTopic@ – Set to true to create a FIFO topic.
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
ctAttributes :: Lens.Lens' CreateTopic (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
ctAttributes = Lens.field @"attributes"
{-# INLINEABLE ctAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The list of tags to add to a new topic.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTopic (Core.Maybe [Types.Tag])
ctTags = Lens.field @"tags"
{-# INLINEABLE ctTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateTopic where
        toQuery CreateTopic{..}
          = Core.toQueryPair "Action" ("CreateTopic" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<> Core.toQueryPair "Name" name
              Core.<>
              Core.toQueryPair "Attributes"
                (Core.maybe Core.mempty (Core.toQueryMap "entry" "key" "value")
                   attributes)
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "member") tags)

instance Core.ToHeaders CreateTopic where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateTopic where
        type Rs CreateTopic = CreateTopicResponse
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
          = Response.receiveXMLWrapper "CreateTopicResult"
              (\ s h x ->
                 CreateTopicResponse' Core.<$>
                   (x Core..@? "TopicArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Response from CreateTopic action.
--
-- /See:/ 'mkCreateTopicResponse' smart constructor.
data CreateTopicResponse = CreateTopicResponse'
  { topicArn :: Core.Maybe Types.TopicArn
    -- ^ The Amazon Resource Name (ARN) assigned to the created topic.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTopicResponse' value with any optional fields omitted.
mkCreateTopicResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTopicResponse
mkCreateTopicResponse responseStatus
  = CreateTopicResponse'{topicArn = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) assigned to the created topic.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsTopicArn :: Lens.Lens' CreateTopicResponse (Core.Maybe Types.TopicArn)
ctrrsTopicArn = Lens.field @"topicArn"
{-# INLINEABLE ctrrsTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsResponseStatus :: Lens.Lens' CreateTopicResponse Core.Int
ctrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
