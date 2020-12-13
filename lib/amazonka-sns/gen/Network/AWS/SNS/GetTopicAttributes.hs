{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetTopicAttributes (..),
    mkGetTopicAttributes,

    -- ** Request lenses
    gtaTopicARN,

    -- * Destructuring the response
    GetTopicAttributesResponse (..),
    mkGetTopicAttributesResponse,

    -- ** Response lenses
    gtarsAttributes,
    gtarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for GetTopicAttributes action.
--
-- /See:/ 'mkGetTopicAttributes' smart constructor.
newtype GetTopicAttributes = GetTopicAttributes'
  { -- | The ARN of the topic whose properties you want to get.
    topicARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTopicAttributes' with the minimum fields required to make a request.
--
-- * 'topicARN' - The ARN of the topic whose properties you want to get.
mkGetTopicAttributes ::
  -- | 'topicARN'
  Lude.Text ->
  GetTopicAttributes
mkGetTopicAttributes pTopicARN_ =
  GetTopicAttributes' {topicARN = pTopicARN_}

-- | The ARN of the topic whose properties you want to get.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtaTopicARN :: Lens.Lens' GetTopicAttributes Lude.Text
gtaTopicARN = Lens.lens (topicARN :: GetTopicAttributes -> Lude.Text) (\s a -> s {topicARN = a} :: GetTopicAttributes)
{-# DEPRECATED gtaTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

instance Lude.AWSRequest GetTopicAttributes where
  type Rs GetTopicAttributes = GetTopicAttributesResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "GetTopicAttributesResult"
      ( \s h x ->
          GetTopicAttributesResponse'
            Lude.<$> ( x Lude..@? "Attributes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLMap "entry" "key" "value")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTopicAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetTopicAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTopicAttributes where
  toQuery GetTopicAttributes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetTopicAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "TopicArn" Lude.=: topicARN
      ]

-- | Response for GetTopicAttributes action.
--
-- /See:/ 'mkGetTopicAttributesResponse' smart constructor.
data GetTopicAttributesResponse = GetTopicAttributesResponse'
  { -- | A map of the topic's attributes. Attributes in this map include the following:
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
    attributes :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTopicAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - A map of the topic's attributes. Attributes in this map include the following:
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
-- * 'responseStatus' - The response status code.
mkGetTopicAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTopicAttributesResponse
mkGetTopicAttributesResponse pResponseStatus_ =
  GetTopicAttributesResponse'
    { attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

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
gtarsAttributes :: Lens.Lens' GetTopicAttributesResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gtarsAttributes = Lens.lens (attributes :: GetTopicAttributesResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: GetTopicAttributesResponse)
{-# DEPRECATED gtarsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtarsResponseStatus :: Lens.Lens' GetTopicAttributesResponse Lude.Int
gtarsResponseStatus = Lens.lens (responseStatus :: GetTopicAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTopicAttributesResponse)
{-# DEPRECATED gtarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
