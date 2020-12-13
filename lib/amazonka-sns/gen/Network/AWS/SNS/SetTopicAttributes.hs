{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SetTopicAttributes (..),
    mkSetTopicAttributes,

    -- ** Request lenses
    staAttributeValue,
    staTopicARN,
    staAttributeName,

    -- * Destructuring the response
    SetTopicAttributesResponse (..),
    mkSetTopicAttributesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for SetTopicAttributes action.
--
-- /See:/ 'mkSetTopicAttributes' smart constructor.
data SetTopicAttributes = SetTopicAttributes'
  { -- | The new value for the attribute.
    attributeValue :: Lude.Maybe Lude.Text,
    -- | The ARN of the topic to modify.
    topicARN :: Lude.Text,
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
    attributeName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetTopicAttributes' with the minimum fields required to make a request.
--
-- * 'attributeValue' - The new value for the attribute.
-- * 'topicARN' - The ARN of the topic to modify.
-- * 'attributeName' - A map of attributes with their corresponding values.
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
mkSetTopicAttributes ::
  -- | 'topicARN'
  Lude.Text ->
  -- | 'attributeName'
  Lude.Text ->
  SetTopicAttributes
mkSetTopicAttributes pTopicARN_ pAttributeName_ =
  SetTopicAttributes'
    { attributeValue = Lude.Nothing,
      topicARN = pTopicARN_,
      attributeName = pAttributeName_
    }

-- | The new value for the attribute.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staAttributeValue :: Lens.Lens' SetTopicAttributes (Lude.Maybe Lude.Text)
staAttributeValue = Lens.lens (attributeValue :: SetTopicAttributes -> Lude.Maybe Lude.Text) (\s a -> s {attributeValue = a} :: SetTopicAttributes)
{-# DEPRECATED staAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

-- | The ARN of the topic to modify.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staTopicARN :: Lens.Lens' SetTopicAttributes Lude.Text
staTopicARN = Lens.lens (topicARN :: SetTopicAttributes -> Lude.Text) (\s a -> s {topicARN = a} :: SetTopicAttributes)
{-# DEPRECATED staTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

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
staAttributeName :: Lens.Lens' SetTopicAttributes Lude.Text
staAttributeName = Lens.lens (attributeName :: SetTopicAttributes -> Lude.Text) (\s a -> s {attributeName = a} :: SetTopicAttributes)
{-# DEPRECATED staAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.AWSRequest SetTopicAttributes where
  type Rs SetTopicAttributes = SetTopicAttributesResponse
  request = Req.postQuery snsService
  response = Res.receiveNull SetTopicAttributesResponse'

instance Lude.ToHeaders SetTopicAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetTopicAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery SetTopicAttributes where
  toQuery SetTopicAttributes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetTopicAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "AttributeValue" Lude.=: attributeValue,
        "TopicArn" Lude.=: topicARN,
        "AttributeName" Lude.=: attributeName
      ]

-- | /See:/ 'mkSetTopicAttributesResponse' smart constructor.
data SetTopicAttributesResponse = SetTopicAttributesResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetTopicAttributesResponse' with the minimum fields required to make a request.
mkSetTopicAttributesResponse ::
  SetTopicAttributesResponse
mkSetTopicAttributesResponse = SetTopicAttributesResponse'
