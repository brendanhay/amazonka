{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateTopic (..),
    mkCreateTopic,

    -- ** Request lenses
    ctAttributes,
    ctTags,
    ctName,

    -- * Destructuring the response
    CreateTopicResponse (..),
    mkCreateTopicResponse,

    -- ** Response lenses
    ctrsTopicARN,
    ctrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for CreateTopic action.
--
-- /See:/ 'mkCreateTopic' smart constructor.
data CreateTopic = CreateTopic'
  { attributes ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    tags :: Lude.Maybe [Tag],
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTopic' with the minimum fields required to make a request.
--
-- * 'attributes' - A map of attributes with their corresponding values.
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
-- * 'name' - The name of the topic you want to create.
--
-- Constraints: Topic names must be made up of only uppercase and lowercase ASCII letters, numbers, underscores, and hyphens, and must be between 1 and 256 characters long.
-- For a FIFO (first-in-first-out) topic, the name must end with the @.fifo@ suffix.
-- * 'tags' - The list of tags to add to a new topic.
mkCreateTopic ::
  -- | 'name'
  Lude.Text ->
  CreateTopic
mkCreateTopic pName_ =
  CreateTopic'
    { attributes = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_
    }

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
ctAttributes :: Lens.Lens' CreateTopic (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ctAttributes = Lens.lens (attributes :: CreateTopic -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: CreateTopic)
{-# DEPRECATED ctAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The list of tags to add to a new topic.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTopic (Lude.Maybe [Tag])
ctTags = Lens.lens (tags :: CreateTopic -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateTopic)
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the topic you want to create.
--
-- Constraints: Topic names must be made up of only uppercase and lowercase ASCII letters, numbers, underscores, and hyphens, and must be between 1 and 256 characters long.
-- For a FIFO (first-in-first-out) topic, the name must end with the @.fifo@ suffix.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctName :: Lens.Lens' CreateTopic Lude.Text
ctName = Lens.lens (name :: CreateTopic -> Lude.Text) (\s a -> s {name = a} :: CreateTopic)
{-# DEPRECATED ctName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateTopic where
  type Rs CreateTopic = CreateTopicResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "CreateTopicResult"
      ( \s h x ->
          CreateTopicResponse'
            Lude.<$> (x Lude..@? "TopicArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTopic where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTopic where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTopic where
  toQuery CreateTopic' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateTopic" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "Attributes"
          Lude.=: Lude.toQuery
            (Lude.toQueryMap "entry" "key" "value" Lude.<$> attributes),
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "Name" Lude.=: name
      ]

-- | Response from CreateTopic action.
--
-- /See:/ 'mkCreateTopicResponse' smart constructor.
data CreateTopicResponse = CreateTopicResponse'
  { topicARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateTopicResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'topicARN' - The Amazon Resource Name (ARN) assigned to the created topic.
mkCreateTopicResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTopicResponse
mkCreateTopicResponse pResponseStatus_ =
  CreateTopicResponse'
    { topicARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) assigned to the created topic.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsTopicARN :: Lens.Lens' CreateTopicResponse (Lude.Maybe Lude.Text)
ctrsTopicARN = Lens.lens (topicARN :: CreateTopicResponse -> Lude.Maybe Lude.Text) (\s a -> s {topicARN = a} :: CreateTopicResponse)
{-# DEPRECATED ctrsTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsResponseStatus :: Lens.Lens' CreateTopicResponse Lude.Int
ctrsResponseStatus = Lens.lens (responseStatus :: CreateTopicResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTopicResponse)
{-# DEPRECATED ctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
