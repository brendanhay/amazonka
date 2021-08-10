{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.GetTopicAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the properties of a topic. Topic properties returned
-- might differ based on the authorization of the user.
module Network.AWS.SNS.GetTopicAttributes
  ( -- * Creating a Request
    GetTopicAttributes (..),
    newGetTopicAttributes,

    -- * Request Lenses
    getTopicAttributes_topicArn,

    -- * Destructuring the Response
    GetTopicAttributesResponse (..),
    newGetTopicAttributesResponse,

    -- * Response Lenses
    getTopicAttributesResponse_attributes,
    getTopicAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | Input for GetTopicAttributes action.
--
-- /See:/ 'newGetTopicAttributes' smart constructor.
data GetTopicAttributes = GetTopicAttributes'
  { -- | The ARN of the topic whose properties you want to get.
    topicArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTopicAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicArn', 'getTopicAttributes_topicArn' - The ARN of the topic whose properties you want to get.
newGetTopicAttributes ::
  -- | 'topicArn'
  Prelude.Text ->
  GetTopicAttributes
newGetTopicAttributes pTopicArn_ =
  GetTopicAttributes' {topicArn = pTopicArn_}

-- | The ARN of the topic whose properties you want to get.
getTopicAttributes_topicArn :: Lens.Lens' GetTopicAttributes Prelude.Text
getTopicAttributes_topicArn = Lens.lens (\GetTopicAttributes' {topicArn} -> topicArn) (\s@GetTopicAttributes' {} a -> s {topicArn = a} :: GetTopicAttributes)

instance Core.AWSRequest GetTopicAttributes where
  type
    AWSResponse GetTopicAttributes =
      GetTopicAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetTopicAttributesResult"
      ( \s h x ->
          GetTopicAttributesResponse'
            Prelude.<$> ( x Core..@? "Attributes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLMap "entry" "key" "value")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTopicAttributes

instance Prelude.NFData GetTopicAttributes

instance Core.ToHeaders GetTopicAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetTopicAttributes where
  toPath = Prelude.const "/"

instance Core.ToQuery GetTopicAttributes where
  toQuery GetTopicAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("GetTopicAttributes" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-03-31" :: Prelude.ByteString),
        "TopicArn" Core.=: topicArn
      ]

-- | Response for GetTopicAttributes action.
--
-- /See:/ 'newGetTopicAttributesResponse' smart constructor.
data GetTopicAttributesResponse = GetTopicAttributesResponse'
  { -- | A map of the topic\'s attributes. Attributes in this map include the
    -- following:
    --
    -- -   @DeliveryPolicy@ – The JSON serialization of the topic\'s delivery
    --     policy.
    --
    -- -   @DisplayName@ – The human-readable name used in the @From@ field for
    --     notifications to @email@ and @email-json@ endpoints.
    --
    -- -   @Owner@ – The AWS account ID of the topic\'s owner.
    --
    -- -   @Policy@ – The JSON serialization of the topic\'s access control
    --     policy.
    --
    -- -   @SubscriptionsConfirmed@ – The number of confirmed subscriptions for
    --     the topic.
    --
    -- -   @SubscriptionsDeleted@ – The number of deleted subscriptions for the
    --     topic.
    --
    -- -   @SubscriptionsPending@ – The number of subscriptions pending
    --     confirmation for the topic.
    --
    -- -   @TopicArn@ – The topic\'s ARN.
    --
    -- -   @EffectiveDeliveryPolicy@ – The JSON serialization of the effective
    --     delivery policy, taking system defaults into account.
    --
    -- The following attribute applies only to
    -- <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption>:
    --
    -- -   @KmsMasterKeyId@ - The ID of an AWS-managed customer master key
    --     (CMK) for Amazon SNS or a custom CMK. For more information, see
    --     <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms>.
    --     For more examples, see
    --     <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId>
    --     in the /AWS Key Management Service API Reference/.
    --
    -- The following attributes apply only to
    -- <https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html FIFO topics>:
    --
    -- -   @FifoTopic@ – When this is set to @true@, a FIFO topic is created.
    --
    -- -   @ContentBasedDeduplication@ – Enables content-based deduplication
    --     for FIFO topics.
    --
    --     -   By default, @ContentBasedDeduplication@ is set to @false@. If
    --         you create a FIFO topic and this attribute is @false@, you must
    --         specify a value for the @MessageDeduplicationId@ parameter for
    --         the
    --         <https://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish>
    --         action.
    --
    --     -   When you set @ContentBasedDeduplication@ to @true@, Amazon SNS
    --         uses a SHA-256 hash to generate the @MessageDeduplicationId@
    --         using the body of the message (but not the attributes of the
    --         message).
    --
    --         (Optional) To override the generated value, you can specify a
    --         value for the the @MessageDeduplicationId@ parameter for the
    --         @Publish@ action.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTopicAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'getTopicAttributesResponse_attributes' - A map of the topic\'s attributes. Attributes in this map include the
-- following:
--
-- -   @DeliveryPolicy@ – The JSON serialization of the topic\'s delivery
--     policy.
--
-- -   @DisplayName@ – The human-readable name used in the @From@ field for
--     notifications to @email@ and @email-json@ endpoints.
--
-- -   @Owner@ – The AWS account ID of the topic\'s owner.
--
-- -   @Policy@ – The JSON serialization of the topic\'s access control
--     policy.
--
-- -   @SubscriptionsConfirmed@ – The number of confirmed subscriptions for
--     the topic.
--
-- -   @SubscriptionsDeleted@ – The number of deleted subscriptions for the
--     topic.
--
-- -   @SubscriptionsPending@ – The number of subscriptions pending
--     confirmation for the topic.
--
-- -   @TopicArn@ – The topic\'s ARN.
--
-- -   @EffectiveDeliveryPolicy@ – The JSON serialization of the effective
--     delivery policy, taking system defaults into account.
--
-- The following attribute applies only to
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption>:
--
-- -   @KmsMasterKeyId@ - The ID of an AWS-managed customer master key
--     (CMK) for Amazon SNS or a custom CMK. For more information, see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms>.
--     For more examples, see
--     <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId>
--     in the /AWS Key Management Service API Reference/.
--
-- The following attributes apply only to
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html FIFO topics>:
--
-- -   @FifoTopic@ – When this is set to @true@, a FIFO topic is created.
--
-- -   @ContentBasedDeduplication@ – Enables content-based deduplication
--     for FIFO topics.
--
--     -   By default, @ContentBasedDeduplication@ is set to @false@. If
--         you create a FIFO topic and this attribute is @false@, you must
--         specify a value for the @MessageDeduplicationId@ parameter for
--         the
--         <https://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish>
--         action.
--
--     -   When you set @ContentBasedDeduplication@ to @true@, Amazon SNS
--         uses a SHA-256 hash to generate the @MessageDeduplicationId@
--         using the body of the message (but not the attributes of the
--         message).
--
--         (Optional) To override the generated value, you can specify a
--         value for the the @MessageDeduplicationId@ parameter for the
--         @Publish@ action.
--
-- 'httpStatus', 'getTopicAttributesResponse_httpStatus' - The response's http status code.
newGetTopicAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTopicAttributesResponse
newGetTopicAttributesResponse pHttpStatus_ =
  GetTopicAttributesResponse'
    { attributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map of the topic\'s attributes. Attributes in this map include the
-- following:
--
-- -   @DeliveryPolicy@ – The JSON serialization of the topic\'s delivery
--     policy.
--
-- -   @DisplayName@ – The human-readable name used in the @From@ field for
--     notifications to @email@ and @email-json@ endpoints.
--
-- -   @Owner@ – The AWS account ID of the topic\'s owner.
--
-- -   @Policy@ – The JSON serialization of the topic\'s access control
--     policy.
--
-- -   @SubscriptionsConfirmed@ – The number of confirmed subscriptions for
--     the topic.
--
-- -   @SubscriptionsDeleted@ – The number of deleted subscriptions for the
--     topic.
--
-- -   @SubscriptionsPending@ – The number of subscriptions pending
--     confirmation for the topic.
--
-- -   @TopicArn@ – The topic\'s ARN.
--
-- -   @EffectiveDeliveryPolicy@ – The JSON serialization of the effective
--     delivery policy, taking system defaults into account.
--
-- The following attribute applies only to
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption>:
--
-- -   @KmsMasterKeyId@ - The ID of an AWS-managed customer master key
--     (CMK) for Amazon SNS or a custom CMK. For more information, see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms>.
--     For more examples, see
--     <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId>
--     in the /AWS Key Management Service API Reference/.
--
-- The following attributes apply only to
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html FIFO topics>:
--
-- -   @FifoTopic@ – When this is set to @true@, a FIFO topic is created.
--
-- -   @ContentBasedDeduplication@ – Enables content-based deduplication
--     for FIFO topics.
--
--     -   By default, @ContentBasedDeduplication@ is set to @false@. If
--         you create a FIFO topic and this attribute is @false@, you must
--         specify a value for the @MessageDeduplicationId@ parameter for
--         the
--         <https://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish>
--         action.
--
--     -   When you set @ContentBasedDeduplication@ to @true@, Amazon SNS
--         uses a SHA-256 hash to generate the @MessageDeduplicationId@
--         using the body of the message (but not the attributes of the
--         message).
--
--         (Optional) To override the generated value, you can specify a
--         value for the the @MessageDeduplicationId@ parameter for the
--         @Publish@ action.
getTopicAttributesResponse_attributes :: Lens.Lens' GetTopicAttributesResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getTopicAttributesResponse_attributes = Lens.lens (\GetTopicAttributesResponse' {attributes} -> attributes) (\s@GetTopicAttributesResponse' {} a -> s {attributes = a} :: GetTopicAttributesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getTopicAttributesResponse_httpStatus :: Lens.Lens' GetTopicAttributesResponse Prelude.Int
getTopicAttributesResponse_httpStatus = Lens.lens (\GetTopicAttributesResponse' {httpStatus} -> httpStatus) (\s@GetTopicAttributesResponse' {} a -> s {httpStatus = a} :: GetTopicAttributesResponse)

instance Prelude.NFData GetTopicAttributesResponse
