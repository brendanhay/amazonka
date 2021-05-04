{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SNS.SetTopicAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a topic owner to set an attribute of the topic to a new value.
module Network.AWS.SNS.SetTopicAttributes
  ( -- * Creating a Request
    SetTopicAttributes (..),
    newSetTopicAttributes,

    -- * Request Lenses
    setTopicAttributes_attributeValue,
    setTopicAttributes_topicArn,
    setTopicAttributes_attributeName,

    -- * Destructuring the Response
    SetTopicAttributesResponse (..),
    newSetTopicAttributesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | Input for SetTopicAttributes action.
--
-- /See:/ 'newSetTopicAttributes' smart constructor.
data SetTopicAttributes = SetTopicAttributes'
  { -- | The new value for the attribute.
    attributeValue :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the topic to modify.
    topicArn :: Prelude.Text,
    -- | A map of attributes with their corresponding values.
    --
    -- The following lists the names, descriptions, and values of the special
    -- request parameters that the @SetTopicAttributes@ action uses:
    --
    -- -   @DeliveryPolicy@ – The policy that defines how Amazon SNS retries
    --     failed deliveries to HTTP\/S endpoints.
    --
    -- -   @DisplayName@ – The display name to use for a topic with SMS
    --     subscriptions.
    --
    -- -   @Policy@ – The policy that defines who can access your topic. By
    --     default, only the topic owner can publish or subscribe to the topic.
    --
    -- The following attribute applies only to
    -- <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption>:
    --
    -- -   @KmsMasterKeyId@ – The ID of an AWS-managed customer master key
    --     (CMK) for Amazon SNS or a custom CMK. For more information, see
    --     <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms>.
    --     For more examples, see
    --     <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId>
    --     in the /AWS Key Management Service API Reference/.
    --
    -- The following attribute applies only to
    -- <https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html FIFO topics>:
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
    attributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetTopicAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeValue', 'setTopicAttributes_attributeValue' - The new value for the attribute.
--
-- 'topicArn', 'setTopicAttributes_topicArn' - The ARN of the topic to modify.
--
-- 'attributeName', 'setTopicAttributes_attributeName' - A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special
-- request parameters that the @SetTopicAttributes@ action uses:
--
-- -   @DeliveryPolicy@ – The policy that defines how Amazon SNS retries
--     failed deliveries to HTTP\/S endpoints.
--
-- -   @DisplayName@ – The display name to use for a topic with SMS
--     subscriptions.
--
-- -   @Policy@ – The policy that defines who can access your topic. By
--     default, only the topic owner can publish or subscribe to the topic.
--
-- The following attribute applies only to
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption>:
--
-- -   @KmsMasterKeyId@ – The ID of an AWS-managed customer master key
--     (CMK) for Amazon SNS or a custom CMK. For more information, see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms>.
--     For more examples, see
--     <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId>
--     in the /AWS Key Management Service API Reference/.
--
-- The following attribute applies only to
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html FIFO topics>:
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
newSetTopicAttributes ::
  -- | 'topicArn'
  Prelude.Text ->
  -- | 'attributeName'
  Prelude.Text ->
  SetTopicAttributes
newSetTopicAttributes pTopicArn_ pAttributeName_ =
  SetTopicAttributes'
    { attributeValue =
        Prelude.Nothing,
      topicArn = pTopicArn_,
      attributeName = pAttributeName_
    }

-- | The new value for the attribute.
setTopicAttributes_attributeValue :: Lens.Lens' SetTopicAttributes (Prelude.Maybe Prelude.Text)
setTopicAttributes_attributeValue = Lens.lens (\SetTopicAttributes' {attributeValue} -> attributeValue) (\s@SetTopicAttributes' {} a -> s {attributeValue = a} :: SetTopicAttributes)

-- | The ARN of the topic to modify.
setTopicAttributes_topicArn :: Lens.Lens' SetTopicAttributes Prelude.Text
setTopicAttributes_topicArn = Lens.lens (\SetTopicAttributes' {topicArn} -> topicArn) (\s@SetTopicAttributes' {} a -> s {topicArn = a} :: SetTopicAttributes)

-- | A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special
-- request parameters that the @SetTopicAttributes@ action uses:
--
-- -   @DeliveryPolicy@ – The policy that defines how Amazon SNS retries
--     failed deliveries to HTTP\/S endpoints.
--
-- -   @DisplayName@ – The display name to use for a topic with SMS
--     subscriptions.
--
-- -   @Policy@ – The policy that defines who can access your topic. By
--     default, only the topic owner can publish or subscribe to the topic.
--
-- The following attribute applies only to
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption>:
--
-- -   @KmsMasterKeyId@ – The ID of an AWS-managed customer master key
--     (CMK) for Amazon SNS or a custom CMK. For more information, see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms>.
--     For more examples, see
--     <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId>
--     in the /AWS Key Management Service API Reference/.
--
-- The following attribute applies only to
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-fifo-topics.html FIFO topics>:
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
setTopicAttributes_attributeName :: Lens.Lens' SetTopicAttributes Prelude.Text
setTopicAttributes_attributeName = Lens.lens (\SetTopicAttributes' {attributeName} -> attributeName) (\s@SetTopicAttributes' {} a -> s {attributeName = a} :: SetTopicAttributes)

instance Prelude.AWSRequest SetTopicAttributes where
  type
    Rs SetTopicAttributes =
      SetTopicAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull SetTopicAttributesResponse'

instance Prelude.Hashable SetTopicAttributes

instance Prelude.NFData SetTopicAttributes

instance Prelude.ToHeaders SetTopicAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath SetTopicAttributes where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SetTopicAttributes where
  toQuery SetTopicAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("SetTopicAttributes" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-03-31" :: Prelude.ByteString),
        "AttributeValue" Prelude.=: attributeValue,
        "TopicArn" Prelude.=: topicArn,
        "AttributeName" Prelude.=: attributeName
      ]

-- | /See:/ 'newSetTopicAttributesResponse' smart constructor.
data SetTopicAttributesResponse = SetTopicAttributesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetTopicAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetTopicAttributesResponse ::
  SetTopicAttributesResponse
newSetTopicAttributesResponse =
  SetTopicAttributesResponse'

instance Prelude.NFData SetTopicAttributesResponse
