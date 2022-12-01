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
-- Module      : Amazonka.SNS.CreateTopic
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a topic to which notifications can be published. Users can
-- create at most 100,000 standard topics (at most 1,000 FIFO topics). For
-- more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-create-topic.html Creating an Amazon SNS topic>
-- in the /Amazon SNS Developer Guide/. This action is idempotent, so if
-- the requester already owns a topic with the specified name, that
-- topic\'s ARN is returned without creating a new topic.
module Amazonka.SNS.CreateTopic
  ( -- * Creating a Request
    CreateTopic (..),
    newCreateTopic,

    -- * Request Lenses
    createTopic_tags,
    createTopic_attributes,
    createTopic_dataProtectionPolicy,
    createTopic_name,

    -- * Destructuring the Response
    CreateTopicResponse (..),
    newCreateTopicResponse,

    -- * Response Lenses
    createTopicResponse_topicArn,
    createTopicResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | Input for CreateTopic action.
--
-- /See:/ 'newCreateTopic' smart constructor.
data CreateTopic = CreateTopic'
  { -- | The list of tags to add to a new topic.
    --
    -- To be able to tag a topic on creation, you must have the
    -- @sns:CreateTopic@ and @sns:TagResource@ permissions.
    tags :: Prelude.Maybe [Tag],
    -- | A map of attributes with their corresponding values.
    --
    -- The following lists the names, descriptions, and values of the special
    -- request parameters that the @CreateTopic@ action uses:
    --
    -- -   @DeliveryPolicy@ – The policy that defines how Amazon SNS retries
    --     failed deliveries to HTTP\/S endpoints.
    --
    -- -   @DisplayName@ – The display name to use for a topic with SMS
    --     subscriptions.
    --
    -- -   @FifoTopic@ – Set to true to create a FIFO topic.
    --
    -- -   @Policy@ – The policy that defines who can access your topic. By
    --     default, only the topic owner can publish or subscribe to the topic.
    --
    -- The following attribute applies only to
    -- <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side encryption>:
    --
    -- -   @KmsMasterKeyId@ – The ID of an Amazon Web Services managed customer
    --     master key (CMK) for Amazon SNS or a custom CMK. For more
    --     information, see
    --     <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms>.
    --     For more examples, see
    --     <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId>
    --     in the /Key Management Service API Reference/.
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
    --         value for the @MessageDeduplicationId@ parameter for the
    --         @Publish@ action.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The body of the policy document you want to use for this topic.
    --
    -- You can only add one policy per topic.
    --
    -- The policy must be in JSON string format.
    --
    -- Length Constraints: Maximum length of 30,720.
    dataProtectionPolicy :: Prelude.Maybe Prelude.Text,
    -- | The name of the topic you want to create.
    --
    -- Constraints: Topic names must be made up of only uppercase and lowercase
    -- ASCII letters, numbers, underscores, and hyphens, and must be between 1
    -- and 256 characters long.
    --
    -- For a FIFO (first-in-first-out) topic, the name must end with the
    -- @.fifo@ suffix.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTopic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createTopic_tags' - The list of tags to add to a new topic.
--
-- To be able to tag a topic on creation, you must have the
-- @sns:CreateTopic@ and @sns:TagResource@ permissions.
--
-- 'attributes', 'createTopic_attributes' - A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special
-- request parameters that the @CreateTopic@ action uses:
--
-- -   @DeliveryPolicy@ – The policy that defines how Amazon SNS retries
--     failed deliveries to HTTP\/S endpoints.
--
-- -   @DisplayName@ – The display name to use for a topic with SMS
--     subscriptions.
--
-- -   @FifoTopic@ – Set to true to create a FIFO topic.
--
-- -   @Policy@ – The policy that defines who can access your topic. By
--     default, only the topic owner can publish or subscribe to the topic.
--
-- The following attribute applies only to
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side encryption>:
--
-- -   @KmsMasterKeyId@ – The ID of an Amazon Web Services managed customer
--     master key (CMK) for Amazon SNS or a custom CMK. For more
--     information, see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms>.
--     For more examples, see
--     <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId>
--     in the /Key Management Service API Reference/.
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
--         value for the @MessageDeduplicationId@ parameter for the
--         @Publish@ action.
--
-- 'dataProtectionPolicy', 'createTopic_dataProtectionPolicy' - The body of the policy document you want to use for this topic.
--
-- You can only add one policy per topic.
--
-- The policy must be in JSON string format.
--
-- Length Constraints: Maximum length of 30,720.
--
-- 'name', 'createTopic_name' - The name of the topic you want to create.
--
-- Constraints: Topic names must be made up of only uppercase and lowercase
-- ASCII letters, numbers, underscores, and hyphens, and must be between 1
-- and 256 characters long.
--
-- For a FIFO (first-in-first-out) topic, the name must end with the
-- @.fifo@ suffix.
newCreateTopic ::
  -- | 'name'
  Prelude.Text ->
  CreateTopic
newCreateTopic pName_ =
  CreateTopic'
    { tags = Prelude.Nothing,
      attributes = Prelude.Nothing,
      dataProtectionPolicy = Prelude.Nothing,
      name = pName_
    }

-- | The list of tags to add to a new topic.
--
-- To be able to tag a topic on creation, you must have the
-- @sns:CreateTopic@ and @sns:TagResource@ permissions.
createTopic_tags :: Lens.Lens' CreateTopic (Prelude.Maybe [Tag])
createTopic_tags = Lens.lens (\CreateTopic' {tags} -> tags) (\s@CreateTopic' {} a -> s {tags = a} :: CreateTopic) Prelude.. Lens.mapping Lens.coerced

-- | A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special
-- request parameters that the @CreateTopic@ action uses:
--
-- -   @DeliveryPolicy@ – The policy that defines how Amazon SNS retries
--     failed deliveries to HTTP\/S endpoints.
--
-- -   @DisplayName@ – The display name to use for a topic with SMS
--     subscriptions.
--
-- -   @FifoTopic@ – Set to true to create a FIFO topic.
--
-- -   @Policy@ – The policy that defines who can access your topic. By
--     default, only the topic owner can publish or subscribe to the topic.
--
-- The following attribute applies only to
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side encryption>:
--
-- -   @KmsMasterKeyId@ – The ID of an Amazon Web Services managed customer
--     master key (CMK) for Amazon SNS or a custom CMK. For more
--     information, see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms>.
--     For more examples, see
--     <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId>
--     in the /Key Management Service API Reference/.
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
--         value for the @MessageDeduplicationId@ parameter for the
--         @Publish@ action.
createTopic_attributes :: Lens.Lens' CreateTopic (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createTopic_attributes = Lens.lens (\CreateTopic' {attributes} -> attributes) (\s@CreateTopic' {} a -> s {attributes = a} :: CreateTopic) Prelude.. Lens.mapping Lens.coerced

-- | The body of the policy document you want to use for this topic.
--
-- You can only add one policy per topic.
--
-- The policy must be in JSON string format.
--
-- Length Constraints: Maximum length of 30,720.
createTopic_dataProtectionPolicy :: Lens.Lens' CreateTopic (Prelude.Maybe Prelude.Text)
createTopic_dataProtectionPolicy = Lens.lens (\CreateTopic' {dataProtectionPolicy} -> dataProtectionPolicy) (\s@CreateTopic' {} a -> s {dataProtectionPolicy = a} :: CreateTopic)

-- | The name of the topic you want to create.
--
-- Constraints: Topic names must be made up of only uppercase and lowercase
-- ASCII letters, numbers, underscores, and hyphens, and must be between 1
-- and 256 characters long.
--
-- For a FIFO (first-in-first-out) topic, the name must end with the
-- @.fifo@ suffix.
createTopic_name :: Lens.Lens' CreateTopic Prelude.Text
createTopic_name = Lens.lens (\CreateTopic' {name} -> name) (\s@CreateTopic' {} a -> s {name = a} :: CreateTopic)

instance Core.AWSRequest CreateTopic where
  type AWSResponse CreateTopic = CreateTopicResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateTopicResult"
      ( \s h x ->
          CreateTopicResponse'
            Prelude.<$> (x Core..@? "TopicArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTopic where
  hashWithSalt _salt CreateTopic' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` dataProtectionPolicy
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateTopic where
  rnf CreateTopic' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf dataProtectionPolicy
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreateTopic where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateTopic where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateTopic where
  toQuery CreateTopic' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateTopic" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-03-31" :: Prelude.ByteString),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> tags),
        "Attributes"
          Core.=: Core.toQuery
            ( Core.toQueryMap "entry" "key" "value"
                Prelude.<$> attributes
            ),
        "DataProtectionPolicy" Core.=: dataProtectionPolicy,
        "Name" Core.=: name
      ]

-- | Response from CreateTopic action.
--
-- /See:/ 'newCreateTopicResponse' smart constructor.
data CreateTopicResponse = CreateTopicResponse'
  { -- | The Amazon Resource Name (ARN) assigned to the created topic.
    topicArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTopicResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicArn', 'createTopicResponse_topicArn' - The Amazon Resource Name (ARN) assigned to the created topic.
--
-- 'httpStatus', 'createTopicResponse_httpStatus' - The response's http status code.
newCreateTopicResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTopicResponse
newCreateTopicResponse pHttpStatus_ =
  CreateTopicResponse'
    { topicArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) assigned to the created topic.
createTopicResponse_topicArn :: Lens.Lens' CreateTopicResponse (Prelude.Maybe Prelude.Text)
createTopicResponse_topicArn = Lens.lens (\CreateTopicResponse' {topicArn} -> topicArn) (\s@CreateTopicResponse' {} a -> s {topicArn = a} :: CreateTopicResponse)

-- | The response's http status code.
createTopicResponse_httpStatus :: Lens.Lens' CreateTopicResponse Prelude.Int
createTopicResponse_httpStatus = Lens.lens (\CreateTopicResponse' {httpStatus} -> httpStatus) (\s@CreateTopicResponse' {} a -> s {httpStatus = a} :: CreateTopicResponse)

instance Prelude.NFData CreateTopicResponse where
  rnf CreateTopicResponse' {..} =
    Prelude.rnf topicArn
      `Prelude.seq` Prelude.rnf httpStatus
