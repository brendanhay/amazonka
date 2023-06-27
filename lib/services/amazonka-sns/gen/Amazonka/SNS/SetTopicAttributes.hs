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
-- Module      : Amazonka.SNS.SetTopicAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a topic owner to set an attribute of the topic to a new value.
--
-- To remove the ability to change topic permissions, you must deny
-- permissions to the @AddPermission@, @RemovePermission@, and
-- @SetTopicAttributes@ actions in your IAM policy.
module Amazonka.SNS.SetTopicAttributes
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

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
    -- -   @ApplicationSuccessFeedbackRoleArn@ – Indicates failed message
    --     delivery status for an Amazon SNS topic that is subscribed to a
    --     platform application endpoint.
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
    -- -   @TracingConfig@ – Tracing mode of an Amazon SNS topic. By default
    --     @TracingConfig@ is set to @PassThrough@, and the topic passes
    --     through the tracing header it receives from an Amazon SNS publisher
    --     to its subscriptions. If set to @Active@, Amazon SNS will vend X-Ray
    --     segment data to topic owner account if the sampled flag in the
    --     tracing header is true. This is only supported on standard topics.
    --
    -- -   HTTP
    --
    --     -   @HTTPSuccessFeedbackRoleArn@ – Indicates successful message
    --         delivery status for an Amazon SNS topic that is subscribed to an
    --         HTTP endpoint.
    --
    --     -   @HTTPSuccessFeedbackSampleRate@ – Indicates percentage of
    --         successful messages to sample for an Amazon SNS topic that is
    --         subscribed to an HTTP endpoint.
    --
    --     -   @HTTPFailureFeedbackRoleArn@ – Indicates failed message delivery
    --         status for an Amazon SNS topic that is subscribed to an HTTP
    --         endpoint.
    --
    -- -   Amazon Kinesis Data Firehose
    --
    --     -   @FirehoseSuccessFeedbackRoleArn@ – Indicates successful message
    --         delivery status for an Amazon SNS topic that is subscribed to an
    --         Amazon Kinesis Data Firehose endpoint.
    --
    --     -   @FirehoseSuccessFeedbackSampleRate@ – Indicates percentage of
    --         successful messages to sample for an Amazon SNS topic that is
    --         subscribed to an Amazon Kinesis Data Firehose endpoint.
    --
    --     -   @FirehoseFailureFeedbackRoleArn@ – Indicates failed message
    --         delivery status for an Amazon SNS topic that is subscribed to an
    --         Amazon Kinesis Data Firehose endpoint.
    --
    -- -   Lambda
    --
    --     -   @LambdaSuccessFeedbackRoleArn@ – Indicates successful message
    --         delivery status for an Amazon SNS topic that is subscribed to an
    --         Lambda endpoint.
    --
    --     -   @LambdaSuccessFeedbackSampleRate@ – Indicates percentage of
    --         successful messages to sample for an Amazon SNS topic that is
    --         subscribed to an Lambda endpoint.
    --
    --     -   @LambdaFailureFeedbackRoleArn@ – Indicates failed message
    --         delivery status for an Amazon SNS topic that is subscribed to an
    --         Lambda endpoint.
    --
    -- -   Platform application endpoint
    --
    --     -   @ApplicationSuccessFeedbackRoleArn@ – Indicates successful
    --         message delivery status for an Amazon SNS topic that is
    --         subscribed to an Amazon Web Services application endpoint.
    --
    --     -   @ApplicationSuccessFeedbackSampleRate@ – Indicates percentage of
    --         successful messages to sample for an Amazon SNS topic that is
    --         subscribed to an Amazon Web Services application endpoint.
    --
    --     -   @ApplicationFailureFeedbackRoleArn@ – Indicates failed message
    --         delivery status for an Amazon SNS topic that is subscribed to an
    --         Amazon Web Services application endpoint.
    --
    --     In addition to being able to configure topic attributes for message
    --     delivery status of notification messages sent to Amazon SNS
    --     application endpoints, you can also configure application attributes
    --     for the delivery status of push notification messages sent to push
    --     notification services.
    --
    --     For example, For more information, see
    --     <https://docs.aws.amazon.com/sns/latest/dg/sns-msg-status.html Using Amazon SNS Application Attributes for Message Delivery Status>.
    --
    -- -   Amazon SQS
    --
    --     -   @SQSSuccessFeedbackRoleArn@ – Indicates successful message
    --         delivery status for an Amazon SNS topic that is subscribed to an
    --         Amazon SQS endpoint.
    --
    --     -   @SQSSuccessFeedbackSampleRate@ – Indicates percentage of
    --         successful messages to sample for an Amazon SNS topic that is
    --         subscribed to an Amazon SQS endpoint.
    --
    --     -   @SQSFailureFeedbackRoleArn@ – Indicates failed message delivery
    --         status for an Amazon SNS topic that is subscribed to an Amazon
    --         SQS endpoint.
    --
    -- The \<ENDPOINT>SuccessFeedbackRoleArn and
    -- \<ENDPOINT>FailureFeedbackRoleArn attributes are used to give Amazon SNS
    -- write access to use CloudWatch Logs on your behalf. The
    -- \<ENDPOINT>SuccessFeedbackSampleRate attribute is for specifying the
    -- sample rate percentage (0-100) of successfully delivered messages. After
    -- you configure the \<ENDPOINT>FailureFeedbackRoleArn attribute, then all
    -- failed message deliveries generate CloudWatch Logs.
    --
    -- The following attribute applies only to
    -- <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption>:
    --
    -- -   @KmsMasterKeyId@ – The ID of an Amazon Web Services managed customer
    --     master key (CMK) for Amazon SNS or a custom CMK. For more
    --     information, see
    --     <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms>.
    --     For more examples, see
    --     <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId>
    --     in the /Key Management Service API Reference/.
    --
    -- -   @SignatureVersion@ – The signature version corresponds to the
    --     hashing algorithm used while creating the signature of the
    --     notifications, subscription confirmations, or unsubscribe
    --     confirmation messages sent by Amazon SNS. By default,
    --     @SignatureVersion@ is set to @1@.
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
    --         value for the @MessageDeduplicationId@ parameter for the
    --         @Publish@ action.
    attributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- -   @ApplicationSuccessFeedbackRoleArn@ – Indicates failed message
--     delivery status for an Amazon SNS topic that is subscribed to a
--     platform application endpoint.
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
-- -   @TracingConfig@ – Tracing mode of an Amazon SNS topic. By default
--     @TracingConfig@ is set to @PassThrough@, and the topic passes
--     through the tracing header it receives from an Amazon SNS publisher
--     to its subscriptions. If set to @Active@, Amazon SNS will vend X-Ray
--     segment data to topic owner account if the sampled flag in the
--     tracing header is true. This is only supported on standard topics.
--
-- -   HTTP
--
--     -   @HTTPSuccessFeedbackRoleArn@ – Indicates successful message
--         delivery status for an Amazon SNS topic that is subscribed to an
--         HTTP endpoint.
--
--     -   @HTTPSuccessFeedbackSampleRate@ – Indicates percentage of
--         successful messages to sample for an Amazon SNS topic that is
--         subscribed to an HTTP endpoint.
--
--     -   @HTTPFailureFeedbackRoleArn@ – Indicates failed message delivery
--         status for an Amazon SNS topic that is subscribed to an HTTP
--         endpoint.
--
-- -   Amazon Kinesis Data Firehose
--
--     -   @FirehoseSuccessFeedbackRoleArn@ – Indicates successful message
--         delivery status for an Amazon SNS topic that is subscribed to an
--         Amazon Kinesis Data Firehose endpoint.
--
--     -   @FirehoseSuccessFeedbackSampleRate@ – Indicates percentage of
--         successful messages to sample for an Amazon SNS topic that is
--         subscribed to an Amazon Kinesis Data Firehose endpoint.
--
--     -   @FirehoseFailureFeedbackRoleArn@ – Indicates failed message
--         delivery status for an Amazon SNS topic that is subscribed to an
--         Amazon Kinesis Data Firehose endpoint.
--
-- -   Lambda
--
--     -   @LambdaSuccessFeedbackRoleArn@ – Indicates successful message
--         delivery status for an Amazon SNS topic that is subscribed to an
--         Lambda endpoint.
--
--     -   @LambdaSuccessFeedbackSampleRate@ – Indicates percentage of
--         successful messages to sample for an Amazon SNS topic that is
--         subscribed to an Lambda endpoint.
--
--     -   @LambdaFailureFeedbackRoleArn@ – Indicates failed message
--         delivery status for an Amazon SNS topic that is subscribed to an
--         Lambda endpoint.
--
-- -   Platform application endpoint
--
--     -   @ApplicationSuccessFeedbackRoleArn@ – Indicates successful
--         message delivery status for an Amazon SNS topic that is
--         subscribed to an Amazon Web Services application endpoint.
--
--     -   @ApplicationSuccessFeedbackSampleRate@ – Indicates percentage of
--         successful messages to sample for an Amazon SNS topic that is
--         subscribed to an Amazon Web Services application endpoint.
--
--     -   @ApplicationFailureFeedbackRoleArn@ – Indicates failed message
--         delivery status for an Amazon SNS topic that is subscribed to an
--         Amazon Web Services application endpoint.
--
--     In addition to being able to configure topic attributes for message
--     delivery status of notification messages sent to Amazon SNS
--     application endpoints, you can also configure application attributes
--     for the delivery status of push notification messages sent to push
--     notification services.
--
--     For example, For more information, see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-msg-status.html Using Amazon SNS Application Attributes for Message Delivery Status>.
--
-- -   Amazon SQS
--
--     -   @SQSSuccessFeedbackRoleArn@ – Indicates successful message
--         delivery status for an Amazon SNS topic that is subscribed to an
--         Amazon SQS endpoint.
--
--     -   @SQSSuccessFeedbackSampleRate@ – Indicates percentage of
--         successful messages to sample for an Amazon SNS topic that is
--         subscribed to an Amazon SQS endpoint.
--
--     -   @SQSFailureFeedbackRoleArn@ – Indicates failed message delivery
--         status for an Amazon SNS topic that is subscribed to an Amazon
--         SQS endpoint.
--
-- The \<ENDPOINT>SuccessFeedbackRoleArn and
-- \<ENDPOINT>FailureFeedbackRoleArn attributes are used to give Amazon SNS
-- write access to use CloudWatch Logs on your behalf. The
-- \<ENDPOINT>SuccessFeedbackSampleRate attribute is for specifying the
-- sample rate percentage (0-100) of successfully delivered messages. After
-- you configure the \<ENDPOINT>FailureFeedbackRoleArn attribute, then all
-- failed message deliveries generate CloudWatch Logs.
--
-- The following attribute applies only to
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption>:
--
-- -   @KmsMasterKeyId@ – The ID of an Amazon Web Services managed customer
--     master key (CMK) for Amazon SNS or a custom CMK. For more
--     information, see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms>.
--     For more examples, see
--     <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId>
--     in the /Key Management Service API Reference/.
--
-- -   @SignatureVersion@ – The signature version corresponds to the
--     hashing algorithm used while creating the signature of the
--     notifications, subscription confirmations, or unsubscribe
--     confirmation messages sent by Amazon SNS. By default,
--     @SignatureVersion@ is set to @1@.
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
--         value for the @MessageDeduplicationId@ parameter for the
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
-- -   @ApplicationSuccessFeedbackRoleArn@ – Indicates failed message
--     delivery status for an Amazon SNS topic that is subscribed to a
--     platform application endpoint.
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
-- -   @TracingConfig@ – Tracing mode of an Amazon SNS topic. By default
--     @TracingConfig@ is set to @PassThrough@, and the topic passes
--     through the tracing header it receives from an Amazon SNS publisher
--     to its subscriptions. If set to @Active@, Amazon SNS will vend X-Ray
--     segment data to topic owner account if the sampled flag in the
--     tracing header is true. This is only supported on standard topics.
--
-- -   HTTP
--
--     -   @HTTPSuccessFeedbackRoleArn@ – Indicates successful message
--         delivery status for an Amazon SNS topic that is subscribed to an
--         HTTP endpoint.
--
--     -   @HTTPSuccessFeedbackSampleRate@ – Indicates percentage of
--         successful messages to sample for an Amazon SNS topic that is
--         subscribed to an HTTP endpoint.
--
--     -   @HTTPFailureFeedbackRoleArn@ – Indicates failed message delivery
--         status for an Amazon SNS topic that is subscribed to an HTTP
--         endpoint.
--
-- -   Amazon Kinesis Data Firehose
--
--     -   @FirehoseSuccessFeedbackRoleArn@ – Indicates successful message
--         delivery status for an Amazon SNS topic that is subscribed to an
--         Amazon Kinesis Data Firehose endpoint.
--
--     -   @FirehoseSuccessFeedbackSampleRate@ – Indicates percentage of
--         successful messages to sample for an Amazon SNS topic that is
--         subscribed to an Amazon Kinesis Data Firehose endpoint.
--
--     -   @FirehoseFailureFeedbackRoleArn@ – Indicates failed message
--         delivery status for an Amazon SNS topic that is subscribed to an
--         Amazon Kinesis Data Firehose endpoint.
--
-- -   Lambda
--
--     -   @LambdaSuccessFeedbackRoleArn@ – Indicates successful message
--         delivery status for an Amazon SNS topic that is subscribed to an
--         Lambda endpoint.
--
--     -   @LambdaSuccessFeedbackSampleRate@ – Indicates percentage of
--         successful messages to sample for an Amazon SNS topic that is
--         subscribed to an Lambda endpoint.
--
--     -   @LambdaFailureFeedbackRoleArn@ – Indicates failed message
--         delivery status for an Amazon SNS topic that is subscribed to an
--         Lambda endpoint.
--
-- -   Platform application endpoint
--
--     -   @ApplicationSuccessFeedbackRoleArn@ – Indicates successful
--         message delivery status for an Amazon SNS topic that is
--         subscribed to an Amazon Web Services application endpoint.
--
--     -   @ApplicationSuccessFeedbackSampleRate@ – Indicates percentage of
--         successful messages to sample for an Amazon SNS topic that is
--         subscribed to an Amazon Web Services application endpoint.
--
--     -   @ApplicationFailureFeedbackRoleArn@ – Indicates failed message
--         delivery status for an Amazon SNS topic that is subscribed to an
--         Amazon Web Services application endpoint.
--
--     In addition to being able to configure topic attributes for message
--     delivery status of notification messages sent to Amazon SNS
--     application endpoints, you can also configure application attributes
--     for the delivery status of push notification messages sent to push
--     notification services.
--
--     For example, For more information, see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-msg-status.html Using Amazon SNS Application Attributes for Message Delivery Status>.
--
-- -   Amazon SQS
--
--     -   @SQSSuccessFeedbackRoleArn@ – Indicates successful message
--         delivery status for an Amazon SNS topic that is subscribed to an
--         Amazon SQS endpoint.
--
--     -   @SQSSuccessFeedbackSampleRate@ – Indicates percentage of
--         successful messages to sample for an Amazon SNS topic that is
--         subscribed to an Amazon SQS endpoint.
--
--     -   @SQSFailureFeedbackRoleArn@ – Indicates failed message delivery
--         status for an Amazon SNS topic that is subscribed to an Amazon
--         SQS endpoint.
--
-- The \<ENDPOINT>SuccessFeedbackRoleArn and
-- \<ENDPOINT>FailureFeedbackRoleArn attributes are used to give Amazon SNS
-- write access to use CloudWatch Logs on your behalf. The
-- \<ENDPOINT>SuccessFeedbackSampleRate attribute is for specifying the
-- sample rate percentage (0-100) of successfully delivered messages. After
-- you configure the \<ENDPOINT>FailureFeedbackRoleArn attribute, then all
-- failed message deliveries generate CloudWatch Logs.
--
-- The following attribute applies only to
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html server-side-encryption>:
--
-- -   @KmsMasterKeyId@ – The ID of an Amazon Web Services managed customer
--     master key (CMK) for Amazon SNS or a custom CMK. For more
--     information, see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-server-side-encryption.html#sse-key-terms Key Terms>.
--     For more examples, see
--     <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId>
--     in the /Key Management Service API Reference/.
--
-- -   @SignatureVersion@ – The signature version corresponds to the
--     hashing algorithm used while creating the signature of the
--     notifications, subscription confirmations, or unsubscribe
--     confirmation messages sent by Amazon SNS. By default,
--     @SignatureVersion@ is set to @1@.
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
--         value for the @MessageDeduplicationId@ parameter for the
--         @Publish@ action.
setTopicAttributes_attributeName :: Lens.Lens' SetTopicAttributes Prelude.Text
setTopicAttributes_attributeName = Lens.lens (\SetTopicAttributes' {attributeName} -> attributeName) (\s@SetTopicAttributes' {} a -> s {attributeName = a} :: SetTopicAttributes)

instance Core.AWSRequest SetTopicAttributes where
  type
    AWSResponse SetTopicAttributes =
      SetTopicAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull SetTopicAttributesResponse'

instance Prelude.Hashable SetTopicAttributes where
  hashWithSalt _salt SetTopicAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` attributeValue
      `Prelude.hashWithSalt` topicArn
      `Prelude.hashWithSalt` attributeName

instance Prelude.NFData SetTopicAttributes where
  rnf SetTopicAttributes' {..} =
    Prelude.rnf attributeValue
      `Prelude.seq` Prelude.rnf topicArn
      `Prelude.seq` Prelude.rnf attributeName

instance Data.ToHeaders SetTopicAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SetTopicAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery SetTopicAttributes where
  toQuery SetTopicAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SetTopicAttributes" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "AttributeValue" Data.=: attributeValue,
        "TopicArn" Data.=: topicArn,
        "AttributeName" Data.=: attributeName
      ]

-- | /See:/ 'newSetTopicAttributesResponse' smart constructor.
data SetTopicAttributesResponse = SetTopicAttributesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetTopicAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetTopicAttributesResponse ::
  SetTopicAttributesResponse
newSetTopicAttributesResponse =
  SetTopicAttributesResponse'

instance Prelude.NFData SetTopicAttributesResponse where
  rnf _ = ()
