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
-- Module      : Network.AWS.SNS.GetSubscriptionAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the properties of a subscription.
module Network.AWS.SNS.GetSubscriptionAttributes
  ( -- * Creating a Request
    GetSubscriptionAttributes (..),
    newGetSubscriptionAttributes,

    -- * Request Lenses
    getSubscriptionAttributes_subscriptionArn,

    -- * Destructuring the Response
    GetSubscriptionAttributesResponse (..),
    newGetSubscriptionAttributesResponse,

    -- * Response Lenses
    getSubscriptionAttributesResponse_attributes,
    getSubscriptionAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | Input for GetSubscriptionAttributes.
--
-- /See:/ 'newGetSubscriptionAttributes' smart constructor.
data GetSubscriptionAttributes = GetSubscriptionAttributes'
  { -- | The ARN of the subscription whose properties you want to get.
    subscriptionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSubscriptionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionArn', 'getSubscriptionAttributes_subscriptionArn' - The ARN of the subscription whose properties you want to get.
newGetSubscriptionAttributes ::
  -- | 'subscriptionArn'
  Prelude.Text ->
  GetSubscriptionAttributes
newGetSubscriptionAttributes pSubscriptionArn_ =
  GetSubscriptionAttributes'
    { subscriptionArn =
        pSubscriptionArn_
    }

-- | The ARN of the subscription whose properties you want to get.
getSubscriptionAttributes_subscriptionArn :: Lens.Lens' GetSubscriptionAttributes Prelude.Text
getSubscriptionAttributes_subscriptionArn = Lens.lens (\GetSubscriptionAttributes' {subscriptionArn} -> subscriptionArn) (\s@GetSubscriptionAttributes' {} a -> s {subscriptionArn = a} :: GetSubscriptionAttributes)

instance Core.AWSRequest GetSubscriptionAttributes where
  type
    AWSResponse GetSubscriptionAttributes =
      GetSubscriptionAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetSubscriptionAttributesResult"
      ( \s h x ->
          GetSubscriptionAttributesResponse'
            Prelude.<$> ( x Core..@? "Attributes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLMap "entry" "key" "value")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSubscriptionAttributes

instance Prelude.NFData GetSubscriptionAttributes

instance Core.ToHeaders GetSubscriptionAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetSubscriptionAttributes where
  toPath = Prelude.const "/"

instance Core.ToQuery GetSubscriptionAttributes where
  toQuery GetSubscriptionAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("GetSubscriptionAttributes" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-03-31" :: Prelude.ByteString),
        "SubscriptionArn" Core.=: subscriptionArn
      ]

-- | Response for GetSubscriptionAttributes action.
--
-- /See:/ 'newGetSubscriptionAttributesResponse' smart constructor.
data GetSubscriptionAttributesResponse = GetSubscriptionAttributesResponse'
  { -- | A map of the subscription\'s attributes. Attributes in this map include
    -- the following:
    --
    -- -   @ConfirmationWasAuthenticated@ – @true@ if the subscription
    --     confirmation request was authenticated.
    --
    -- -   @DeliveryPolicy@ – The JSON serialization of the subscription\'s
    --     delivery policy.
    --
    -- -   @EffectiveDeliveryPolicy@ – The JSON serialization of the effective
    --     delivery policy that takes into account the topic delivery policy
    --     and account system defaults.
    --
    -- -   @FilterPolicy@ – The filter policy JSON that is assigned to the
    --     subscription. For more information, see
    --     <https://docs.aws.amazon.com/sns/latest/dg/sns-message-filtering.html Amazon SNS Message Filtering>
    --     in the /Amazon SNS Developer Guide/.
    --
    -- -   @Owner@ – The AWS account ID of the subscription\'s owner.
    --
    -- -   @PendingConfirmation@ – @true@ if the subscription hasn\'t been
    --     confirmed. To confirm a pending subscription, call the
    --     @ConfirmSubscription@ action with a confirmation token.
    --
    -- -   @RawMessageDelivery@ – @true@ if raw message delivery is enabled for
    --     the subscription. Raw messages are free of JSON formatting and can
    --     be sent to HTTP\/S and Amazon SQS endpoints.
    --
    -- -   @RedrivePolicy@ – When specified, sends undeliverable messages to
    --     the specified Amazon SQS dead-letter queue. Messages that can\'t be
    --     delivered due to client errors (for example, when the subscribed
    --     endpoint is unreachable) or server errors (for example, when the
    --     service that powers the subscribed endpoint becomes unavailable) are
    --     held in the dead-letter queue for further analysis or reprocessing.
    --
    -- -   @SubscriptionArn@ – The subscription\'s ARN.
    --
    -- -   @TopicArn@ – The topic ARN that the subscription is associated with.
    --
    -- The following attribute applies only to Amazon Kinesis Data Firehose
    -- delivery stream subscriptions:
    --
    -- -   @SubscriptionRoleArn@ – The ARN of the IAM role that has the
    --     following:
    --
    --     -   Permission to write to the Kinesis Data Firehose delivery stream
    --
    --     -   Amazon SNS listed as a trusted entity
    --
    --     Specifying a valid ARN for this attribute is required for Kinesis
    --     Data Firehose delivery stream subscriptions. For more information,
    --     see
    --     <https://docs.aws.amazon.com/sns/latest/dg/sns-kinesis-subscriber.html Fanout to Kinesis Data Firehose delivery streams>
    --     in the /Amazon SNS Developer Guide/.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSubscriptionAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'getSubscriptionAttributesResponse_attributes' - A map of the subscription\'s attributes. Attributes in this map include
-- the following:
--
-- -   @ConfirmationWasAuthenticated@ – @true@ if the subscription
--     confirmation request was authenticated.
--
-- -   @DeliveryPolicy@ – The JSON serialization of the subscription\'s
--     delivery policy.
--
-- -   @EffectiveDeliveryPolicy@ – The JSON serialization of the effective
--     delivery policy that takes into account the topic delivery policy
--     and account system defaults.
--
-- -   @FilterPolicy@ – The filter policy JSON that is assigned to the
--     subscription. For more information, see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-message-filtering.html Amazon SNS Message Filtering>
--     in the /Amazon SNS Developer Guide/.
--
-- -   @Owner@ – The AWS account ID of the subscription\'s owner.
--
-- -   @PendingConfirmation@ – @true@ if the subscription hasn\'t been
--     confirmed. To confirm a pending subscription, call the
--     @ConfirmSubscription@ action with a confirmation token.
--
-- -   @RawMessageDelivery@ – @true@ if raw message delivery is enabled for
--     the subscription. Raw messages are free of JSON formatting and can
--     be sent to HTTP\/S and Amazon SQS endpoints.
--
-- -   @RedrivePolicy@ – When specified, sends undeliverable messages to
--     the specified Amazon SQS dead-letter queue. Messages that can\'t be
--     delivered due to client errors (for example, when the subscribed
--     endpoint is unreachable) or server errors (for example, when the
--     service that powers the subscribed endpoint becomes unavailable) are
--     held in the dead-letter queue for further analysis or reprocessing.
--
-- -   @SubscriptionArn@ – The subscription\'s ARN.
--
-- -   @TopicArn@ – The topic ARN that the subscription is associated with.
--
-- The following attribute applies only to Amazon Kinesis Data Firehose
-- delivery stream subscriptions:
--
-- -   @SubscriptionRoleArn@ – The ARN of the IAM role that has the
--     following:
--
--     -   Permission to write to the Kinesis Data Firehose delivery stream
--
--     -   Amazon SNS listed as a trusted entity
--
--     Specifying a valid ARN for this attribute is required for Kinesis
--     Data Firehose delivery stream subscriptions. For more information,
--     see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-kinesis-subscriber.html Fanout to Kinesis Data Firehose delivery streams>
--     in the /Amazon SNS Developer Guide/.
--
-- 'httpStatus', 'getSubscriptionAttributesResponse_httpStatus' - The response's http status code.
newGetSubscriptionAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSubscriptionAttributesResponse
newGetSubscriptionAttributesResponse pHttpStatus_ =
  GetSubscriptionAttributesResponse'
    { attributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map of the subscription\'s attributes. Attributes in this map include
-- the following:
--
-- -   @ConfirmationWasAuthenticated@ – @true@ if the subscription
--     confirmation request was authenticated.
--
-- -   @DeliveryPolicy@ – The JSON serialization of the subscription\'s
--     delivery policy.
--
-- -   @EffectiveDeliveryPolicy@ – The JSON serialization of the effective
--     delivery policy that takes into account the topic delivery policy
--     and account system defaults.
--
-- -   @FilterPolicy@ – The filter policy JSON that is assigned to the
--     subscription. For more information, see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-message-filtering.html Amazon SNS Message Filtering>
--     in the /Amazon SNS Developer Guide/.
--
-- -   @Owner@ – The AWS account ID of the subscription\'s owner.
--
-- -   @PendingConfirmation@ – @true@ if the subscription hasn\'t been
--     confirmed. To confirm a pending subscription, call the
--     @ConfirmSubscription@ action with a confirmation token.
--
-- -   @RawMessageDelivery@ – @true@ if raw message delivery is enabled for
--     the subscription. Raw messages are free of JSON formatting and can
--     be sent to HTTP\/S and Amazon SQS endpoints.
--
-- -   @RedrivePolicy@ – When specified, sends undeliverable messages to
--     the specified Amazon SQS dead-letter queue. Messages that can\'t be
--     delivered due to client errors (for example, when the subscribed
--     endpoint is unreachable) or server errors (for example, when the
--     service that powers the subscribed endpoint becomes unavailable) are
--     held in the dead-letter queue for further analysis or reprocessing.
--
-- -   @SubscriptionArn@ – The subscription\'s ARN.
--
-- -   @TopicArn@ – The topic ARN that the subscription is associated with.
--
-- The following attribute applies only to Amazon Kinesis Data Firehose
-- delivery stream subscriptions:
--
-- -   @SubscriptionRoleArn@ – The ARN of the IAM role that has the
--     following:
--
--     -   Permission to write to the Kinesis Data Firehose delivery stream
--
--     -   Amazon SNS listed as a trusted entity
--
--     Specifying a valid ARN for this attribute is required for Kinesis
--     Data Firehose delivery stream subscriptions. For more information,
--     see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-kinesis-subscriber.html Fanout to Kinesis Data Firehose delivery streams>
--     in the /Amazon SNS Developer Guide/.
getSubscriptionAttributesResponse_attributes :: Lens.Lens' GetSubscriptionAttributesResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getSubscriptionAttributesResponse_attributes = Lens.lens (\GetSubscriptionAttributesResponse' {attributes} -> attributes) (\s@GetSubscriptionAttributesResponse' {} a -> s {attributes = a} :: GetSubscriptionAttributesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getSubscriptionAttributesResponse_httpStatus :: Lens.Lens' GetSubscriptionAttributesResponse Prelude.Int
getSubscriptionAttributesResponse_httpStatus = Lens.lens (\GetSubscriptionAttributesResponse' {httpStatus} -> httpStatus) (\s@GetSubscriptionAttributesResponse' {} a -> s {httpStatus = a} :: GetSubscriptionAttributesResponse)

instance
  Prelude.NFData
    GetSubscriptionAttributesResponse
