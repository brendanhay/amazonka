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
-- Module      : Amazonka.SNS.SetSubscriptionAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a subscription owner to set an attribute of the subscription to a
-- new value.
module Amazonka.SNS.SetSubscriptionAttributes
  ( -- * Creating a Request
    SetSubscriptionAttributes (..),
    newSetSubscriptionAttributes,

    -- * Request Lenses
    setSubscriptionAttributes_attributeValue,
    setSubscriptionAttributes_subscriptionArn,
    setSubscriptionAttributes_attributeName,

    -- * Destructuring the Response
    SetSubscriptionAttributesResponse (..),
    newSetSubscriptionAttributesResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | Input for SetSubscriptionAttributes action.
--
-- /See:/ 'newSetSubscriptionAttributes' smart constructor.
data SetSubscriptionAttributes = SetSubscriptionAttributes'
  { -- | The new value for the attribute in JSON format.
    attributeValue :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the subscription to modify.
    subscriptionArn :: Prelude.Text,
    -- | A map of attributes with their corresponding values.
    --
    -- The following lists the names, descriptions, and values of the special
    -- request parameters that this action uses:
    --
    -- -   @DeliveryPolicy@ – The policy that defines how Amazon SNS retries
    --     failed deliveries to HTTP\/S endpoints.
    --
    -- -   @FilterPolicy@ – The simple JSON object that lets your subscriber
    --     receive only a subset of messages, rather than receiving every
    --     message published to the topic.
    --
    -- -   @FilterPolicyScope@ – This attribute lets you choose the filtering
    --     scope by using one of the following string value types:
    --
    --     -   @MessageAttributes@ (default) – The filter is applied on the
    --         message attributes.
    --
    --     -   @MessageBody@ – The filter is applied on the message body.
    --
    -- -   @RawMessageDelivery@ – When set to @true@, enables raw message
    --     delivery to Amazon SQS or HTTP\/S endpoints. This eliminates the
    --     need for the endpoints to process JSON formatting, which is
    --     otherwise created for Amazon SNS metadata.
    --
    -- -   @RedrivePolicy@ – When specified, sends undeliverable messages to
    --     the specified Amazon SQS dead-letter queue. Messages that can\'t be
    --     delivered due to client errors (for example, when the subscribed
    --     endpoint is unreachable) or server errors (for example, when the
    --     service that powers the subscribed endpoint becomes unavailable) are
    --     held in the dead-letter queue for further analysis or reprocessing.
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
    --     <https://docs.aws.amazon.com/sns/latest/dg/sns-firehose-as-subscriber.html Fanout to Kinesis Data Firehose delivery streams>
    --     in the /Amazon SNS Developer Guide/.
    attributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetSubscriptionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeValue', 'setSubscriptionAttributes_attributeValue' - The new value for the attribute in JSON format.
--
-- 'subscriptionArn', 'setSubscriptionAttributes_subscriptionArn' - The ARN of the subscription to modify.
--
-- 'attributeName', 'setSubscriptionAttributes_attributeName' - A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special
-- request parameters that this action uses:
--
-- -   @DeliveryPolicy@ – The policy that defines how Amazon SNS retries
--     failed deliveries to HTTP\/S endpoints.
--
-- -   @FilterPolicy@ – The simple JSON object that lets your subscriber
--     receive only a subset of messages, rather than receiving every
--     message published to the topic.
--
-- -   @FilterPolicyScope@ – This attribute lets you choose the filtering
--     scope by using one of the following string value types:
--
--     -   @MessageAttributes@ (default) – The filter is applied on the
--         message attributes.
--
--     -   @MessageBody@ – The filter is applied on the message body.
--
-- -   @RawMessageDelivery@ – When set to @true@, enables raw message
--     delivery to Amazon SQS or HTTP\/S endpoints. This eliminates the
--     need for the endpoints to process JSON formatting, which is
--     otherwise created for Amazon SNS metadata.
--
-- -   @RedrivePolicy@ – When specified, sends undeliverable messages to
--     the specified Amazon SQS dead-letter queue. Messages that can\'t be
--     delivered due to client errors (for example, when the subscribed
--     endpoint is unreachable) or server errors (for example, when the
--     service that powers the subscribed endpoint becomes unavailable) are
--     held in the dead-letter queue for further analysis or reprocessing.
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
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-firehose-as-subscriber.html Fanout to Kinesis Data Firehose delivery streams>
--     in the /Amazon SNS Developer Guide/.
newSetSubscriptionAttributes ::
  -- | 'subscriptionArn'
  Prelude.Text ->
  -- | 'attributeName'
  Prelude.Text ->
  SetSubscriptionAttributes
newSetSubscriptionAttributes
  pSubscriptionArn_
  pAttributeName_ =
    SetSubscriptionAttributes'
      { attributeValue =
          Prelude.Nothing,
        subscriptionArn = pSubscriptionArn_,
        attributeName = pAttributeName_
      }

-- | The new value for the attribute in JSON format.
setSubscriptionAttributes_attributeValue :: Lens.Lens' SetSubscriptionAttributes (Prelude.Maybe Prelude.Text)
setSubscriptionAttributes_attributeValue = Lens.lens (\SetSubscriptionAttributes' {attributeValue} -> attributeValue) (\s@SetSubscriptionAttributes' {} a -> s {attributeValue = a} :: SetSubscriptionAttributes)

-- | The ARN of the subscription to modify.
setSubscriptionAttributes_subscriptionArn :: Lens.Lens' SetSubscriptionAttributes Prelude.Text
setSubscriptionAttributes_subscriptionArn = Lens.lens (\SetSubscriptionAttributes' {subscriptionArn} -> subscriptionArn) (\s@SetSubscriptionAttributes' {} a -> s {subscriptionArn = a} :: SetSubscriptionAttributes)

-- | A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special
-- request parameters that this action uses:
--
-- -   @DeliveryPolicy@ – The policy that defines how Amazon SNS retries
--     failed deliveries to HTTP\/S endpoints.
--
-- -   @FilterPolicy@ – The simple JSON object that lets your subscriber
--     receive only a subset of messages, rather than receiving every
--     message published to the topic.
--
-- -   @FilterPolicyScope@ – This attribute lets you choose the filtering
--     scope by using one of the following string value types:
--
--     -   @MessageAttributes@ (default) – The filter is applied on the
--         message attributes.
--
--     -   @MessageBody@ – The filter is applied on the message body.
--
-- -   @RawMessageDelivery@ – When set to @true@, enables raw message
--     delivery to Amazon SQS or HTTP\/S endpoints. This eliminates the
--     need for the endpoints to process JSON formatting, which is
--     otherwise created for Amazon SNS metadata.
--
-- -   @RedrivePolicy@ – When specified, sends undeliverable messages to
--     the specified Amazon SQS dead-letter queue. Messages that can\'t be
--     delivered due to client errors (for example, when the subscribed
--     endpoint is unreachable) or server errors (for example, when the
--     service that powers the subscribed endpoint becomes unavailable) are
--     held in the dead-letter queue for further analysis or reprocessing.
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
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-firehose-as-subscriber.html Fanout to Kinesis Data Firehose delivery streams>
--     in the /Amazon SNS Developer Guide/.
setSubscriptionAttributes_attributeName :: Lens.Lens' SetSubscriptionAttributes Prelude.Text
setSubscriptionAttributes_attributeName = Lens.lens (\SetSubscriptionAttributes' {attributeName} -> attributeName) (\s@SetSubscriptionAttributes' {} a -> s {attributeName = a} :: SetSubscriptionAttributes)

instance Core.AWSRequest SetSubscriptionAttributes where
  type
    AWSResponse SetSubscriptionAttributes =
      SetSubscriptionAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      SetSubscriptionAttributesResponse'

instance Prelude.Hashable SetSubscriptionAttributes where
  hashWithSalt _salt SetSubscriptionAttributes' {..} =
    _salt `Prelude.hashWithSalt` attributeValue
      `Prelude.hashWithSalt` subscriptionArn
      `Prelude.hashWithSalt` attributeName

instance Prelude.NFData SetSubscriptionAttributes where
  rnf SetSubscriptionAttributes' {..} =
    Prelude.rnf attributeValue
      `Prelude.seq` Prelude.rnf subscriptionArn
      `Prelude.seq` Prelude.rnf attributeName

instance Data.ToHeaders SetSubscriptionAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SetSubscriptionAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery SetSubscriptionAttributes where
  toQuery SetSubscriptionAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SetSubscriptionAttributes" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "AttributeValue" Data.=: attributeValue,
        "SubscriptionArn" Data.=: subscriptionArn,
        "AttributeName" Data.=: attributeName
      ]

-- | /See:/ 'newSetSubscriptionAttributesResponse' smart constructor.
data SetSubscriptionAttributesResponse = SetSubscriptionAttributesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetSubscriptionAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetSubscriptionAttributesResponse ::
  SetSubscriptionAttributesResponse
newSetSubscriptionAttributesResponse =
  SetSubscriptionAttributesResponse'

instance
  Prelude.NFData
    SetSubscriptionAttributesResponse
  where
  rnf _ = ()
