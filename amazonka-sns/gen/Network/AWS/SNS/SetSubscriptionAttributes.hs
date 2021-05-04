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
-- Module      : Network.AWS.SNS.SetSubscriptionAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a subscription owner to set an attribute of the subscription to a
-- new value.
module Network.AWS.SNS.SetSubscriptionAttributes
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

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
    --     <https://docs.aws.amazon.com/sns/latest/dg/sns-kinesis-subscriber.html Fanout to Kinesis Data Firehose delivery streams>
    --     in the /Amazon SNS Developer Guide/.
    attributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-kinesis-subscriber.html Fanout to Kinesis Data Firehose delivery streams>
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
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-kinesis-subscriber.html Fanout to Kinesis Data Firehose delivery streams>
--     in the /Amazon SNS Developer Guide/.
setSubscriptionAttributes_attributeName :: Lens.Lens' SetSubscriptionAttributes Prelude.Text
setSubscriptionAttributes_attributeName = Lens.lens (\SetSubscriptionAttributes' {attributeName} -> attributeName) (\s@SetSubscriptionAttributes' {} a -> s {attributeName = a} :: SetSubscriptionAttributes)

instance Prelude.AWSRequest SetSubscriptionAttributes where
  type
    Rs SetSubscriptionAttributes =
      SetSubscriptionAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      SetSubscriptionAttributesResponse'

instance Prelude.Hashable SetSubscriptionAttributes

instance Prelude.NFData SetSubscriptionAttributes

instance Prelude.ToHeaders SetSubscriptionAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath SetSubscriptionAttributes where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SetSubscriptionAttributes where
  toQuery SetSubscriptionAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("SetSubscriptionAttributes" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-03-31" :: Prelude.ByteString),
        "AttributeValue" Prelude.=: attributeValue,
        "SubscriptionArn" Prelude.=: subscriptionArn,
        "AttributeName" Prelude.=: attributeName
      ]

-- | /See:/ 'newSetSubscriptionAttributesResponse' smart constructor.
data SetSubscriptionAttributesResponse = SetSubscriptionAttributesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
