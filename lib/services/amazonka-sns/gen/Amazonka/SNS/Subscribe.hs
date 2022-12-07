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
-- Module      : Amazonka.SNS.Subscribe
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Subscribes an endpoint to an Amazon SNS topic. If the endpoint type is
-- HTTP\/S or email, or if the endpoint and the topic are not in the same
-- Amazon Web Services account, the endpoint owner must run the
-- @ConfirmSubscription@ action to confirm the subscription.
--
-- You call the @ConfirmSubscription@ action with the token from the
-- subscription response. Confirmation tokens are valid for three days.
--
-- This action is throttled at 100 transactions per second (TPS).
module Amazonka.SNS.Subscribe
  ( -- * Creating a Request
    Subscribe (..),
    newSubscribe,

    -- * Request Lenses
    subscribe_returnSubscriptionArn,
    subscribe_attributes,
    subscribe_endpoint,
    subscribe_topicArn,
    subscribe_protocol,

    -- * Destructuring the Response
    SubscribeResponse (..),
    newSubscribeResponse,

    -- * Response Lenses
    subscribeResponse_subscriptionArn,
    subscribeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | Input for Subscribe action.
--
-- /See:/ 'newSubscribe' smart constructor.
data Subscribe = Subscribe'
  { -- | Sets whether the response from the @Subscribe@ request includes the
    -- subscription ARN, even if the subscription is not yet confirmed.
    --
    -- If you set this parameter to @true@, the response includes the ARN in
    -- all cases, even if the subscription is not yet confirmed. In addition to
    -- the ARN for confirmed subscriptions, the response also includes the
    -- @pending subscription@ ARN value for subscriptions that aren\'t yet
    -- confirmed. A subscription becomes confirmed when the subscriber calls
    -- the @ConfirmSubscription@ action with a confirmation token.
    --
    -- The default value is @false@.
    returnSubscriptionArn :: Prelude.Maybe Prelude.Bool,
    -- | A map of attributes with their corresponding values.
    --
    -- The following lists the names, descriptions, and values of the special
    -- request parameters that the @Subscribe@ action uses:
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
    --     <https://docs.aws.amazon.com/sns/latest/dg/sns-firehose-as-subscriber.html Fanout to Kinesis Data Firehose delivery streams>
    --     in the /Amazon SNS Developer Guide/.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The endpoint that you want to receive notifications. Endpoints vary by
    -- protocol:
    --
    -- -   For the @http@ protocol, the (public) endpoint is a URL beginning
    --     with @http:\/\/@.
    --
    -- -   For the @https@ protocol, the (public) endpoint is a URL beginning
    --     with @https:\/\/@.
    --
    -- -   For the @email@ protocol, the endpoint is an email address.
    --
    -- -   For the @email-json@ protocol, the endpoint is an email address.
    --
    -- -   For the @sms@ protocol, the endpoint is a phone number of an
    --     SMS-enabled device.
    --
    -- -   For the @sqs@ protocol, the endpoint is the ARN of an Amazon SQS
    --     queue.
    --
    -- -   For the @application@ protocol, the endpoint is the EndpointArn of a
    --     mobile app and device.
    --
    -- -   For the @lambda@ protocol, the endpoint is the ARN of an Lambda
    --     function.
    --
    -- -   For the @firehose@ protocol, the endpoint is the ARN of an Amazon
    --     Kinesis Data Firehose delivery stream.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the topic you want to subscribe to.
    topicArn :: Prelude.Text,
    -- | The protocol that you want to use. Supported protocols include:
    --
    -- -   @http@ – delivery of JSON-encoded message via HTTP POST
    --
    -- -   @https@ – delivery of JSON-encoded message via HTTPS POST
    --
    -- -   @email@ – delivery of message via SMTP
    --
    -- -   @email-json@ – delivery of JSON-encoded message via SMTP
    --
    -- -   @sms@ – delivery of message via SMS
    --
    -- -   @sqs@ – delivery of JSON-encoded message to an Amazon SQS queue
    --
    -- -   @application@ – delivery of JSON-encoded message to an EndpointArn
    --     for a mobile app and device
    --
    -- -   @lambda@ – delivery of JSON-encoded message to an Lambda function
    --
    -- -   @firehose@ – delivery of JSON-encoded message to an Amazon Kinesis
    --     Data Firehose delivery stream.
    protocol :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Subscribe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnSubscriptionArn', 'subscribe_returnSubscriptionArn' - Sets whether the response from the @Subscribe@ request includes the
-- subscription ARN, even if the subscription is not yet confirmed.
--
-- If you set this parameter to @true@, the response includes the ARN in
-- all cases, even if the subscription is not yet confirmed. In addition to
-- the ARN for confirmed subscriptions, the response also includes the
-- @pending subscription@ ARN value for subscriptions that aren\'t yet
-- confirmed. A subscription becomes confirmed when the subscriber calls
-- the @ConfirmSubscription@ action with a confirmation token.
--
-- The default value is @false@.
--
-- 'attributes', 'subscribe_attributes' - A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special
-- request parameters that the @Subscribe@ action uses:
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
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-firehose-as-subscriber.html Fanout to Kinesis Data Firehose delivery streams>
--     in the /Amazon SNS Developer Guide/.
--
-- 'endpoint', 'subscribe_endpoint' - The endpoint that you want to receive notifications. Endpoints vary by
-- protocol:
--
-- -   For the @http@ protocol, the (public) endpoint is a URL beginning
--     with @http:\/\/@.
--
-- -   For the @https@ protocol, the (public) endpoint is a URL beginning
--     with @https:\/\/@.
--
-- -   For the @email@ protocol, the endpoint is an email address.
--
-- -   For the @email-json@ protocol, the endpoint is an email address.
--
-- -   For the @sms@ protocol, the endpoint is a phone number of an
--     SMS-enabled device.
--
-- -   For the @sqs@ protocol, the endpoint is the ARN of an Amazon SQS
--     queue.
--
-- -   For the @application@ protocol, the endpoint is the EndpointArn of a
--     mobile app and device.
--
-- -   For the @lambda@ protocol, the endpoint is the ARN of an Lambda
--     function.
--
-- -   For the @firehose@ protocol, the endpoint is the ARN of an Amazon
--     Kinesis Data Firehose delivery stream.
--
-- 'topicArn', 'subscribe_topicArn' - The ARN of the topic you want to subscribe to.
--
-- 'protocol', 'subscribe_protocol' - The protocol that you want to use. Supported protocols include:
--
-- -   @http@ – delivery of JSON-encoded message via HTTP POST
--
-- -   @https@ – delivery of JSON-encoded message via HTTPS POST
--
-- -   @email@ – delivery of message via SMTP
--
-- -   @email-json@ – delivery of JSON-encoded message via SMTP
--
-- -   @sms@ – delivery of message via SMS
--
-- -   @sqs@ – delivery of JSON-encoded message to an Amazon SQS queue
--
-- -   @application@ – delivery of JSON-encoded message to an EndpointArn
--     for a mobile app and device
--
-- -   @lambda@ – delivery of JSON-encoded message to an Lambda function
--
-- -   @firehose@ – delivery of JSON-encoded message to an Amazon Kinesis
--     Data Firehose delivery stream.
newSubscribe ::
  -- | 'topicArn'
  Prelude.Text ->
  -- | 'protocol'
  Prelude.Text ->
  Subscribe
newSubscribe pTopicArn_ pProtocol_ =
  Subscribe'
    { returnSubscriptionArn = Prelude.Nothing,
      attributes = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      topicArn = pTopicArn_,
      protocol = pProtocol_
    }

-- | Sets whether the response from the @Subscribe@ request includes the
-- subscription ARN, even if the subscription is not yet confirmed.
--
-- If you set this parameter to @true@, the response includes the ARN in
-- all cases, even if the subscription is not yet confirmed. In addition to
-- the ARN for confirmed subscriptions, the response also includes the
-- @pending subscription@ ARN value for subscriptions that aren\'t yet
-- confirmed. A subscription becomes confirmed when the subscriber calls
-- the @ConfirmSubscription@ action with a confirmation token.
--
-- The default value is @false@.
subscribe_returnSubscriptionArn :: Lens.Lens' Subscribe (Prelude.Maybe Prelude.Bool)
subscribe_returnSubscriptionArn = Lens.lens (\Subscribe' {returnSubscriptionArn} -> returnSubscriptionArn) (\s@Subscribe' {} a -> s {returnSubscriptionArn = a} :: Subscribe)

-- | A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special
-- request parameters that the @Subscribe@ action uses:
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
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-firehose-as-subscriber.html Fanout to Kinesis Data Firehose delivery streams>
--     in the /Amazon SNS Developer Guide/.
subscribe_attributes :: Lens.Lens' Subscribe (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
subscribe_attributes = Lens.lens (\Subscribe' {attributes} -> attributes) (\s@Subscribe' {} a -> s {attributes = a} :: Subscribe) Prelude.. Lens.mapping Lens.coerced

-- | The endpoint that you want to receive notifications. Endpoints vary by
-- protocol:
--
-- -   For the @http@ protocol, the (public) endpoint is a URL beginning
--     with @http:\/\/@.
--
-- -   For the @https@ protocol, the (public) endpoint is a URL beginning
--     with @https:\/\/@.
--
-- -   For the @email@ protocol, the endpoint is an email address.
--
-- -   For the @email-json@ protocol, the endpoint is an email address.
--
-- -   For the @sms@ protocol, the endpoint is a phone number of an
--     SMS-enabled device.
--
-- -   For the @sqs@ protocol, the endpoint is the ARN of an Amazon SQS
--     queue.
--
-- -   For the @application@ protocol, the endpoint is the EndpointArn of a
--     mobile app and device.
--
-- -   For the @lambda@ protocol, the endpoint is the ARN of an Lambda
--     function.
--
-- -   For the @firehose@ protocol, the endpoint is the ARN of an Amazon
--     Kinesis Data Firehose delivery stream.
subscribe_endpoint :: Lens.Lens' Subscribe (Prelude.Maybe Prelude.Text)
subscribe_endpoint = Lens.lens (\Subscribe' {endpoint} -> endpoint) (\s@Subscribe' {} a -> s {endpoint = a} :: Subscribe)

-- | The ARN of the topic you want to subscribe to.
subscribe_topicArn :: Lens.Lens' Subscribe Prelude.Text
subscribe_topicArn = Lens.lens (\Subscribe' {topicArn} -> topicArn) (\s@Subscribe' {} a -> s {topicArn = a} :: Subscribe)

-- | The protocol that you want to use. Supported protocols include:
--
-- -   @http@ – delivery of JSON-encoded message via HTTP POST
--
-- -   @https@ – delivery of JSON-encoded message via HTTPS POST
--
-- -   @email@ – delivery of message via SMTP
--
-- -   @email-json@ – delivery of JSON-encoded message via SMTP
--
-- -   @sms@ – delivery of message via SMS
--
-- -   @sqs@ – delivery of JSON-encoded message to an Amazon SQS queue
--
-- -   @application@ – delivery of JSON-encoded message to an EndpointArn
--     for a mobile app and device
--
-- -   @lambda@ – delivery of JSON-encoded message to an Lambda function
--
-- -   @firehose@ – delivery of JSON-encoded message to an Amazon Kinesis
--     Data Firehose delivery stream.
subscribe_protocol :: Lens.Lens' Subscribe Prelude.Text
subscribe_protocol = Lens.lens (\Subscribe' {protocol} -> protocol) (\s@Subscribe' {} a -> s {protocol = a} :: Subscribe)

instance Core.AWSRequest Subscribe where
  type AWSResponse Subscribe = SubscribeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SubscribeResult"
      ( \s h x ->
          SubscribeResponse'
            Prelude.<$> (x Data..@? "SubscriptionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Subscribe where
  hashWithSalt _salt Subscribe' {..} =
    _salt `Prelude.hashWithSalt` returnSubscriptionArn
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` topicArn
      `Prelude.hashWithSalt` protocol

instance Prelude.NFData Subscribe where
  rnf Subscribe' {..} =
    Prelude.rnf returnSubscriptionArn
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf topicArn
      `Prelude.seq` Prelude.rnf protocol

instance Data.ToHeaders Subscribe where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath Subscribe where
  toPath = Prelude.const "/"

instance Data.ToQuery Subscribe where
  toQuery Subscribe' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("Subscribe" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "ReturnSubscriptionArn"
          Data.=: returnSubscriptionArn,
        "Attributes"
          Data.=: Data.toQuery
            ( Data.toQueryMap "entry" "key" "value"
                Prelude.<$> attributes
            ),
        "Endpoint" Data.=: endpoint,
        "TopicArn" Data.=: topicArn,
        "Protocol" Data.=: protocol
      ]

-- | Response for Subscribe action.
--
-- /See:/ 'newSubscribeResponse' smart constructor.
data SubscribeResponse = SubscribeResponse'
  { -- | The ARN of the subscription if it is confirmed, or the string \"pending
    -- confirmation\" if the subscription requires confirmation. However, if
    -- the API request parameter @ReturnSubscriptionArn@ is true, then the
    -- value is always the subscription ARN, even if the subscription requires
    -- confirmation.
    subscriptionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubscribeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionArn', 'subscribeResponse_subscriptionArn' - The ARN of the subscription if it is confirmed, or the string \"pending
-- confirmation\" if the subscription requires confirmation. However, if
-- the API request parameter @ReturnSubscriptionArn@ is true, then the
-- value is always the subscription ARN, even if the subscription requires
-- confirmation.
--
-- 'httpStatus', 'subscribeResponse_httpStatus' - The response's http status code.
newSubscribeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SubscribeResponse
newSubscribeResponse pHttpStatus_ =
  SubscribeResponse'
    { subscriptionArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the subscription if it is confirmed, or the string \"pending
-- confirmation\" if the subscription requires confirmation. However, if
-- the API request parameter @ReturnSubscriptionArn@ is true, then the
-- value is always the subscription ARN, even if the subscription requires
-- confirmation.
subscribeResponse_subscriptionArn :: Lens.Lens' SubscribeResponse (Prelude.Maybe Prelude.Text)
subscribeResponse_subscriptionArn = Lens.lens (\SubscribeResponse' {subscriptionArn} -> subscriptionArn) (\s@SubscribeResponse' {} a -> s {subscriptionArn = a} :: SubscribeResponse)

-- | The response's http status code.
subscribeResponse_httpStatus :: Lens.Lens' SubscribeResponse Prelude.Int
subscribeResponse_httpStatus = Lens.lens (\SubscribeResponse' {httpStatus} -> httpStatus) (\s@SubscribeResponse' {} a -> s {httpStatus = a} :: SubscribeResponse)

instance Prelude.NFData SubscribeResponse where
  rnf SubscribeResponse' {..} =
    Prelude.rnf subscriptionArn
      `Prelude.seq` Prelude.rnf httpStatus
