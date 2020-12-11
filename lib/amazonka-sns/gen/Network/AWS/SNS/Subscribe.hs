{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Subscribe
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Subscribes an endpoint to an Amazon SNS topic. If the endpoint type is HTTP/S or email, or if the endpoint and the topic are not in the same AWS account, the endpoint owner must run the @ConfirmSubscription@ action to confirm the subscription.
--
-- You call the @ConfirmSubscription@ action with the token from the subscription response. Confirmation tokens are valid for three days.
-- This action is throttled at 100 transactions per second (TPS).
module Network.AWS.SNS.Subscribe
  ( -- * Creating a request
    Subscribe (..),
    mkSubscribe,

    -- ** Request lenses
    subReturnSubscriptionARN,
    subAttributes,
    subEndpoint,
    subTopicARN,
    subProtocol,

    -- * Destructuring the response
    SubscribeResponse (..),
    mkSubscribeResponse,

    -- ** Response lenses
    srsSubscriptionARN,
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for Subscribe action.
--
-- /See:/ 'mkSubscribe' smart constructor.
data Subscribe = Subscribe'
  { returnSubscriptionARN ::
      Lude.Maybe Lude.Bool,
    attributes :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    endpoint :: Lude.Maybe Lude.Text,
    topicARN :: Lude.Text,
    protocol :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Subscribe' with the minimum fields required to make a request.
--
-- * 'attributes' - A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special request parameters that the @SetTopicAttributes@ action uses:
--
--     * @DeliveryPolicy@ – The policy that defines how Amazon SNS retries failed deliveries to HTTP/S endpoints.
--
--
--     * @FilterPolicy@ – The simple JSON object that lets your subscriber receive only a subset of messages, rather than receiving every message published to the topic.
--
--
--     * @RawMessageDelivery@ – When set to @true@ , enables raw message delivery to Amazon SQS or HTTP/S endpoints. This eliminates the need for the endpoints to process JSON formatting, which is otherwise created for Amazon SNS metadata.
--
--
--     * @RedrivePolicy@ – When specified, sends undeliverable messages to the specified Amazon SQS dead-letter queue. Messages that can't be delivered due to client errors (for example, when the subscribed endpoint is unreachable) or server errors (for example, when the service that powers the subscribed endpoint becomes unavailable) are held in the dead-letter queue for further analysis or reprocessing.
--
--
-- * 'endpoint' - The endpoint that you want to receive notifications. Endpoints vary by protocol:
--
--
--     * For the @http@ protocol, the (public) endpoint is a URL beginning with @http://@
--
--
--     * For the @https@ protocol, the (public) endpoint is a URL beginning with @https://@
--
--
--     * For the @email@ protocol, the endpoint is an email address
--
--
--     * For the @email-json@ protocol, the endpoint is an email address
--
--
--     * For the @sms@ protocol, the endpoint is a phone number of an SMS-enabled device
--
--
--     * For the @sqs@ protocol, the endpoint is the ARN of an Amazon SQS queue
--
--
--     * For the @application@ protocol, the endpoint is the EndpointArn of a mobile app and device.
--
--
--     * For the @lambda@ protocol, the endpoint is the ARN of an Amazon Lambda function.
--
--
-- * 'protocol' - The protocol you want to use. Supported protocols include:
--
--
--     * @http@ – delivery of JSON-encoded message via HTTP POST
--
--
--     * @https@ – delivery of JSON-encoded message via HTTPS POST
--
--
--     * @email@ – delivery of message via SMTP
--
--
--     * @email-json@ – delivery of JSON-encoded message via SMTP
--
--
--     * @sms@ – delivery of message via SMS
--
--
--     * @sqs@ – delivery of JSON-encoded message to an Amazon SQS queue
--
--
--     * @application@ – delivery of JSON-encoded message to an EndpointArn for a mobile app and device.
--
--
--     * @lambda@ – delivery of JSON-encoded message to an Amazon Lambda function.
--
--
-- * 'returnSubscriptionARN' - Sets whether the response from the @Subscribe@ request includes the subscription ARN, even if the subscription is not yet confirmed.
--
-- If you set this parameter to @true@ , the response includes the ARN in all cases, even if the subscription is not yet confirmed. In addition to the ARN for confirmed subscriptions, the response also includes the @pending subscription@ ARN value for subscriptions that aren't yet confirmed. A subscription becomes confirmed when the subscriber calls the @ConfirmSubscription@ action with a confirmation token.
--
-- The default value is @false@ .
-- * 'topicARN' - The ARN of the topic you want to subscribe to.
mkSubscribe ::
  -- | 'topicARN'
  Lude.Text ->
  -- | 'protocol'
  Lude.Text ->
  Subscribe
mkSubscribe pTopicARN_ pProtocol_ =
  Subscribe'
    { returnSubscriptionARN = Lude.Nothing,
      attributes = Lude.Nothing,
      endpoint = Lude.Nothing,
      topicARN = pTopicARN_,
      protocol = pProtocol_
    }

-- | Sets whether the response from the @Subscribe@ request includes the subscription ARN, even if the subscription is not yet confirmed.
--
-- If you set this parameter to @true@ , the response includes the ARN in all cases, even if the subscription is not yet confirmed. In addition to the ARN for confirmed subscriptions, the response also includes the @pending subscription@ ARN value for subscriptions that aren't yet confirmed. A subscription becomes confirmed when the subscriber calls the @ConfirmSubscription@ action with a confirmation token.
--
-- The default value is @false@ .
--
-- /Note:/ Consider using 'returnSubscriptionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subReturnSubscriptionARN :: Lens.Lens' Subscribe (Lude.Maybe Lude.Bool)
subReturnSubscriptionARN = Lens.lens (returnSubscriptionARN :: Subscribe -> Lude.Maybe Lude.Bool) (\s a -> s {returnSubscriptionARN = a} :: Subscribe)
{-# DEPRECATED subReturnSubscriptionARN "Use generic-lens or generic-optics with 'returnSubscriptionARN' instead." #-}

-- | A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special request parameters that the @SetTopicAttributes@ action uses:
--
--     * @DeliveryPolicy@ – The policy that defines how Amazon SNS retries failed deliveries to HTTP/S endpoints.
--
--
--     * @FilterPolicy@ – The simple JSON object that lets your subscriber receive only a subset of messages, rather than receiving every message published to the topic.
--
--
--     * @RawMessageDelivery@ – When set to @true@ , enables raw message delivery to Amazon SQS or HTTP/S endpoints. This eliminates the need for the endpoints to process JSON formatting, which is otherwise created for Amazon SNS metadata.
--
--
--     * @RedrivePolicy@ – When specified, sends undeliverable messages to the specified Amazon SQS dead-letter queue. Messages that can't be delivered due to client errors (for example, when the subscribed endpoint is unreachable) or server errors (for example, when the service that powers the subscribed endpoint becomes unavailable) are held in the dead-letter queue for further analysis or reprocessing.
--
--
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subAttributes :: Lens.Lens' Subscribe (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
subAttributes = Lens.lens (attributes :: Subscribe -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: Subscribe)
{-# DEPRECATED subAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The endpoint that you want to receive notifications. Endpoints vary by protocol:
--
--
--     * For the @http@ protocol, the (public) endpoint is a URL beginning with @http://@
--
--
--     * For the @https@ protocol, the (public) endpoint is a URL beginning with @https://@
--
--
--     * For the @email@ protocol, the endpoint is an email address
--
--
--     * For the @email-json@ protocol, the endpoint is an email address
--
--
--     * For the @sms@ protocol, the endpoint is a phone number of an SMS-enabled device
--
--
--     * For the @sqs@ protocol, the endpoint is the ARN of an Amazon SQS queue
--
--
--     * For the @application@ protocol, the endpoint is the EndpointArn of a mobile app and device.
--
--
--     * For the @lambda@ protocol, the endpoint is the ARN of an Amazon Lambda function.
--
--
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subEndpoint :: Lens.Lens' Subscribe (Lude.Maybe Lude.Text)
subEndpoint = Lens.lens (endpoint :: Subscribe -> Lude.Maybe Lude.Text) (\s a -> s {endpoint = a} :: Subscribe)
{-# DEPRECATED subEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The ARN of the topic you want to subscribe to.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subTopicARN :: Lens.Lens' Subscribe Lude.Text
subTopicARN = Lens.lens (topicARN :: Subscribe -> Lude.Text) (\s a -> s {topicARN = a} :: Subscribe)
{-# DEPRECATED subTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The protocol you want to use. Supported protocols include:
--
--
--     * @http@ – delivery of JSON-encoded message via HTTP POST
--
--
--     * @https@ – delivery of JSON-encoded message via HTTPS POST
--
--
--     * @email@ – delivery of message via SMTP
--
--
--     * @email-json@ – delivery of JSON-encoded message via SMTP
--
--
--     * @sms@ – delivery of message via SMS
--
--
--     * @sqs@ – delivery of JSON-encoded message to an Amazon SQS queue
--
--
--     * @application@ – delivery of JSON-encoded message to an EndpointArn for a mobile app and device.
--
--
--     * @lambda@ – delivery of JSON-encoded message to an Amazon Lambda function.
--
--
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subProtocol :: Lens.Lens' Subscribe Lude.Text
subProtocol = Lens.lens (protocol :: Subscribe -> Lude.Text) (\s a -> s {protocol = a} :: Subscribe)
{-# DEPRECATED subProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

instance Lude.AWSRequest Subscribe where
  type Rs Subscribe = SubscribeResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "SubscribeResult"
      ( \s h x ->
          SubscribeResponse'
            Lude.<$> (x Lude..@? "SubscriptionArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders Subscribe where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath Subscribe where
  toPath = Lude.const "/"

instance Lude.ToQuery Subscribe where
  toQuery Subscribe' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("Subscribe" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "ReturnSubscriptionArn" Lude.=: returnSubscriptionARN,
        "Attributes"
          Lude.=: Lude.toQuery
            (Lude.toQueryMap "entry" "key" "value" Lude.<$> attributes),
        "Endpoint" Lude.=: endpoint,
        "TopicArn" Lude.=: topicARN,
        "Protocol" Lude.=: protocol
      ]

-- | Response for Subscribe action.
--
-- /See:/ 'mkSubscribeResponse' smart constructor.
data SubscribeResponse = SubscribeResponse'
  { subscriptionARN ::
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

-- | Creates a value of 'SubscribeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'subscriptionARN' - The ARN of the subscription if it is confirmed, or the string "pending confirmation" if the subscription requires confirmation. However, if the API request parameter @ReturnSubscriptionArn@ is true, then the value is always the subscription ARN, even if the subscription requires confirmation.
mkSubscribeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SubscribeResponse
mkSubscribeResponse pResponseStatus_ =
  SubscribeResponse'
    { subscriptionARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the subscription if it is confirmed, or the string "pending confirmation" if the subscription requires confirmation. However, if the API request parameter @ReturnSubscriptionArn@ is true, then the value is always the subscription ARN, even if the subscription requires confirmation.
--
-- /Note:/ Consider using 'subscriptionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsSubscriptionARN :: Lens.Lens' SubscribeResponse (Lude.Maybe Lude.Text)
srsSubscriptionARN = Lens.lens (subscriptionARN :: SubscribeResponse -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionARN = a} :: SubscribeResponse)
{-# DEPRECATED srsSubscriptionARN "Use generic-lens or generic-optics with 'subscriptionARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' SubscribeResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: SubscribeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SubscribeResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
