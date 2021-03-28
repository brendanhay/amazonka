{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      Subscribe (..)
    , mkSubscribe
    -- ** Request lenses
    , sfTopicArn
    , sfProtocol
    , sfAttributes
    , sfEndpoint
    , sfReturnSubscriptionArn

    -- * Destructuring the response
    , SubscribeResponse (..)
    , mkSubscribeResponse
    -- ** Response lenses
    , srrsSubscriptionArn
    , srrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for Subscribe action.
--
-- /See:/ 'mkSubscribe' smart constructor.
data Subscribe = Subscribe'
  { topicArn :: Types.TopicARN
    -- ^ The ARN of the topic you want to subscribe to.
  , protocol :: Types.Protocol
    -- ^ The protocol you want to use. Supported protocols include:
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
  , attributes :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue)
    -- ^ A map of attributes with their corresponding values.
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
  , endpoint :: Core.Maybe Types.Endpoint
    -- ^ The endpoint that you want to receive notifications. Endpoints vary by protocol:
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
  , returnSubscriptionArn :: Core.Maybe Core.Bool
    -- ^ Sets whether the response from the @Subscribe@ request includes the subscription ARN, even if the subscription is not yet confirmed.
--
-- If you set this parameter to @true@ , the response includes the ARN in all cases, even if the subscription is not yet confirmed. In addition to the ARN for confirmed subscriptions, the response also includes the @pending subscription@ ARN value for subscriptions that aren't yet confirmed. A subscription becomes confirmed when the subscriber calls the @ConfirmSubscription@ action with a confirmation token.
--
-- The default value is @false@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Subscribe' value with any optional fields omitted.
mkSubscribe
    :: Types.TopicARN -- ^ 'topicArn'
    -> Types.Protocol -- ^ 'protocol'
    -> Subscribe
mkSubscribe topicArn protocol
  = Subscribe'{topicArn, protocol, attributes = Core.Nothing,
               endpoint = Core.Nothing, returnSubscriptionArn = Core.Nothing}

-- | The ARN of the topic you want to subscribe to.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfTopicArn :: Lens.Lens' Subscribe Types.TopicARN
sfTopicArn = Lens.field @"topicArn"
{-# INLINEABLE sfTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

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
sfProtocol :: Lens.Lens' Subscribe Types.Protocol
sfProtocol = Lens.field @"protocol"
{-# INLINEABLE sfProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

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
sfAttributes :: Lens.Lens' Subscribe (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
sfAttributes = Lens.field @"attributes"
{-# INLINEABLE sfAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

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
sfEndpoint :: Lens.Lens' Subscribe (Core.Maybe Types.Endpoint)
sfEndpoint = Lens.field @"endpoint"
{-# INLINEABLE sfEndpoint #-}
{-# DEPRECATED endpoint "Use generic-lens or generic-optics with 'endpoint' instead"  #-}

-- | Sets whether the response from the @Subscribe@ request includes the subscription ARN, even if the subscription is not yet confirmed.
--
-- If you set this parameter to @true@ , the response includes the ARN in all cases, even if the subscription is not yet confirmed. In addition to the ARN for confirmed subscriptions, the response also includes the @pending subscription@ ARN value for subscriptions that aren't yet confirmed. A subscription becomes confirmed when the subscriber calls the @ConfirmSubscription@ action with a confirmation token.
--
-- The default value is @false@ .
--
-- /Note:/ Consider using 'returnSubscriptionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfReturnSubscriptionArn :: Lens.Lens' Subscribe (Core.Maybe Core.Bool)
sfReturnSubscriptionArn = Lens.field @"returnSubscriptionArn"
{-# INLINEABLE sfReturnSubscriptionArn #-}
{-# DEPRECATED returnSubscriptionArn "Use generic-lens or generic-optics with 'returnSubscriptionArn' instead"  #-}

instance Core.ToQuery Subscribe where
        toQuery Subscribe{..}
          = Core.toQueryPair "Action" ("Subscribe" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<> Core.toQueryPair "TopicArn" topicArn
              Core.<> Core.toQueryPair "Protocol" protocol
              Core.<>
              Core.toQueryPair "Attributes"
                (Core.maybe Core.mempty (Core.toQueryMap "entry" "key" "value")
                   attributes)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Endpoint") endpoint
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReturnSubscriptionArn")
                returnSubscriptionArn

instance Core.ToHeaders Subscribe where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest Subscribe where
        type Rs Subscribe = SubscribeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "SubscribeResult"
              (\ s h x ->
                 SubscribeResponse' Core.<$>
                   (x Core..@? "SubscriptionArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Response for Subscribe action.
--
-- /See:/ 'mkSubscribeResponse' smart constructor.
data SubscribeResponse = SubscribeResponse'
  { subscriptionArn :: Core.Maybe Types.SubscriptionArn
    -- ^ The ARN of the subscription if it is confirmed, or the string "pending confirmation" if the subscription requires confirmation. However, if the API request parameter @ReturnSubscriptionArn@ is true, then the value is always the subscription ARN, even if the subscription requires confirmation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubscribeResponse' value with any optional fields omitted.
mkSubscribeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SubscribeResponse
mkSubscribeResponse responseStatus
  = SubscribeResponse'{subscriptionArn = Core.Nothing,
                       responseStatus}

-- | The ARN of the subscription if it is confirmed, or the string "pending confirmation" if the subscription requires confirmation. However, if the API request parameter @ReturnSubscriptionArn@ is true, then the value is always the subscription ARN, even if the subscription requires confirmation.
--
-- /Note:/ Consider using 'subscriptionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsSubscriptionArn :: Lens.Lens' SubscribeResponse (Core.Maybe Types.SubscriptionArn)
srrsSubscriptionArn = Lens.field @"subscriptionArn"
{-# INLINEABLE srrsSubscriptionArn #-}
{-# DEPRECATED subscriptionArn "Use generic-lens or generic-optics with 'subscriptionArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsResponseStatus :: Lens.Lens' SubscribeResponse Core.Int
srrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
