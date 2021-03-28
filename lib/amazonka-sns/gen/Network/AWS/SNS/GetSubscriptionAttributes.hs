{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.GetSubscriptionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the properties of a subscription.
module Network.AWS.SNS.GetSubscriptionAttributes
    (
    -- * Creating a request
      GetSubscriptionAttributes (..)
    , mkGetSubscriptionAttributes
    -- ** Request lenses
    , gsaSubscriptionArn

    -- * Destructuring the response
    , GetSubscriptionAttributesResponse (..)
    , mkGetSubscriptionAttributesResponse
    -- ** Response lenses
    , gsarrsAttributes
    , gsarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for GetSubscriptionAttributes.
--
-- /See:/ 'mkGetSubscriptionAttributes' smart constructor.
newtype GetSubscriptionAttributes = GetSubscriptionAttributes'
  { subscriptionArn :: Types.SubscriptionArn
    -- ^ The ARN of the subscription whose properties you want to get.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSubscriptionAttributes' value with any optional fields omitted.
mkGetSubscriptionAttributes
    :: Types.SubscriptionArn -- ^ 'subscriptionArn'
    -> GetSubscriptionAttributes
mkGetSubscriptionAttributes subscriptionArn
  = GetSubscriptionAttributes'{subscriptionArn}

-- | The ARN of the subscription whose properties you want to get.
--
-- /Note:/ Consider using 'subscriptionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsaSubscriptionArn :: Lens.Lens' GetSubscriptionAttributes Types.SubscriptionArn
gsaSubscriptionArn = Lens.field @"subscriptionArn"
{-# INLINEABLE gsaSubscriptionArn #-}
{-# DEPRECATED subscriptionArn "Use generic-lens or generic-optics with 'subscriptionArn' instead"  #-}

instance Core.ToQuery GetSubscriptionAttributes where
        toQuery GetSubscriptionAttributes{..}
          = Core.toQueryPair "Action"
              ("GetSubscriptionAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<> Core.toQueryPair "SubscriptionArn" subscriptionArn

instance Core.ToHeaders GetSubscriptionAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetSubscriptionAttributes where
        type Rs GetSubscriptionAttributes =
             GetSubscriptionAttributesResponse
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
          = Response.receiveXMLWrapper "GetSubscriptionAttributesResult"
              (\ s h x ->
                 GetSubscriptionAttributesResponse' Core.<$>
                   (x Core..@? "Attributes" Core..<@>
                      Core.parseXMLMap "entry" "key" "value")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Response for GetSubscriptionAttributes action.
--
-- /See:/ 'mkGetSubscriptionAttributesResponse' smart constructor.
data GetSubscriptionAttributesResponse = GetSubscriptionAttributesResponse'
  { attributes :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue)
    -- ^ A map of the subscription's attributes. Attributes in this map include the following:
--
--
--     * @ConfirmationWasAuthenticated@ – @true@ if the subscription confirmation request was authenticated.
--
--
--     * @DeliveryPolicy@ – The JSON serialization of the subscription's delivery policy.
--
--
--     * @EffectiveDeliveryPolicy@ – The JSON serialization of the effective delivery policy that takes into account the topic delivery policy and account system defaults.
--
--
--     * @FilterPolicy@ – The filter policy JSON that is assigned to the subscription. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/sns-message-filtering.html Amazon SNS Message Filtering> in the /Amazon SNS Developer Guide/ .
--
--
--     * @Owner@ – The AWS account ID of the subscription's owner.
--
--
--     * @PendingConfirmation@ – @true@ if the subscription hasn't been confirmed. To confirm a pending subscription, call the @ConfirmSubscription@ action with a confirmation token.
--
--
--     * @RawMessageDelivery@ – @true@ if raw message delivery is enabled for the subscription. Raw messages are free of JSON formatting and can be sent to HTTP/S and Amazon SQS endpoints.
--
--
--     * @RedrivePolicy@ – When specified, sends undeliverable messages to the specified Amazon SQS dead-letter queue. Messages that can't be delivered due to client errors (for example, when the subscribed endpoint is unreachable) or server errors (for example, when the service that powers the subscribed endpoint becomes unavailable) are held in the dead-letter queue for further analysis or reprocessing.
--
--
--     * @SubscriptionArn@ – The subscription's ARN.
--
--
--     * @TopicArn@ – The topic ARN that the subscription is associated with.
--
--
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSubscriptionAttributesResponse' value with any optional fields omitted.
mkGetSubscriptionAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSubscriptionAttributesResponse
mkGetSubscriptionAttributesResponse responseStatus
  = GetSubscriptionAttributesResponse'{attributes = Core.Nothing,
                                       responseStatus}

-- | A map of the subscription's attributes. Attributes in this map include the following:
--
--
--     * @ConfirmationWasAuthenticated@ – @true@ if the subscription confirmation request was authenticated.
--
--
--     * @DeliveryPolicy@ – The JSON serialization of the subscription's delivery policy.
--
--
--     * @EffectiveDeliveryPolicy@ – The JSON serialization of the effective delivery policy that takes into account the topic delivery policy and account system defaults.
--
--
--     * @FilterPolicy@ – The filter policy JSON that is assigned to the subscription. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/sns-message-filtering.html Amazon SNS Message Filtering> in the /Amazon SNS Developer Guide/ .
--
--
--     * @Owner@ – The AWS account ID of the subscription's owner.
--
--
--     * @PendingConfirmation@ – @true@ if the subscription hasn't been confirmed. To confirm a pending subscription, call the @ConfirmSubscription@ action with a confirmation token.
--
--
--     * @RawMessageDelivery@ – @true@ if raw message delivery is enabled for the subscription. Raw messages are free of JSON formatting and can be sent to HTTP/S and Amazon SQS endpoints.
--
--
--     * @RedrivePolicy@ – When specified, sends undeliverable messages to the specified Amazon SQS dead-letter queue. Messages that can't be delivered due to client errors (for example, when the subscribed endpoint is unreachable) or server errors (for example, when the service that powers the subscribed endpoint becomes unavailable) are held in the dead-letter queue for further analysis or reprocessing.
--
--
--     * @SubscriptionArn@ – The subscription's ARN.
--
--
--     * @TopicArn@ – The topic ARN that the subscription is associated with.
--
--
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsarrsAttributes :: Lens.Lens' GetSubscriptionAttributesResponse (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
gsarrsAttributes = Lens.field @"attributes"
{-# INLINEABLE gsarrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsarrsResponseStatus :: Lens.Lens' GetSubscriptionAttributesResponse Core.Int
gsarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
