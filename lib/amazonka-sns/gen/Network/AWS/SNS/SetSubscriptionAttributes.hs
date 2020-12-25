{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.SetSubscriptionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a subscription owner to set an attribute of the subscription to a new value.
module Network.AWS.SNS.SetSubscriptionAttributes
  ( -- * Creating a request
    SetSubscriptionAttributes (..),
    mkSetSubscriptionAttributes,

    -- ** Request lenses
    ssaSubscriptionArn,
    ssaAttributeName,
    ssaAttributeValue,

    -- * Destructuring the response
    SetSubscriptionAttributesResponse (..),
    mkSetSubscriptionAttributesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for SetSubscriptionAttributes action.
--
-- /See:/ 'mkSetSubscriptionAttributes' smart constructor.
data SetSubscriptionAttributes = SetSubscriptionAttributes'
  { -- | The ARN of the subscription to modify.
    subscriptionArn :: Types.SubscriptionArn,
    -- | A map of attributes with their corresponding values.
    --
    -- The following lists the names, descriptions, and values of the special request parameters that this action uses:
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
    attributeName :: Types.AttributeName,
    -- | The new value for the attribute in JSON format.
    attributeValue :: Core.Maybe Types.AttributeValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetSubscriptionAttributes' value with any optional fields omitted.
mkSetSubscriptionAttributes ::
  -- | 'subscriptionArn'
  Types.SubscriptionArn ->
  -- | 'attributeName'
  Types.AttributeName ->
  SetSubscriptionAttributes
mkSetSubscriptionAttributes subscriptionArn attributeName =
  SetSubscriptionAttributes'
    { subscriptionArn,
      attributeName,
      attributeValue = Core.Nothing
    }

-- | The ARN of the subscription to modify.
--
-- /Note:/ Consider using 'subscriptionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssaSubscriptionArn :: Lens.Lens' SetSubscriptionAttributes Types.SubscriptionArn
ssaSubscriptionArn = Lens.field @"subscriptionArn"
{-# DEPRECATED ssaSubscriptionArn "Use generic-lens or generic-optics with 'subscriptionArn' instead." #-}

-- | A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special request parameters that this action uses:
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
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssaAttributeName :: Lens.Lens' SetSubscriptionAttributes Types.AttributeName
ssaAttributeName = Lens.field @"attributeName"
{-# DEPRECATED ssaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The new value for the attribute in JSON format.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssaAttributeValue :: Lens.Lens' SetSubscriptionAttributes (Core.Maybe Types.AttributeValue)
ssaAttributeValue = Lens.field @"attributeValue"
{-# DEPRECATED ssaAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

instance Core.AWSRequest SetSubscriptionAttributes where
  type
    Rs SetSubscriptionAttributes =
      SetSubscriptionAttributesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "SetSubscriptionAttributes")
                Core.<> (Core.pure ("Version", "2010-03-31"))
                Core.<> (Core.toQueryValue "SubscriptionArn" subscriptionArn)
                Core.<> (Core.toQueryValue "AttributeName" attributeName)
                Core.<> (Core.toQueryValue "AttributeValue" Core.<$> attributeValue)
            )
      }
  response = Response.receiveNull SetSubscriptionAttributesResponse'

-- | /See:/ 'mkSetSubscriptionAttributesResponse' smart constructor.
data SetSubscriptionAttributesResponse = SetSubscriptionAttributesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetSubscriptionAttributesResponse' value with any optional fields omitted.
mkSetSubscriptionAttributesResponse ::
  SetSubscriptionAttributesResponse
mkSetSubscriptionAttributesResponse =
  SetSubscriptionAttributesResponse'
