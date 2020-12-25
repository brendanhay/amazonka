{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.CreateNotificationSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configure Amazon WorkDocs to use Amazon SNS notifications. The endpoint receives a confirmation message, and must confirm the subscription.
--
-- For more information, see <https://docs.aws.amazon.com/workdocs/latest/developerguide/subscribe-notifications.html Subscribe to Notifications> in the /Amazon WorkDocs Developer Guide/ .
module Network.AWS.WorkDocs.CreateNotificationSubscription
  ( -- * Creating a request
    CreateNotificationSubscription (..),
    mkCreateNotificationSubscription,

    -- ** Request lenses
    cnsOrganizationId,
    cnsEndpoint,
    cnsProtocol,
    cnsSubscriptionType,

    -- * Destructuring the response
    CreateNotificationSubscriptionResponse (..),
    mkCreateNotificationSubscriptionResponse,

    -- ** Response lenses
    cnsrrsSubscription,
    cnsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkCreateNotificationSubscription' smart constructor.
data CreateNotificationSubscription = CreateNotificationSubscription'
  { -- | The ID of the organization.
    organizationId :: Types.IdType,
    -- | The endpoint to receive the notifications. If the protocol is HTTPS, the endpoint is a URL that begins with @https@ .
    endpoint :: Types.SubscriptionEndPointType,
    -- | The protocol to use. The supported value is https, which delivers JSON-encoded messages using HTTPS POST.
    protocol :: Types.SubscriptionProtocolType,
    -- | The notification type.
    subscriptionType :: Types.SubscriptionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNotificationSubscription' value with any optional fields omitted.
mkCreateNotificationSubscription ::
  -- | 'organizationId'
  Types.IdType ->
  -- | 'endpoint'
  Types.SubscriptionEndPointType ->
  -- | 'protocol'
  Types.SubscriptionProtocolType ->
  -- | 'subscriptionType'
  Types.SubscriptionType ->
  CreateNotificationSubscription
mkCreateNotificationSubscription
  organizationId
  endpoint
  protocol
  subscriptionType =
    CreateNotificationSubscription'
      { organizationId,
        endpoint,
        protocol,
        subscriptionType
      }

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnsOrganizationId :: Lens.Lens' CreateNotificationSubscription Types.IdType
cnsOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED cnsOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The endpoint to receive the notifications. If the protocol is HTTPS, the endpoint is a URL that begins with @https@ .
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnsEndpoint :: Lens.Lens' CreateNotificationSubscription Types.SubscriptionEndPointType
cnsEndpoint = Lens.field @"endpoint"
{-# DEPRECATED cnsEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The protocol to use. The supported value is https, which delivers JSON-encoded messages using HTTPS POST.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnsProtocol :: Lens.Lens' CreateNotificationSubscription Types.SubscriptionProtocolType
cnsProtocol = Lens.field @"protocol"
{-# DEPRECATED cnsProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The notification type.
--
-- /Note:/ Consider using 'subscriptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnsSubscriptionType :: Lens.Lens' CreateNotificationSubscription Types.SubscriptionType
cnsSubscriptionType = Lens.field @"subscriptionType"
{-# DEPRECATED cnsSubscriptionType "Use generic-lens or generic-optics with 'subscriptionType' instead." #-}

instance Core.FromJSON CreateNotificationSubscription where
  toJSON CreateNotificationSubscription {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Endpoint" Core..= endpoint),
            Core.Just ("Protocol" Core..= protocol),
            Core.Just ("SubscriptionType" Core..= subscriptionType)
          ]
      )

instance Core.AWSRequest CreateNotificationSubscription where
  type
    Rs CreateNotificationSubscription =
      CreateNotificationSubscriptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/organizations/" Core.<> (Core.toText organizationId)
                Core.<> ("/subscriptions")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNotificationSubscriptionResponse'
            Core.<$> (x Core..:? "Subscription") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateNotificationSubscriptionResponse' smart constructor.
data CreateNotificationSubscriptionResponse = CreateNotificationSubscriptionResponse'
  { -- | The subscription.
    subscription :: Core.Maybe Types.Subscription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNotificationSubscriptionResponse' value with any optional fields omitted.
mkCreateNotificationSubscriptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateNotificationSubscriptionResponse
mkCreateNotificationSubscriptionResponse responseStatus =
  CreateNotificationSubscriptionResponse'
    { subscription =
        Core.Nothing,
      responseStatus
    }

-- | The subscription.
--
-- /Note:/ Consider using 'subscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnsrrsSubscription :: Lens.Lens' CreateNotificationSubscriptionResponse (Core.Maybe Types.Subscription)
cnsrrsSubscription = Lens.field @"subscription"
{-# DEPRECATED cnsrrsSubscription "Use generic-lens or generic-optics with 'subscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnsrrsResponseStatus :: Lens.Lens' CreateNotificationSubscriptionResponse Core.Int
cnsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cnsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
