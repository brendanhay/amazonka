{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.UpdateAnomalySubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing cost anomaly monitor subscription.
module Network.AWS.CostExplorer.UpdateAnomalySubscription
  ( -- * Creating a request
    UpdateAnomalySubscription (..),
    mkUpdateAnomalySubscription,

    -- ** Request lenses
    uasSubscriptionArn,
    uasFrequency,
    uasMonitorArnList,
    uasSubscribers,
    uasSubscriptionName,
    uasThreshold,

    -- * Destructuring the response
    UpdateAnomalySubscriptionResponse (..),
    mkUpdateAnomalySubscriptionResponse,

    -- ** Response lenses
    uasrrsSubscriptionArn,
    uasrrsResponseStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateAnomalySubscription' smart constructor.
data UpdateAnomalySubscription = UpdateAnomalySubscription'
  { -- | A cost anomaly subscription Amazon Resource Name (ARN).
    subscriptionArn :: Types.GenericString,
    -- | The update to the frequency value at which subscribers will receive notifications.
    frequency :: Core.Maybe Types.AnomalySubscriptionFrequency,
    -- | A list of cost anomaly subscription ARNs.
    monitorArnList :: Core.Maybe [Types.Value],
    -- | The update to the subscriber list.
    subscribers :: Core.Maybe [Types.Subscriber],
    -- | The subscription's new name.
    subscriptionName :: Core.Maybe Types.GenericString,
    -- | The update to the threshold value for receiving notifications.
    threshold :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAnomalySubscription' value with any optional fields omitted.
mkUpdateAnomalySubscription ::
  -- | 'subscriptionArn'
  Types.GenericString ->
  UpdateAnomalySubscription
mkUpdateAnomalySubscription subscriptionArn =
  UpdateAnomalySubscription'
    { subscriptionArn,
      frequency = Core.Nothing,
      monitorArnList = Core.Nothing,
      subscribers = Core.Nothing,
      subscriptionName = Core.Nothing,
      threshold = Core.Nothing
    }

-- | A cost anomaly subscription Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'subscriptionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasSubscriptionArn :: Lens.Lens' UpdateAnomalySubscription Types.GenericString
uasSubscriptionArn = Lens.field @"subscriptionArn"
{-# DEPRECATED uasSubscriptionArn "Use generic-lens or generic-optics with 'subscriptionArn' instead." #-}

-- | The update to the frequency value at which subscribers will receive notifications.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasFrequency :: Lens.Lens' UpdateAnomalySubscription (Core.Maybe Types.AnomalySubscriptionFrequency)
uasFrequency = Lens.field @"frequency"
{-# DEPRECATED uasFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | A list of cost anomaly subscription ARNs.
--
-- /Note:/ Consider using 'monitorArnList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasMonitorArnList :: Lens.Lens' UpdateAnomalySubscription (Core.Maybe [Types.Value])
uasMonitorArnList = Lens.field @"monitorArnList"
{-# DEPRECATED uasMonitorArnList "Use generic-lens or generic-optics with 'monitorArnList' instead." #-}

-- | The update to the subscriber list.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasSubscribers :: Lens.Lens' UpdateAnomalySubscription (Core.Maybe [Types.Subscriber])
uasSubscribers = Lens.field @"subscribers"
{-# DEPRECATED uasSubscribers "Use generic-lens or generic-optics with 'subscribers' instead." #-}

-- | The subscription's new name.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasSubscriptionName :: Lens.Lens' UpdateAnomalySubscription (Core.Maybe Types.GenericString)
uasSubscriptionName = Lens.field @"subscriptionName"
{-# DEPRECATED uasSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

-- | The update to the threshold value for receiving notifications.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasThreshold :: Lens.Lens' UpdateAnomalySubscription (Core.Maybe Core.Double)
uasThreshold = Lens.field @"threshold"
{-# DEPRECATED uasThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

instance Core.FromJSON UpdateAnomalySubscription where
  toJSON UpdateAnomalySubscription {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SubscriptionArn" Core..= subscriptionArn),
            ("Frequency" Core..=) Core.<$> frequency,
            ("MonitorArnList" Core..=) Core.<$> monitorArnList,
            ("Subscribers" Core..=) Core.<$> subscribers,
            ("SubscriptionName" Core..=) Core.<$> subscriptionName,
            ("Threshold" Core..=) Core.<$> threshold
          ]
      )

instance Core.AWSRequest UpdateAnomalySubscription where
  type
    Rs UpdateAnomalySubscription =
      UpdateAnomalySubscriptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSInsightsIndexService.UpdateAnomalySubscription"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAnomalySubscriptionResponse'
            Core.<$> (x Core..: "SubscriptionArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateAnomalySubscriptionResponse' smart constructor.
data UpdateAnomalySubscriptionResponse = UpdateAnomalySubscriptionResponse'
  { -- | A cost anomaly subscription ARN.
    subscriptionArn :: Types.GenericString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAnomalySubscriptionResponse' value with any optional fields omitted.
mkUpdateAnomalySubscriptionResponse ::
  -- | 'subscriptionArn'
  Types.GenericString ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateAnomalySubscriptionResponse
mkUpdateAnomalySubscriptionResponse subscriptionArn responseStatus =
  UpdateAnomalySubscriptionResponse'
    { subscriptionArn,
      responseStatus
    }

-- | A cost anomaly subscription ARN.
--
-- /Note:/ Consider using 'subscriptionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrrsSubscriptionArn :: Lens.Lens' UpdateAnomalySubscriptionResponse Types.GenericString
uasrrsSubscriptionArn = Lens.field @"subscriptionArn"
{-# DEPRECATED uasrrsSubscriptionArn "Use generic-lens or generic-optics with 'subscriptionArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrrsResponseStatus :: Lens.Lens' UpdateAnomalySubscriptionResponse Core.Int
uasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
