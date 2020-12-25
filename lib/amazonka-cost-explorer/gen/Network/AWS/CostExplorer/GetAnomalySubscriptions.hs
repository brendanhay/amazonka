{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetAnomalySubscriptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the cost anomaly subscription objects for your account. You can filter using a list of cost anomaly monitor Amazon Resource Names (ARNs).
module Network.AWS.CostExplorer.GetAnomalySubscriptions
  ( -- * Creating a request
    GetAnomalySubscriptions (..),
    mkGetAnomalySubscriptions,

    -- ** Request lenses
    gasMaxResults,
    gasMonitorArn,
    gasNextPageToken,
    gasSubscriptionArnList,

    -- * Destructuring the response
    GetAnomalySubscriptionsResponse (..),
    mkGetAnomalySubscriptionsResponse,

    -- ** Response lenses
    gasrrsAnomalySubscriptions,
    gasrrsNextPageToken,
    gasrrsResponseStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAnomalySubscriptions' smart constructor.
data GetAnomalySubscriptions = GetAnomalySubscriptions'
  { -- | The number of entries a paginated response contains.
    maxResults :: Core.Maybe Core.Int,
    -- | Cost anomaly monitor ARNs.
    monitorArn :: Core.Maybe Types.MonitorArn,
    -- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | A list of cost anomaly subscription ARNs.
    subscriptionArnList :: Core.Maybe [Types.Value]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAnomalySubscriptions' value with any optional fields omitted.
mkGetAnomalySubscriptions ::
  GetAnomalySubscriptions
mkGetAnomalySubscriptions =
  GetAnomalySubscriptions'
    { maxResults = Core.Nothing,
      monitorArn = Core.Nothing,
      nextPageToken = Core.Nothing,
      subscriptionArnList = Core.Nothing
    }

-- | The number of entries a paginated response contains.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasMaxResults :: Lens.Lens' GetAnomalySubscriptions (Core.Maybe Core.Int)
gasMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gasMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Cost anomaly monitor ARNs.
--
-- /Note:/ Consider using 'monitorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasMonitorArn :: Lens.Lens' GetAnomalySubscriptions (Core.Maybe Types.MonitorArn)
gasMonitorArn = Lens.field @"monitorArn"
{-# DEPRECATED gasMonitorArn "Use generic-lens or generic-optics with 'monitorArn' instead." #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasNextPageToken :: Lens.Lens' GetAnomalySubscriptions (Core.Maybe Types.NextPageToken)
gasNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED gasNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | A list of cost anomaly subscription ARNs.
--
-- /Note:/ Consider using 'subscriptionArnList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasSubscriptionArnList :: Lens.Lens' GetAnomalySubscriptions (Core.Maybe [Types.Value])
gasSubscriptionArnList = Lens.field @"subscriptionArnList"
{-# DEPRECATED gasSubscriptionArnList "Use generic-lens or generic-optics with 'subscriptionArnList' instead." #-}

instance Core.FromJSON GetAnomalySubscriptions where
  toJSON GetAnomalySubscriptions {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("MonitorArn" Core..=) Core.<$> monitorArn,
            ("NextPageToken" Core..=) Core.<$> nextPageToken,
            ("SubscriptionArnList" Core..=) Core.<$> subscriptionArnList
          ]
      )

instance Core.AWSRequest GetAnomalySubscriptions where
  type Rs GetAnomalySubscriptions = GetAnomalySubscriptionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSInsightsIndexService.GetAnomalySubscriptions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAnomalySubscriptionsResponse'
            Core.<$> (x Core..:? "AnomalySubscriptions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAnomalySubscriptionsResponse' smart constructor.
data GetAnomalySubscriptionsResponse = GetAnomalySubscriptionsResponse'
  { -- | A list of cost anomaly subscriptions that includes the detailed metadata for each one.
    anomalySubscriptions :: [Types.AnomalySubscription],
    -- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAnomalySubscriptionsResponse' value with any optional fields omitted.
mkGetAnomalySubscriptionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAnomalySubscriptionsResponse
mkGetAnomalySubscriptionsResponse responseStatus =
  GetAnomalySubscriptionsResponse'
    { anomalySubscriptions =
        Core.mempty,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | A list of cost anomaly subscriptions that includes the detailed metadata for each one.
--
-- /Note:/ Consider using 'anomalySubscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsAnomalySubscriptions :: Lens.Lens' GetAnomalySubscriptionsResponse [Types.AnomalySubscription]
gasrrsAnomalySubscriptions = Lens.field @"anomalySubscriptions"
{-# DEPRECATED gasrrsAnomalySubscriptions "Use generic-lens or generic-optics with 'anomalySubscriptions' instead." #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsNextPageToken :: Lens.Lens' GetAnomalySubscriptionsResponse (Core.Maybe Types.NextPageToken)
gasrrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED gasrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrrsResponseStatus :: Lens.Lens' GetAnomalySubscriptionsResponse Core.Int
gasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
