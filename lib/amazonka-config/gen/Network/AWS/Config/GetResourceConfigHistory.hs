{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetResourceConfigHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of configuration items for the specified resource. The list contains details about each state of the resource during the specified time interval. If you specified a retention period to retain your @ConfigurationItems@ between a minimum of 30 days and a maximum of 7 years (2557 days), AWS Config returns the @ConfigurationItems@ for the specified retention period.
--
-- The response is paginated. By default, AWS Config returns a limit of 10 configuration items per page. You can customize this number with the @limit@ parameter. The response includes a @nextToken@ string. To get the next page of results, run the request again and specify the string for the @nextToken@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.Config.GetResourceConfigHistory
  ( -- * Creating a request
    GetResourceConfigHistory (..),
    mkGetResourceConfigHistory,

    -- ** Request lenses
    grchResourceType,
    grchResourceId,
    grchChronologicalOrder,
    grchEarlierTime,
    grchLaterTime,
    grchLimit,
    grchNextToken,

    -- * Destructuring the response
    GetResourceConfigHistoryResponse (..),
    mkGetResourceConfigHistoryResponse,

    -- ** Response lenses
    grchrrsConfigurationItems,
    grchrrsNextToken,
    grchrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'GetResourceConfigHistory' action.
--
-- /See:/ 'mkGetResourceConfigHistory' smart constructor.
data GetResourceConfigHistory = GetResourceConfigHistory'
  { -- | The resource type.
    resourceType :: Types.ResourceType,
    -- | The ID of the resource (for example., @sg-xxxxxx@ ).
    resourceId :: Types.ResourceId,
    -- | The chronological order for configuration items listed. By default, the results are listed in reverse chronological order.
    chronologicalOrder :: Core.Maybe Types.ChronologicalOrder,
    -- | The time stamp that indicates an earlier time. If not specified, the action returns paginated results that contain configuration items that start when the first configuration item was recorded.
    earlierTime :: Core.Maybe Core.NominalDiffTime,
    -- | The time stamp that indicates a later time. If not specified, current time is taken.
    laterTime :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of configuration items returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetResourceConfigHistory' value with any optional fields omitted.
mkGetResourceConfigHistory ::
  -- | 'resourceType'
  Types.ResourceType ->
  -- | 'resourceId'
  Types.ResourceId ->
  GetResourceConfigHistory
mkGetResourceConfigHistory resourceType resourceId =
  GetResourceConfigHistory'
    { resourceType,
      resourceId,
      chronologicalOrder = Core.Nothing,
      earlierTime = Core.Nothing,
      laterTime = Core.Nothing,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchResourceType :: Lens.Lens' GetResourceConfigHistory Types.ResourceType
grchResourceType = Lens.field @"resourceType"
{-# DEPRECATED grchResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the resource (for example., @sg-xxxxxx@ ).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchResourceId :: Lens.Lens' GetResourceConfigHistory Types.ResourceId
grchResourceId = Lens.field @"resourceId"
{-# DEPRECATED grchResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The chronological order for configuration items listed. By default, the results are listed in reverse chronological order.
--
-- /Note:/ Consider using 'chronologicalOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchChronologicalOrder :: Lens.Lens' GetResourceConfigHistory (Core.Maybe Types.ChronologicalOrder)
grchChronologicalOrder = Lens.field @"chronologicalOrder"
{-# DEPRECATED grchChronologicalOrder "Use generic-lens or generic-optics with 'chronologicalOrder' instead." #-}

-- | The time stamp that indicates an earlier time. If not specified, the action returns paginated results that contain configuration items that start when the first configuration item was recorded.
--
-- /Note:/ Consider using 'earlierTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchEarlierTime :: Lens.Lens' GetResourceConfigHistory (Core.Maybe Core.NominalDiffTime)
grchEarlierTime = Lens.field @"earlierTime"
{-# DEPRECATED grchEarlierTime "Use generic-lens or generic-optics with 'earlierTime' instead." #-}

-- | The time stamp that indicates a later time. If not specified, current time is taken.
--
-- /Note:/ Consider using 'laterTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchLaterTime :: Lens.Lens' GetResourceConfigHistory (Core.Maybe Core.NominalDiffTime)
grchLaterTime = Lens.field @"laterTime"
{-# DEPRECATED grchLaterTime "Use generic-lens or generic-optics with 'laterTime' instead." #-}

-- | The maximum number of configuration items returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchLimit :: Lens.Lens' GetResourceConfigHistory (Core.Maybe Core.Natural)
grchLimit = Lens.field @"limit"
{-# DEPRECATED grchLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchNextToken :: Lens.Lens' GetResourceConfigHistory (Core.Maybe Types.NextToken)
grchNextToken = Lens.field @"nextToken"
{-# DEPRECATED grchNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetResourceConfigHistory where
  toJSON GetResourceConfigHistory {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("resourceType" Core..= resourceType),
            Core.Just ("resourceId" Core..= resourceId),
            ("chronologicalOrder" Core..=) Core.<$> chronologicalOrder,
            ("earlierTime" Core..=) Core.<$> earlierTime,
            ("laterTime" Core..=) Core.<$> laterTime,
            ("limit" Core..=) Core.<$> limit,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetResourceConfigHistory where
  type Rs GetResourceConfigHistory = GetResourceConfigHistoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StarlingDoveService.GetResourceConfigHistory")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceConfigHistoryResponse'
            Core.<$> (x Core..:? "configurationItems")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetResourceConfigHistory where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"configurationItems" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | The output for the 'GetResourceConfigHistory' action.
--
-- /See:/ 'mkGetResourceConfigHistoryResponse' smart constructor.
data GetResourceConfigHistoryResponse = GetResourceConfigHistoryResponse'
  { -- | A list that contains the configuration history of one or more resources.
    configurationItems :: Core.Maybe [Types.ConfigurationItem],
    -- | The string that you use in a subsequent request to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetResourceConfigHistoryResponse' value with any optional fields omitted.
mkGetResourceConfigHistoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetResourceConfigHistoryResponse
mkGetResourceConfigHistoryResponse responseStatus =
  GetResourceConfigHistoryResponse'
    { configurationItems =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list that contains the configuration history of one or more resources.
--
-- /Note:/ Consider using 'configurationItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchrrsConfigurationItems :: Lens.Lens' GetResourceConfigHistoryResponse (Core.Maybe [Types.ConfigurationItem])
grchrrsConfigurationItems = Lens.field @"configurationItems"
{-# DEPRECATED grchrrsConfigurationItems "Use generic-lens or generic-optics with 'configurationItems' instead." #-}

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchrrsNextToken :: Lens.Lens' GetResourceConfigHistoryResponse (Core.Maybe Types.NextToken)
grchrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED grchrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grchrrsResponseStatus :: Lens.Lens' GetResourceConfigHistoryResponse Core.Int
grchrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grchrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
