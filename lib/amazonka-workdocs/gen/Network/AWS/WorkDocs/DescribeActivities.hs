{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeActivities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the user activities in a specified time period.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeActivities
  ( -- * Creating a request
    DescribeActivities (..),
    mkDescribeActivities,

    -- ** Request lenses
    daActivityTypes,
    daAuthenticationToken,
    daEndTime,
    daIncludeIndirectActivities,
    daLimit,
    daMarker,
    daOrganizationId,
    daResourceId,
    daStartTime,
    daUserId,

    -- * Destructuring the response
    DescribeActivitiesResponse (..),
    mkDescribeActivitiesResponse,

    -- ** Response lenses
    darrsMarker,
    darrsUserActivities,
    darrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDescribeActivities' smart constructor.
data DescribeActivities = DescribeActivities'
  { -- | Specifies which activity types to include in the response. If this field is left empty, all activity types are returned.
    activityTypes :: Core.Maybe Types.ActivityTypes,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | The timestamp that determines the end time of the activities. The response includes the activities performed before the specified timestamp.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | Includes indirect activities. An indirect activity results from a direct activity performed on a parent resource. For example, sharing a parent folder (the direct activity) shares all of the subfolders and documents within the parent folder (the indirect activity).
    includeIndirectActivities :: Core.Maybe Core.Bool,
    -- | The maximum number of items to return.
    limit :: Core.Maybe Core.Natural,
    -- | The marker for the next set of results.
    marker :: Core.Maybe Types.Marker,
    -- | The ID of the organization. This is a mandatory parameter when using administrative API (SigV4) requests.
    organizationId :: Core.Maybe Types.OrganizationId,
    -- | The document or folder ID for which to describe activity types.
    resourceId :: Core.Maybe Types.ResourceId,
    -- | The timestamp that determines the starting time of the activities. The response includes the activities performed after the specified timestamp.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the user who performed the action. The response includes activities pertaining to this user. This is an optional parameter and is only applicable for administrative API (SigV4) requests.
    userId :: Core.Maybe Types.UserId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeActivities' value with any optional fields omitted.
mkDescribeActivities ::
  DescribeActivities
mkDescribeActivities =
  DescribeActivities'
    { activityTypes = Core.Nothing,
      authenticationToken = Core.Nothing,
      endTime = Core.Nothing,
      includeIndirectActivities = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing,
      organizationId = Core.Nothing,
      resourceId = Core.Nothing,
      startTime = Core.Nothing,
      userId = Core.Nothing
    }

-- | Specifies which activity types to include in the response. If this field is left empty, all activity types are returned.
--
-- /Note:/ Consider using 'activityTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daActivityTypes :: Lens.Lens' DescribeActivities (Core.Maybe Types.ActivityTypes)
daActivityTypes = Lens.field @"activityTypes"
{-# DEPRECATED daActivityTypes "Use generic-lens or generic-optics with 'activityTypes' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAuthenticationToken :: Lens.Lens' DescribeActivities (Core.Maybe Types.AuthenticationHeaderType)
daAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED daAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The timestamp that determines the end time of the activities. The response includes the activities performed before the specified timestamp.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daEndTime :: Lens.Lens' DescribeActivities (Core.Maybe Core.NominalDiffTime)
daEndTime = Lens.field @"endTime"
{-# DEPRECATED daEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Includes indirect activities. An indirect activity results from a direct activity performed on a parent resource. For example, sharing a parent folder (the direct activity) shares all of the subfolders and documents within the parent folder (the indirect activity).
--
-- /Note:/ Consider using 'includeIndirectActivities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daIncludeIndirectActivities :: Lens.Lens' DescribeActivities (Core.Maybe Core.Bool)
daIncludeIndirectActivities = Lens.field @"includeIndirectActivities"
{-# DEPRECATED daIncludeIndirectActivities "Use generic-lens or generic-optics with 'includeIndirectActivities' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daLimit :: Lens.Lens' DescribeActivities (Core.Maybe Core.Natural)
daLimit = Lens.field @"limit"
{-# DEPRECATED daLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daMarker :: Lens.Lens' DescribeActivities (Core.Maybe Types.Marker)
daMarker = Lens.field @"marker"
{-# DEPRECATED daMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The ID of the organization. This is a mandatory parameter when using administrative API (SigV4) requests.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daOrganizationId :: Lens.Lens' DescribeActivities (Core.Maybe Types.OrganizationId)
daOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED daOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The document or folder ID for which to describe activity types.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daResourceId :: Lens.Lens' DescribeActivities (Core.Maybe Types.ResourceId)
daResourceId = Lens.field @"resourceId"
{-# DEPRECATED daResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The timestamp that determines the starting time of the activities. The response includes the activities performed after the specified timestamp.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daStartTime :: Lens.Lens' DescribeActivities (Core.Maybe Core.NominalDiffTime)
daStartTime = Lens.field @"startTime"
{-# DEPRECATED daStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The ID of the user who performed the action. The response includes activities pertaining to this user. This is an optional parameter and is only applicable for administrative API (SigV4) requests.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daUserId :: Lens.Lens' DescribeActivities (Core.Maybe Types.UserId)
daUserId = Lens.field @"userId"
{-# DEPRECATED daUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Core.AWSRequest DescribeActivities where
  type Rs DescribeActivities = DescribeActivitiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/api/v1/activities",
        Core._rqQuery =
          Core.toQueryValue "activityTypes" Core.<$> activityTypes
            Core.<> (Core.toQueryValue "endTime" Core.<$> endTime)
            Core.<> ( Core.toQueryValue "includeIndirectActivities"
                        Core.<$> includeIndirectActivities
                    )
            Core.<> (Core.toQueryValue "limit" Core.<$> limit)
            Core.<> (Core.toQueryValue "marker" Core.<$> marker)
            Core.<> (Core.toQueryValue "organizationId" Core.<$> organizationId)
            Core.<> (Core.toQueryValue "resourceId" Core.<$> resourceId)
            Core.<> (Core.toQueryValue "startTime" Core.<$> startTime)
            Core.<> (Core.toQueryValue "userId" Core.<$> userId),
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeActivitiesResponse'
            Core.<$> (x Core..:? "Marker")
            Core.<*> (x Core..:? "UserActivities")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeActivities where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"userActivities" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeActivitiesResponse' smart constructor.
data DescribeActivitiesResponse = DescribeActivitiesResponse'
  { -- | The marker for the next set of results.
    marker :: Core.Maybe Types.MarkerType,
    -- | The list of activities for the specified user and time period.
    userActivities :: Core.Maybe [Types.Activity],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeActivitiesResponse' value with any optional fields omitted.
mkDescribeActivitiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeActivitiesResponse
mkDescribeActivitiesResponse responseStatus =
  DescribeActivitiesResponse'
    { marker = Core.Nothing,
      userActivities = Core.Nothing,
      responseStatus
    }

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsMarker :: Lens.Lens' DescribeActivitiesResponse (Core.Maybe Types.MarkerType)
darrsMarker = Lens.field @"marker"
{-# DEPRECATED darrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The list of activities for the specified user and time period.
--
-- /Note:/ Consider using 'userActivities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsUserActivities :: Lens.Lens' DescribeActivitiesResponse (Core.Maybe [Types.Activity])
darrsUserActivities = Lens.field @"userActivities"
{-# DEPRECATED darrsUserActivities "Use generic-lens or generic-optics with 'userActivities' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeActivitiesResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
