{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.ListJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the jobs associated with the requester. AWS Import/Export lists the jobs in reverse chronological order based on the date of creation. For example if Job Test1 was created 2009Dec30 and Test2 was created 2010Feb05, the ListJobs operation would return Test2 followed by Test1.
--
-- This operation returns paginated results.
module Network.AWS.ImportExport.ListJobs
  ( -- * Creating a request
    ListJobs (..),
    mkListJobs,

    -- ** Request lenses
    ljAPIVersion,
    ljMarker,
    ljMaxJobs,

    -- * Destructuring the response
    ListJobsResponse (..),
    mkListJobsResponse,

    -- ** Response lenses
    ljrrsIsTruncated,
    ljrrsJobs,
    ljrrsResponseStatus,
  )
where

import qualified Network.AWS.ImportExport.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input structure for the ListJobs operation.
--
-- /See:/ 'mkListJobs' smart constructor.
data ListJobs = ListJobs'
  { aPIVersion :: Core.Maybe Types.APIVersion,
    marker :: Core.Maybe Types.Marker,
    maxJobs :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListJobs' value with any optional fields omitted.
mkListJobs ::
  ListJobs
mkListJobs =
  ListJobs'
    { aPIVersion = Core.Nothing,
      marker = Core.Nothing,
      maxJobs = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPIVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljAPIVersion :: Lens.Lens' ListJobs (Core.Maybe Types.APIVersion)
ljAPIVersion = Lens.field @"aPIVersion"
{-# DEPRECATED ljAPIVersion "Use generic-lens or generic-optics with 'aPIVersion' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljMarker :: Lens.Lens' ListJobs (Core.Maybe Types.Marker)
ljMarker = Lens.field @"marker"
{-# DEPRECATED ljMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljMaxJobs :: Lens.Lens' ListJobs (Core.Maybe Core.Int)
ljMaxJobs = Lens.field @"maxJobs"
{-# DEPRECATED ljMaxJobs "Use generic-lens or generic-optics with 'maxJobs' instead." #-}

instance Core.AWSRequest ListJobs where
  type Rs ListJobs = ListJobsResponse
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
            ( Core.pure ("Operation=ListJobs", "")
                Core.<> (Core.pure ("Action", "ListJobs"))
                Core.<> (Core.pure ("Version", "2010-06-01"))
                Core.<> (Core.toQueryValue "APIVersion" Core.<$> aPIVersion)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxJobs" Core.<$> maxJobs)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListJobsResult"
      ( \s h x ->
          ListJobsResponse'
            Core.<$> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Jobs" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListJobs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? Lens.field @"jobs" Core.. Lens._last Core.. Lens.field @"jobId"
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker"
            Lens..~ rs
            Lens.^? Lens.field @"jobs" Core.. Lens._last Core.. Lens.field @"jobId"
        )

-- | Output structure for the ListJobs operation.
--
-- /See:/ 'mkListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { isTruncated :: Core.Maybe Core.Bool,
    jobs :: Core.Maybe [Types.Job],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListJobsResponse' value with any optional fields omitted.
mkListJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListJobsResponse
mkListJobsResponse responseStatus =
  ListJobsResponse'
    { isTruncated = Core.Nothing,
      jobs = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsIsTruncated :: Lens.Lens' ListJobsResponse (Core.Maybe Core.Bool)
ljrrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED ljrrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsJobs :: Lens.Lens' ListJobsResponse (Core.Maybe [Types.Job])
ljrrsJobs = Lens.field @"jobs"
{-# DEPRECATED ljrrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsResponseStatus :: Lens.Lens' ListJobsResponse Core.Int
ljrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ljrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
