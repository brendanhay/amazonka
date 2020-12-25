{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ListBuilds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves build resources for all builds associated with the AWS account in use. You can limit results to builds that are in a specific status by using the @Status@ parameter. Use the pagination parameters to retrieve results in a set of sequential pages.
--
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html Upload a Custom Server Build>
-- __Related operations__
--
--     * 'CreateBuild'
--
--
--     * 'ListBuilds'
--
--
--     * 'DescribeBuild'
--
--
--     * 'UpdateBuild'
--
--
--     * 'DeleteBuild'
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListBuilds
  ( -- * Creating a request
    ListBuilds (..),
    mkListBuilds,

    -- ** Request lenses
    lbLimit,
    lbNextToken,
    lbStatus,

    -- * Destructuring the response
    ListBuildsResponse (..),
    mkListBuildsResponse,

    -- ** Response lenses
    lbrrsBuilds,
    lbrrsNextToken,
    lbrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkListBuilds' smart constructor.
data ListBuilds = ListBuilds'
  { -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
    limit :: Core.Maybe Core.Natural,
    -- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Build status to filter results by. To retrieve all builds, leave this parameter empty.
    --
    -- Possible build statuses include the following:
    --
    --     * __INITIALIZED__ -- A new build has been defined, but no files have been uploaded. You cannot create fleets for builds that are in this status. When a build is successfully created, the build status is set to this value.
    --
    --
    --     * __READY__ -- The game build has been successfully uploaded. You can now create new fleets for this build.
    --
    --
    --     * __FAILED__ -- The game build upload failed. You cannot create new fleets for this build.
    status :: Core.Maybe Types.BuildStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBuilds' value with any optional fields omitted.
mkListBuilds ::
  ListBuilds
mkListBuilds =
  ListBuilds'
    { limit = Core.Nothing,
      nextToken = Core.Nothing,
      status = Core.Nothing
    }

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbLimit :: Lens.Lens' ListBuilds (Core.Maybe Core.Natural)
lbLimit = Lens.field @"limit"
{-# DEPRECATED lbLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbNextToken :: Lens.Lens' ListBuilds (Core.Maybe Types.NextToken)
lbNextToken = Lens.field @"nextToken"
{-# DEPRECATED lbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Build status to filter results by. To retrieve all builds, leave this parameter empty.
--
-- Possible build statuses include the following:
--
--     * __INITIALIZED__ -- A new build has been defined, but no files have been uploaded. You cannot create fleets for builds that are in this status. When a build is successfully created, the build status is set to this value.
--
--
--     * __READY__ -- The game build has been successfully uploaded. You can now create new fleets for this build.
--
--
--     * __FAILED__ -- The game build upload failed. You cannot create new fleets for this build.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbStatus :: Lens.Lens' ListBuilds (Core.Maybe Types.BuildStatus)
lbStatus = Lens.field @"status"
{-# DEPRECATED lbStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON ListBuilds where
  toJSON ListBuilds {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Status" Core..=) Core.<$> status
          ]
      )

instance Core.AWSRequest ListBuilds where
  type Rs ListBuilds = ListBuildsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.ListBuilds")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBuildsResponse'
            Core.<$> (x Core..:? "Builds")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListBuilds where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"builds" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkListBuildsResponse' smart constructor.
data ListBuildsResponse = ListBuildsResponse'
  { -- | A collection of build resources that match the request.
    builds :: Core.Maybe [Types.Build],
    -- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Core.Maybe Types.NonEmptyString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListBuildsResponse' value with any optional fields omitted.
mkListBuildsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListBuildsResponse
mkListBuildsResponse responseStatus =
  ListBuildsResponse'
    { builds = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A collection of build resources that match the request.
--
-- /Note:/ Consider using 'builds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrrsBuilds :: Lens.Lens' ListBuildsResponse (Core.Maybe [Types.Build])
lbrrsBuilds = Lens.field @"builds"
{-# DEPRECATED lbrrsBuilds "Use generic-lens or generic-optics with 'builds' instead." #-}

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrrsNextToken :: Lens.Lens' ListBuildsResponse (Core.Maybe Types.NonEmptyString)
lbrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lbrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrrsResponseStatus :: Lens.Lens' ListBuildsResponse Core.Int
lbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
