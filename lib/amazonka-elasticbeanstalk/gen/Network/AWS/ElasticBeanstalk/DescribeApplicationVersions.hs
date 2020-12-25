{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of application versions.
--
-- This operation returns paginated results.
module Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
  ( -- * Creating a request
    DescribeApplicationVersions (..),
    mkDescribeApplicationVersions,

    -- ** Request lenses
    dApplicationName,
    dMaxRecords,
    dNextToken,
    dVersionLabels,

    -- * Destructuring the response
    DescribeApplicationVersionsResponse (..),
    mkDescribeApplicationVersionsResponse,

    -- ** Response lenses
    davrrsApplicationVersions,
    davrrsNextToken,
    davrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe application versions.
--
-- /See:/ 'mkDescribeApplicationVersions' smart constructor.
data DescribeApplicationVersions = DescribeApplicationVersions'
  { -- | Specify an application name to show only application versions for that application.
    applicationName :: Core.Maybe Types.ApplicationName,
    -- | For a paginated request. Specify a maximum number of application versions to include in each response.
    --
    -- If no @MaxRecords@ is specified, all available application versions are retrieved in a single response.
    maxRecords :: Core.Maybe Core.Natural,
    -- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
    --
    -- If no @NextToken@ is specified, the first page is retrieved.
    nextToken :: Core.Maybe Types.Token,
    -- | Specify a version label to show a specific application version.
    versionLabels :: Core.Maybe [Types.VersionLabel]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeApplicationVersions' value with any optional fields omitted.
mkDescribeApplicationVersions ::
  DescribeApplicationVersions
mkDescribeApplicationVersions =
  DescribeApplicationVersions'
    { applicationName = Core.Nothing,
      maxRecords = Core.Nothing,
      nextToken = Core.Nothing,
      versionLabels = Core.Nothing
    }

-- | Specify an application name to show only application versions for that application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dApplicationName :: Lens.Lens' DescribeApplicationVersions (Core.Maybe Types.ApplicationName)
dApplicationName = Lens.field @"applicationName"
{-# DEPRECATED dApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | For a paginated request. Specify a maximum number of application versions to include in each response.
--
-- If no @MaxRecords@ is specified, all available application versions are retrieved in a single response.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxRecords :: Lens.Lens' DescribeApplicationVersions (Core.Maybe Core.Natural)
dMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeApplicationVersions (Core.Maybe Types.Token)
dNextToken = Lens.field @"nextToken"
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specify a version label to show a specific application version.
--
-- /Note:/ Consider using 'versionLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVersionLabels :: Lens.Lens' DescribeApplicationVersions (Core.Maybe [Types.VersionLabel])
dVersionLabels = Lens.field @"versionLabels"
{-# DEPRECATED dVersionLabels "Use generic-lens or generic-optics with 'versionLabels' instead." #-}

instance Core.AWSRequest DescribeApplicationVersions where
  type
    Rs DescribeApplicationVersions =
      DescribeApplicationVersionsResponse
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
            ( Core.pure ("Action", "DescribeApplicationVersions")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ApplicationName" Core.<$> applicationName)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> ( Core.toQueryValue
                            "VersionLabels"
                            (Core.toQueryList "member" Core.<$> versionLabels)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeApplicationVersionsResult"
      ( \s h x ->
          DescribeApplicationVersionsResponse'
            Core.<$> ( x Core..@? "ApplicationVersions"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeApplicationVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"applicationVersions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Result message wrapping a list of application version descriptions.
--
-- /See:/ 'mkDescribeApplicationVersionsResponse' smart constructor.
data DescribeApplicationVersionsResponse = DescribeApplicationVersionsResponse'
  { -- | List of @ApplicationVersionDescription@ objects sorted in order of creation.
    applicationVersions :: Core.Maybe [Types.ApplicationVersionDescription],
    -- | In a paginated request, the token that you can pass in a subsequent request to get the next response page.
    nextToken :: Core.Maybe Types.Token,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeApplicationVersionsResponse' value with any optional fields omitted.
mkDescribeApplicationVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeApplicationVersionsResponse
mkDescribeApplicationVersionsResponse responseStatus =
  DescribeApplicationVersionsResponse'
    { applicationVersions =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | List of @ApplicationVersionDescription@ objects sorted in order of creation.
--
-- /Note:/ Consider using 'applicationVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davrrsApplicationVersions :: Lens.Lens' DescribeApplicationVersionsResponse (Core.Maybe [Types.ApplicationVersionDescription])
davrrsApplicationVersions = Lens.field @"applicationVersions"
{-# DEPRECATED davrrsApplicationVersions "Use generic-lens or generic-optics with 'applicationVersions' instead." #-}

-- | In a paginated request, the token that you can pass in a subsequent request to get the next response page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davrrsNextToken :: Lens.Lens' DescribeApplicationVersionsResponse (Core.Maybe Types.Token)
davrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED davrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davrrsResponseStatus :: Lens.Lens' DescribeApplicationVersionsResponse Core.Int
davrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED davrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
