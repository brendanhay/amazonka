{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeAccessPoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of a specific Amazon EFS access point if the @AccessPointId@ is provided. If you provide an EFS @FileSystemId@ , it returns descriptions of all access points for that file system. You can provide either an @AccessPointId@ or a @FileSystemId@ in the request, but not both.
--
-- This operation requires permissions for the @elasticfilesystem:DescribeAccessPoints@ action.
module Network.AWS.EFS.DescribeAccessPoints
  ( -- * Creating a request
    DescribeAccessPoints (..),
    mkDescribeAccessPoints,

    -- ** Request lenses
    dapAccessPointId,
    dapFileSystemId,
    dapMaxResults,
    dapNextToken,

    -- * Destructuring the response
    DescribeAccessPointsResponse (..),
    mkDescribeAccessPointsResponse,

    -- ** Response lenses
    daprrsAccessPoints,
    daprrsNextToken,
    daprrsResponseStatus,
  )
where

import qualified Network.AWS.EFS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAccessPoints' smart constructor.
data DescribeAccessPoints = DescribeAccessPoints'
  { -- | (Optional) Specifies an EFS access point to describe in the response; mutually exclusive with @FileSystemId@ .
    accessPointId :: Core.Maybe Types.AccessPointId,
    -- | (Optional) If you provide a @FileSystemId@ , EFS returns all access points for that file system; mutually exclusive with @AccessPointId@ .
    fileSystemId :: Core.Maybe Types.FileSystemId,
    -- | (Optional) When retrieving all access points for a file system, you can optionally specify the @MaxItems@ parameter to limit the number of objects returned in a response. The default value is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | @NextToken@ is present if the response is paginated. You can use @NextMarker@ in the subsequent request to fetch the next page of access point descriptions.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccessPoints' value with any optional fields omitted.
mkDescribeAccessPoints ::
  DescribeAccessPoints
mkDescribeAccessPoints =
  DescribeAccessPoints'
    { accessPointId = Core.Nothing,
      fileSystemId = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | (Optional) Specifies an EFS access point to describe in the response; mutually exclusive with @FileSystemId@ .
--
-- /Note:/ Consider using 'accessPointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapAccessPointId :: Lens.Lens' DescribeAccessPoints (Core.Maybe Types.AccessPointId)
dapAccessPointId = Lens.field @"accessPointId"
{-# DEPRECATED dapAccessPointId "Use generic-lens or generic-optics with 'accessPointId' instead." #-}

-- | (Optional) If you provide a @FileSystemId@ , EFS returns all access points for that file system; mutually exclusive with @AccessPointId@ .
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapFileSystemId :: Lens.Lens' DescribeAccessPoints (Core.Maybe Types.FileSystemId)
dapFileSystemId = Lens.field @"fileSystemId"
{-# DEPRECATED dapFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | (Optional) When retrieving all access points for a file system, you can optionally specify the @MaxItems@ parameter to limit the number of objects returned in a response. The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapMaxResults :: Lens.Lens' DescribeAccessPoints (Core.Maybe Core.Natural)
dapMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dapMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | @NextToken@ is present if the response is paginated. You can use @NextMarker@ in the subsequent request to fetch the next page of access point descriptions.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapNextToken :: Lens.Lens' DescribeAccessPoints (Core.Maybe Types.NextToken)
dapNextToken = Lens.field @"nextToken"
{-# DEPRECATED dapNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeAccessPoints where
  type Rs DescribeAccessPoints = DescribeAccessPointsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2015-02-01/access-points",
        Core._rqQuery =
          Core.toQueryValue "AccessPointId" Core.<$> accessPointId
            Core.<> (Core.toQueryValue "FileSystemId" Core.<$> fileSystemId)
            Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccessPointsResponse'
            Core.<$> (x Core..:? "AccessPoints")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAccessPointsResponse' smart constructor.
data DescribeAccessPointsResponse = DescribeAccessPointsResponse'
  { -- | An array of access point descriptions.
    accessPoints :: Core.Maybe [Types.AccessPointDescription],
    -- | Present if there are more access points than returned in the response. You can use the NextMarker in the subsequent request to fetch the additional descriptions.
    nextToken :: Core.Maybe Types.Token,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccessPointsResponse' value with any optional fields omitted.
mkDescribeAccessPointsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAccessPointsResponse
mkDescribeAccessPointsResponse responseStatus =
  DescribeAccessPointsResponse'
    { accessPoints = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of access point descriptions.
--
-- /Note:/ Consider using 'accessPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daprrsAccessPoints :: Lens.Lens' DescribeAccessPointsResponse (Core.Maybe [Types.AccessPointDescription])
daprrsAccessPoints = Lens.field @"accessPoints"
{-# DEPRECATED daprrsAccessPoints "Use generic-lens or generic-optics with 'accessPoints' instead." #-}

-- | Present if there are more access points than returned in the response. You can use the NextMarker in the subsequent request to fetch the additional descriptions.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daprrsNextToken :: Lens.Lens' DescribeAccessPointsResponse (Core.Maybe Types.Token)
daprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED daprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daprrsResponseStatus :: Lens.Lens' DescribeAccessPointsResponse Core.Int
daprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED daprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
