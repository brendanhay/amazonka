{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeStaleSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Describes the stale security group rules for security groups in a specified VPC. Rules are stale when they reference a deleted security group in a peer VPC, or a security group in a peer VPC for which the VPC peering connection has been deleted.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeStaleSecurityGroups
  ( -- * Creating a request
    DescribeStaleSecurityGroups (..),
    mkDescribeStaleSecurityGroups,

    -- ** Request lenses
    dssgVpcId,
    dssgDryRun,
    dssgMaxResults,
    dssgNextToken,

    -- * Destructuring the response
    DescribeStaleSecurityGroupsResponse (..),
    mkDescribeStaleSecurityGroupsResponse,

    -- ** Response lenses
    dssgrrsNextToken,
    dssgrrsStaleSecurityGroupSet,
    dssgrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStaleSecurityGroups' smart constructor.
data DescribeStaleSecurityGroups = DescribeStaleSecurityGroups'
  { -- | The ID of the VPC.
    vpcId :: Types.VpcId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a prior call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStaleSecurityGroups' value with any optional fields omitted.
mkDescribeStaleSecurityGroups ::
  -- | 'vpcId'
  Types.VpcId ->
  DescribeStaleSecurityGroups
mkDescribeStaleSecurityGroups vpcId =
  DescribeStaleSecurityGroups'
    { vpcId,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssgVpcId :: Lens.Lens' DescribeStaleSecurityGroups Types.VpcId
dssgVpcId = Lens.field @"vpcId"
{-# DEPRECATED dssgVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssgDryRun :: Lens.Lens' DescribeStaleSecurityGroups (Core.Maybe Core.Bool)
dssgDryRun = Lens.field @"dryRun"
{-# DEPRECATED dssgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssgMaxResults :: Lens.Lens' DescribeStaleSecurityGroups (Core.Maybe Core.Natural)
dssgMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dssgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a prior call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssgNextToken :: Lens.Lens' DescribeStaleSecurityGroups (Core.Maybe Types.NextToken)
dssgNextToken = Lens.field @"nextToken"
{-# DEPRECATED dssgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeStaleSecurityGroups where
  type
    Rs DescribeStaleSecurityGroups =
      DescribeStaleSecurityGroupsResponse
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
            ( Core.pure ("Action", "DescribeStaleSecurityGroups")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "VpcId" vpcId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeStaleSecurityGroupsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "staleSecurityGroupSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeStaleSecurityGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"staleSecurityGroupSet" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeStaleSecurityGroupsResponse' smart constructor.
data DescribeStaleSecurityGroupsResponse = DescribeStaleSecurityGroupsResponse'
  { -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.String,
    -- | Information about the stale security groups.
    staleSecurityGroupSet :: Core.Maybe [Types.StaleSecurityGroup],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStaleSecurityGroupsResponse' value with any optional fields omitted.
mkDescribeStaleSecurityGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeStaleSecurityGroupsResponse
mkDescribeStaleSecurityGroupsResponse responseStatus =
  DescribeStaleSecurityGroupsResponse'
    { nextToken = Core.Nothing,
      staleSecurityGroupSet = Core.Nothing,
      responseStatus
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssgrrsNextToken :: Lens.Lens' DescribeStaleSecurityGroupsResponse (Core.Maybe Types.String)
dssgrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dssgrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the stale security groups.
--
-- /Note:/ Consider using 'staleSecurityGroupSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssgrrsStaleSecurityGroupSet :: Lens.Lens' DescribeStaleSecurityGroupsResponse (Core.Maybe [Types.StaleSecurityGroup])
dssgrrsStaleSecurityGroupSet = Lens.field @"staleSecurityGroupSet"
{-# DEPRECATED dssgrrsStaleSecurityGroupSet "Use generic-lens or generic-optics with 'staleSecurityGroupSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssgrrsResponseStatus :: Lens.Lens' DescribeStaleSecurityGroupsResponse Core.Int
dssgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dssgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
