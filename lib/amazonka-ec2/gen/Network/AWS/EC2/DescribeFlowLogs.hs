{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeFlowLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more flow logs. To view the information in your flow logs (the log streams for the network interfaces), you must use the CloudWatch Logs console or the CloudWatch Logs API.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeFlowLogs
    (
    -- * Creating a request
      DescribeFlowLogs (..)
    , mkDescribeFlowLogs
    -- ** Request lenses
    , dflsDryRun
    , dflsFilter
    , dflsFlowLogIds
    , dflsMaxResults
    , dflsNextToken

    -- * Destructuring the response
    , DescribeFlowLogsResponse (..)
    , mkDescribeFlowLogsResponse
    -- ** Response lenses
    , dflrfrsFlowLogs
    , dflrfrsNextToken
    , dflrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeFlowLogs' smart constructor.
data DescribeFlowLogs = DescribeFlowLogs'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filter :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @deliver-log-status@ - The status of the logs delivery (@SUCCESS@ | @FAILED@ ).
--
--
--     * @log-destination-type@ - The type of destination to which the flow log publishes data. Possible destination types include @cloud-watch-logs@ and @s3@ .
--
--
--     * @flow-log-id@ - The ID of the flow log.
--
--
--     * @log-group-name@ - The name of the log group.
--
--
--     * @resource-id@ - The ID of the VPC, subnet, or network interface.
--
--
--     * @traffic-type@ - The type of traffic (@ACCEPT@ | @REJECT@ | @ALL@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
  , flowLogIds :: Core.Maybe [Types.VpcFlowLogId]
    -- ^ One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeFlowLogs' value with any optional fields omitted.
mkDescribeFlowLogs
    :: DescribeFlowLogs
mkDescribeFlowLogs
  = DescribeFlowLogs'{dryRun = Core.Nothing, filter = Core.Nothing,
                      flowLogIds = Core.Nothing, maxResults = Core.Nothing,
                      nextToken = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflsDryRun :: Lens.Lens' DescribeFlowLogs (Core.Maybe Core.Bool)
dflsDryRun = Lens.field @"dryRun"
{-# INLINEABLE dflsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @deliver-log-status@ - The status of the logs delivery (@SUCCESS@ | @FAILED@ ).
--
--
--     * @log-destination-type@ - The type of destination to which the flow log publishes data. Possible destination types include @cloud-watch-logs@ and @s3@ .
--
--
--     * @flow-log-id@ - The ID of the flow log.
--
--
--     * @log-group-name@ - The name of the log group.
--
--
--     * @resource-id@ - The ID of the VPC, subnet, or network interface.
--
--
--     * @traffic-type@ - The type of traffic (@ACCEPT@ | @REJECT@ | @ALL@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflsFilter :: Lens.Lens' DescribeFlowLogs (Core.Maybe [Types.Filter])
dflsFilter = Lens.field @"filter"
{-# INLINEABLE dflsFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
--
-- /Note:/ Consider using 'flowLogIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflsFlowLogIds :: Lens.Lens' DescribeFlowLogs (Core.Maybe [Types.VpcFlowLogId])
dflsFlowLogIds = Lens.field @"flowLogIds"
{-# INLINEABLE dflsFlowLogIds #-}
{-# DEPRECATED flowLogIds "Use generic-lens or generic-optics with 'flowLogIds' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflsMaxResults :: Lens.Lens' DescribeFlowLogs (Core.Maybe Core.Int)
dflsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dflsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflsNextToken :: Lens.Lens' DescribeFlowLogs (Core.Maybe Core.Text)
dflsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dflsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeFlowLogs where
        toQuery DescribeFlowLogs{..}
          = Core.toQueryPair "Action" ("DescribeFlowLogs" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filter
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "FlowLogId") flowLogIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeFlowLogs where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeFlowLogs where
        type Rs DescribeFlowLogs = DescribeFlowLogsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DescribeFlowLogsResponse' Core.<$>
                   (x Core..@? "flowLogSet" Core..<@> Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeFlowLogs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"flowLogs" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeFlowLogsResponse' smart constructor.
data DescribeFlowLogsResponse = DescribeFlowLogsResponse'
  { flowLogs :: Core.Maybe [Types.FlowLog]
    -- ^ Information about the flow logs.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeFlowLogsResponse' value with any optional fields omitted.
mkDescribeFlowLogsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeFlowLogsResponse
mkDescribeFlowLogsResponse responseStatus
  = DescribeFlowLogsResponse'{flowLogs = Core.Nothing,
                              nextToken = Core.Nothing, responseStatus}

-- | Information about the flow logs.
--
-- /Note:/ Consider using 'flowLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflrfrsFlowLogs :: Lens.Lens' DescribeFlowLogsResponse (Core.Maybe [Types.FlowLog])
dflrfrsFlowLogs = Lens.field @"flowLogs"
{-# INLINEABLE dflrfrsFlowLogs #-}
{-# DEPRECATED flowLogs "Use generic-lens or generic-optics with 'flowLogs' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflrfrsNextToken :: Lens.Lens' DescribeFlowLogsResponse (Core.Maybe Core.Text)
dflrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dflrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflrfrsResponseStatus :: Lens.Lens' DescribeFlowLogsResponse Core.Int
dflrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dflrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
