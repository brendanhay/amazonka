{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeFlowLogs (..),
    mkDescribeFlowLogs,

    -- ** Request lenses
    dflsNextToken,
    dflsFlowLogIds,
    dflsFilter,
    dflsDryRun,
    dflsMaxResults,

    -- * Destructuring the response
    DescribeFlowLogsResponse (..),
    mkDescribeFlowLogsResponse,

    -- ** Response lenses
    dflsrsNextToken,
    dflsrsFlowLogs,
    dflsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeFlowLogs' smart constructor.
data DescribeFlowLogs = DescribeFlowLogs'
  { nextToken ::
      Lude.Maybe Lude.Text,
    flowLogIds :: Lude.Maybe [Lude.Text],
    filter :: Lude.Maybe [Filter],
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFlowLogs' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filter' - One or more filters.
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
-- * 'flowLogIds' - One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
mkDescribeFlowLogs ::
  DescribeFlowLogs
mkDescribeFlowLogs =
  DescribeFlowLogs'
    { nextToken = Lude.Nothing,
      flowLogIds = Lude.Nothing,
      filter = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflsNextToken :: Lens.Lens' DescribeFlowLogs (Lude.Maybe Lude.Text)
dflsNextToken = Lens.lens (nextToken :: DescribeFlowLogs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFlowLogs)
{-# DEPRECATED dflsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
--
-- /Note:/ Consider using 'flowLogIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflsFlowLogIds :: Lens.Lens' DescribeFlowLogs (Lude.Maybe [Lude.Text])
dflsFlowLogIds = Lens.lens (flowLogIds :: DescribeFlowLogs -> Lude.Maybe [Lude.Text]) (\s a -> s {flowLogIds = a} :: DescribeFlowLogs)
{-# DEPRECATED dflsFlowLogIds "Use generic-lens or generic-optics with 'flowLogIds' instead." #-}

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
dflsFilter :: Lens.Lens' DescribeFlowLogs (Lude.Maybe [Filter])
dflsFilter = Lens.lens (filter :: DescribeFlowLogs -> Lude.Maybe [Filter]) (\s a -> s {filter = a} :: DescribeFlowLogs)
{-# DEPRECATED dflsFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflsDryRun :: Lens.Lens' DescribeFlowLogs (Lude.Maybe Lude.Bool)
dflsDryRun = Lens.lens (dryRun :: DescribeFlowLogs -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeFlowLogs)
{-# DEPRECATED dflsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflsMaxResults :: Lens.Lens' DescribeFlowLogs (Lude.Maybe Lude.Int)
dflsMaxResults = Lens.lens (maxResults :: DescribeFlowLogs -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeFlowLogs)
{-# DEPRECATED dflsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeFlowLogs where
  page rq rs
    | Page.stop (rs Lens.^. dflsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dflsrsFlowLogs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dflsNextToken Lens..~ rs Lens.^. dflsrsNextToken

instance Lude.AWSRequest DescribeFlowLogs where
  type Rs DescribeFlowLogs = DescribeFlowLogsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeFlowLogsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "flowLogSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFlowLogs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeFlowLogs where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeFlowLogs where
  toQuery DescribeFlowLogs' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeFlowLogs" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery (Lude.toQueryList "FlowLogId" Lude.<$> flowLogIds),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filter),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeFlowLogsResponse' smart constructor.
data DescribeFlowLogsResponse = DescribeFlowLogsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    flowLogs :: Lude.Maybe [FlowLog],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFlowLogsResponse' with the minimum fields required to make a request.
--
-- * 'flowLogs' - Information about the flow logs.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeFlowLogsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFlowLogsResponse
mkDescribeFlowLogsResponse pResponseStatus_ =
  DescribeFlowLogsResponse'
    { nextToken = Lude.Nothing,
      flowLogs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflsrsNextToken :: Lens.Lens' DescribeFlowLogsResponse (Lude.Maybe Lude.Text)
dflsrsNextToken = Lens.lens (nextToken :: DescribeFlowLogsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFlowLogsResponse)
{-# DEPRECATED dflsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the flow logs.
--
-- /Note:/ Consider using 'flowLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflsrsFlowLogs :: Lens.Lens' DescribeFlowLogsResponse (Lude.Maybe [FlowLog])
dflsrsFlowLogs = Lens.lens (flowLogs :: DescribeFlowLogsResponse -> Lude.Maybe [FlowLog]) (\s a -> s {flowLogs = a} :: DescribeFlowLogsResponse)
{-# DEPRECATED dflsrsFlowLogs "Use generic-lens or generic-optics with 'flowLogs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflsrsResponseStatus :: Lens.Lens' DescribeFlowLogsResponse Lude.Int
dflsrsResponseStatus = Lens.lens (responseStatus :: DescribeFlowLogsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFlowLogsResponse)
{-# DEPRECATED dflsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
