{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeInstanceCreditSpecifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the credit option for CPU usage of the specified burstable performance instances. The credit options are @standard@ and @unlimited@ .
--
-- If you do not specify an instance ID, Amazon EC2 returns burstable performance instances with the @unlimited@ credit option, as well as instances that were previously configured as T2, T3, and T3a with the @unlimited@ credit option. For example, if you resize a T2 instance, while it is configured as @unlimited@ , to an M4 instance, Amazon EC2 returns the M4 instance.
-- If you specify one or more instance IDs, Amazon EC2 returns the credit option (@standard@ or @unlimited@ ) of those instances. If you specify an instance ID that is not valid, such as an instance that is not a burstable performance instance, an error is returned.
-- Recently terminated instances might appear in the returned results. This interval is usually less than one hour.
-- If an Availability Zone is experiencing a service disruption and you specify instance IDs in the affected zone, or do not specify any instance IDs at all, the call fails. If you specify only instance IDs in an unaffected zone, the call works normally.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeInstanceCreditSpecifications
  ( -- * Creating a request
    DescribeInstanceCreditSpecifications (..),
    mkDescribeInstanceCreditSpecifications,

    -- ** Request lenses
    dicsFilters,
    dicsNextToken,
    dicsInstanceIds,
    dicsDryRun,
    dicsMaxResults,

    -- * Destructuring the response
    DescribeInstanceCreditSpecificationsResponse (..),
    mkDescribeInstanceCreditSpecificationsResponse,

    -- ** Response lenses
    dicsrsNextToken,
    dicsrsInstanceCreditSpecifications,
    dicsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeInstanceCreditSpecifications' smart constructor.
data DescribeInstanceCreditSpecifications = DescribeInstanceCreditSpecifications'
  { -- | The filters.
    --
    --
    --     * @instance-id@ - The ID of the instance.
    filters :: Lude.Maybe [Filter],
    -- | The token to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The instance IDs.
    --
    -- Default: Describes all your instances.
    -- Constraints: Maximum 1000 explicitly specified instance IDs.
    instanceIds :: Lude.Maybe [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000. You cannot specify this parameter and the instance IDs parameter in the same call.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceCreditSpecifications' with the minimum fields required to make a request.
--
-- * 'filters' - The filters.
--
--
--     * @instance-id@ - The ID of the instance.
--
--
-- * 'nextToken' - The token to retrieve the next page of results.
-- * 'instanceIds' - The instance IDs.
--
-- Default: Describes all your instances.
-- Constraints: Maximum 1000 explicitly specified instance IDs.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000. You cannot specify this parameter and the instance IDs parameter in the same call.
mkDescribeInstanceCreditSpecifications ::
  DescribeInstanceCreditSpecifications
mkDescribeInstanceCreditSpecifications =
  DescribeInstanceCreditSpecifications'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      instanceIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters.
--
--
--     * @instance-id@ - The ID of the instance.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dicsFilters :: Lens.Lens' DescribeInstanceCreditSpecifications (Lude.Maybe [Filter])
dicsFilters = Lens.lens (filters :: DescribeInstanceCreditSpecifications -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeInstanceCreditSpecifications)
{-# DEPRECATED dicsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dicsNextToken :: Lens.Lens' DescribeInstanceCreditSpecifications (Lude.Maybe Lude.Text)
dicsNextToken = Lens.lens (nextToken :: DescribeInstanceCreditSpecifications -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstanceCreditSpecifications)
{-# DEPRECATED dicsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The instance IDs.
--
-- Default: Describes all your instances.
-- Constraints: Maximum 1000 explicitly specified instance IDs.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dicsInstanceIds :: Lens.Lens' DescribeInstanceCreditSpecifications (Lude.Maybe [Lude.Text])
dicsInstanceIds = Lens.lens (instanceIds :: DescribeInstanceCreditSpecifications -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceIds = a} :: DescribeInstanceCreditSpecifications)
{-# DEPRECATED dicsInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dicsDryRun :: Lens.Lens' DescribeInstanceCreditSpecifications (Lude.Maybe Lude.Bool)
dicsDryRun = Lens.lens (dryRun :: DescribeInstanceCreditSpecifications -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeInstanceCreditSpecifications)
{-# DEPRECATED dicsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000. You cannot specify this parameter and the instance IDs parameter in the same call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dicsMaxResults :: Lens.Lens' DescribeInstanceCreditSpecifications (Lude.Maybe Lude.Natural)
dicsMaxResults = Lens.lens (maxResults :: DescribeInstanceCreditSpecifications -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeInstanceCreditSpecifications)
{-# DEPRECATED dicsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeInstanceCreditSpecifications where
  page rq rs
    | Page.stop (rs Lens.^. dicsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dicsrsInstanceCreditSpecifications) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dicsNextToken Lens..~ rs Lens.^. dicsrsNextToken

instance Lude.AWSRequest DescribeInstanceCreditSpecifications where
  type
    Rs DescribeInstanceCreditSpecifications =
      DescribeInstanceCreditSpecificationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeInstanceCreditSpecificationsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "instanceCreditSpecificationSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstanceCreditSpecifications where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeInstanceCreditSpecifications where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstanceCreditSpecifications where
  toQuery DescribeInstanceCreditSpecifications' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeInstanceCreditSpecifications" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery (Lude.toQueryList "InstanceId" Lude.<$> instanceIds),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeInstanceCreditSpecificationsResponse' smart constructor.
data DescribeInstanceCreditSpecificationsResponse = DescribeInstanceCreditSpecificationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the credit option for CPU usage of an instance.
    instanceCreditSpecifications :: Lude.Maybe [InstanceCreditSpecification],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceCreditSpecificationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'instanceCreditSpecifications' - Information about the credit option for CPU usage of an instance.
-- * 'responseStatus' - The response status code.
mkDescribeInstanceCreditSpecificationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstanceCreditSpecificationsResponse
mkDescribeInstanceCreditSpecificationsResponse pResponseStatus_ =
  DescribeInstanceCreditSpecificationsResponse'
    { nextToken =
        Lude.Nothing,
      instanceCreditSpecifications = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dicsrsNextToken :: Lens.Lens' DescribeInstanceCreditSpecificationsResponse (Lude.Maybe Lude.Text)
dicsrsNextToken = Lens.lens (nextToken :: DescribeInstanceCreditSpecificationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstanceCreditSpecificationsResponse)
{-# DEPRECATED dicsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the credit option for CPU usage of an instance.
--
-- /Note:/ Consider using 'instanceCreditSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dicsrsInstanceCreditSpecifications :: Lens.Lens' DescribeInstanceCreditSpecificationsResponse (Lude.Maybe [InstanceCreditSpecification])
dicsrsInstanceCreditSpecifications = Lens.lens (instanceCreditSpecifications :: DescribeInstanceCreditSpecificationsResponse -> Lude.Maybe [InstanceCreditSpecification]) (\s a -> s {instanceCreditSpecifications = a} :: DescribeInstanceCreditSpecificationsResponse)
{-# DEPRECATED dicsrsInstanceCreditSpecifications "Use generic-lens or generic-optics with 'instanceCreditSpecifications' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dicsrsResponseStatus :: Lens.Lens' DescribeInstanceCreditSpecificationsResponse Lude.Int
dicsrsResponseStatus = Lens.lens (responseStatus :: DescribeInstanceCreditSpecificationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstanceCreditSpecificationsResponse)
{-# DEPRECATED dicsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
