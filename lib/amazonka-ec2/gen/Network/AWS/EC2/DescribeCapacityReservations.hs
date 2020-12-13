{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeCapacityReservations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your Capacity Reservations. The results describe only the Capacity Reservations in the AWS Region that you're currently using.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeCapacityReservations
  ( -- * Creating a request
    DescribeCapacityReservations (..),
    mkDescribeCapacityReservations,

    -- ** Request lenses
    dcrCapacityReservationIds,
    dcrFilters,
    dcrNextToken,
    dcrDryRun,
    dcrMaxResults,

    -- * Destructuring the response
    DescribeCapacityReservationsResponse (..),
    mkDescribeCapacityReservationsResponse,

    -- ** Response lenses
    dcrrsCapacityReservations,
    dcrrsNextToken,
    dcrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCapacityReservations' smart constructor.
data DescribeCapacityReservations = DescribeCapacityReservations'
  { -- | The ID of the Capacity Reservation.
    capacityReservationIds :: Lude.Maybe [Lude.Text],
    -- | One or more filters.
    --
    --
    --     * @instance-type@ - The type of instance for which the Capacity Reservation reserves capacity.
    --
    --
    --     * @owner-id@ - The ID of the AWS account that owns the Capacity Reservation.
    --
    --
    --     * @availability-zone-id@ - The Availability Zone ID of the Capacity Reservation.
    --
    --
    --     * @instance-platform@ - The type of operating system for which the Capacity Reservation reserves capacity.
    --
    --
    --     * @availability-zone@ - The Availability Zone ID of the Capacity Reservation.
    --
    --
    --     * @tenancy@ - Indicates the tenancy of the Capacity Reservation. A Capacity Reservation can have one of the following tenancy settings:
    --
    --     * @default@ - The Capacity Reservation is created on hardware that is shared with other AWS accounts.
    --
    --
    --     * @dedicated@ - The Capacity Reservation is created on single-tenant hardware that is dedicated to a single AWS account.
    --
    --
    --
    --
    --     * @state@ - The current state of the Capacity Reservation. A Capacity Reservation can be in one of the following states:
    --
    --     * @active@ - The Capacity Reservation is active and the capacity is available for your use.
    --
    --
    --     * @expired@ - The Capacity Reservation expired automatically at the date and time specified in your request. The reserved capacity is no longer available for your use.
    --
    --
    --     * @cancelled@ - The Capacity Reservation was manually cancelled. The reserved capacity is no longer available for your use.
    --
    --
    --     * @pending@ - The Capacity Reservation request was successful but the capacity provisioning is still pending.
    --
    --
    --     * @failed@ - The Capacity Reservation request has failed. A request might fail due to invalid request parameters, capacity constraints, or instance limit constraints. Failed requests are retained for 60 minutes.
    --
    --
    --
    --
    --     * @end-date@ - The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to expired when it reaches its end date and time.
    --
    --
    --     * @end-date-type@ - Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:
    --
    --     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it.
    --
    --
    --     * @limited@ - The Capacity Reservation expires automatically at a specified date and time.
    --
    --
    --
    --
    --     * @instance-match-criteria@ - Indicates the type of instance launches that the Capacity Reservation accepts. The options include:
    --
    --     * @open@ - The Capacity Reservation accepts all instances that have matching attributes (instance type, platform, and Availability Zone). Instances that have matching attributes launch into the Capacity Reservation automatically without specifying any additional parameters.
    --
    --
    --     * @targeted@ - The Capacity Reservation only accepts instances that have matching attributes (instance type, platform, and Availability Zone), and explicitly target the Capacity Reservation. This ensures that only permitted instances can use the reserved capacity.
    filters :: Lude.Maybe [Filter],
    -- | The token to use to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCapacityReservations' with the minimum fields required to make a request.
--
-- * 'capacityReservationIds' - The ID of the Capacity Reservation.
-- * 'filters' - One or more filters.
--
--
--     * @instance-type@ - The type of instance for which the Capacity Reservation reserves capacity.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the Capacity Reservation.
--
--
--     * @availability-zone-id@ - The Availability Zone ID of the Capacity Reservation.
--
--
--     * @instance-platform@ - The type of operating system for which the Capacity Reservation reserves capacity.
--
--
--     * @availability-zone@ - The Availability Zone ID of the Capacity Reservation.
--
--
--     * @tenancy@ - Indicates the tenancy of the Capacity Reservation. A Capacity Reservation can have one of the following tenancy settings:
--
--     * @default@ - The Capacity Reservation is created on hardware that is shared with other AWS accounts.
--
--
--     * @dedicated@ - The Capacity Reservation is created on single-tenant hardware that is dedicated to a single AWS account.
--
--
--
--
--     * @state@ - The current state of the Capacity Reservation. A Capacity Reservation can be in one of the following states:
--
--     * @active@ - The Capacity Reservation is active and the capacity is available for your use.
--
--
--     * @expired@ - The Capacity Reservation expired automatically at the date and time specified in your request. The reserved capacity is no longer available for your use.
--
--
--     * @cancelled@ - The Capacity Reservation was manually cancelled. The reserved capacity is no longer available for your use.
--
--
--     * @pending@ - The Capacity Reservation request was successful but the capacity provisioning is still pending.
--
--
--     * @failed@ - The Capacity Reservation request has failed. A request might fail due to invalid request parameters, capacity constraints, or instance limit constraints. Failed requests are retained for 60 minutes.
--
--
--
--
--     * @end-date@ - The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to expired when it reaches its end date and time.
--
--
--     * @end-date-type@ - Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:
--
--     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it.
--
--
--     * @limited@ - The Capacity Reservation expires automatically at a specified date and time.
--
--
--
--
--     * @instance-match-criteria@ - Indicates the type of instance launches that the Capacity Reservation accepts. The options include:
--
--     * @open@ - The Capacity Reservation accepts all instances that have matching attributes (instance type, platform, and Availability Zone). Instances that have matching attributes launch into the Capacity Reservation automatically without specifying any additional parameters.
--
--
--     * @targeted@ - The Capacity Reservation only accepts instances that have matching attributes (instance type, platform, and Availability Zone), and explicitly target the Capacity Reservation. This ensures that only permitted instances can use the reserved capacity.
--
--
--
--
-- * 'nextToken' - The token to use to retrieve the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
mkDescribeCapacityReservations ::
  DescribeCapacityReservations
mkDescribeCapacityReservations =
  DescribeCapacityReservations'
    { capacityReservationIds =
        Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrCapacityReservationIds :: Lens.Lens' DescribeCapacityReservations (Lude.Maybe [Lude.Text])
dcrCapacityReservationIds = Lens.lens (capacityReservationIds :: DescribeCapacityReservations -> Lude.Maybe [Lude.Text]) (\s a -> s {capacityReservationIds = a} :: DescribeCapacityReservations)
{-# DEPRECATED dcrCapacityReservationIds "Use generic-lens or generic-optics with 'capacityReservationIds' instead." #-}

-- | One or more filters.
--
--
--     * @instance-type@ - The type of instance for which the Capacity Reservation reserves capacity.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the Capacity Reservation.
--
--
--     * @availability-zone-id@ - The Availability Zone ID of the Capacity Reservation.
--
--
--     * @instance-platform@ - The type of operating system for which the Capacity Reservation reserves capacity.
--
--
--     * @availability-zone@ - The Availability Zone ID of the Capacity Reservation.
--
--
--     * @tenancy@ - Indicates the tenancy of the Capacity Reservation. A Capacity Reservation can have one of the following tenancy settings:
--
--     * @default@ - The Capacity Reservation is created on hardware that is shared with other AWS accounts.
--
--
--     * @dedicated@ - The Capacity Reservation is created on single-tenant hardware that is dedicated to a single AWS account.
--
--
--
--
--     * @state@ - The current state of the Capacity Reservation. A Capacity Reservation can be in one of the following states:
--
--     * @active@ - The Capacity Reservation is active and the capacity is available for your use.
--
--
--     * @expired@ - The Capacity Reservation expired automatically at the date and time specified in your request. The reserved capacity is no longer available for your use.
--
--
--     * @cancelled@ - The Capacity Reservation was manually cancelled. The reserved capacity is no longer available for your use.
--
--
--     * @pending@ - The Capacity Reservation request was successful but the capacity provisioning is still pending.
--
--
--     * @failed@ - The Capacity Reservation request has failed. A request might fail due to invalid request parameters, capacity constraints, or instance limit constraints. Failed requests are retained for 60 minutes.
--
--
--
--
--     * @end-date@ - The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to expired when it reaches its end date and time.
--
--
--     * @end-date-type@ - Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:
--
--     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it.
--
--
--     * @limited@ - The Capacity Reservation expires automatically at a specified date and time.
--
--
--
--
--     * @instance-match-criteria@ - Indicates the type of instance launches that the Capacity Reservation accepts. The options include:
--
--     * @open@ - The Capacity Reservation accepts all instances that have matching attributes (instance type, platform, and Availability Zone). Instances that have matching attributes launch into the Capacity Reservation automatically without specifying any additional parameters.
--
--
--     * @targeted@ - The Capacity Reservation only accepts instances that have matching attributes (instance type, platform, and Availability Zone), and explicitly target the Capacity Reservation. This ensures that only permitted instances can use the reserved capacity.
--
--
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrFilters :: Lens.Lens' DescribeCapacityReservations (Lude.Maybe [Filter])
dcrFilters = Lens.lens (filters :: DescribeCapacityReservations -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeCapacityReservations)
{-# DEPRECATED dcrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrNextToken :: Lens.Lens' DescribeCapacityReservations (Lude.Maybe Lude.Text)
dcrNextToken = Lens.lens (nextToken :: DescribeCapacityReservations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeCapacityReservations)
{-# DEPRECATED dcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrDryRun :: Lens.Lens' DescribeCapacityReservations (Lude.Maybe Lude.Bool)
dcrDryRun = Lens.lens (dryRun :: DescribeCapacityReservations -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeCapacityReservations)
{-# DEPRECATED dcrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrMaxResults :: Lens.Lens' DescribeCapacityReservations (Lude.Maybe Lude.Natural)
dcrMaxResults = Lens.lens (maxResults :: DescribeCapacityReservations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeCapacityReservations)
{-# DEPRECATED dcrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeCapacityReservations where
  page rq rs
    | Page.stop (rs Lens.^. dcrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcrrsCapacityReservations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcrNextToken Lens..~ rs Lens.^. dcrrsNextToken

instance Lude.AWSRequest DescribeCapacityReservations where
  type
    Rs DescribeCapacityReservations =
      DescribeCapacityReservationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeCapacityReservationsResponse'
            Lude.<$> ( x Lude..@? "capacityReservationSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCapacityReservations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeCapacityReservations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCapacityReservations where
  toQuery DescribeCapacityReservations' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeCapacityReservations" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          ( Lude.toQueryList "CapacityReservationId"
              Lude.<$> capacityReservationIds
          ),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeCapacityReservationsResponse' smart constructor.
data DescribeCapacityReservationsResponse = DescribeCapacityReservationsResponse'
  { -- | Information about the Capacity Reservations.
    capacityReservations :: Lude.Maybe [CapacityReservation],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCapacityReservationsResponse' with the minimum fields required to make a request.
--
-- * 'capacityReservations' - Information about the Capacity Reservations.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeCapacityReservationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCapacityReservationsResponse
mkDescribeCapacityReservationsResponse pResponseStatus_ =
  DescribeCapacityReservationsResponse'
    { capacityReservations =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Capacity Reservations.
--
-- /Note:/ Consider using 'capacityReservations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCapacityReservations :: Lens.Lens' DescribeCapacityReservationsResponse (Lude.Maybe [CapacityReservation])
dcrrsCapacityReservations = Lens.lens (capacityReservations :: DescribeCapacityReservationsResponse -> Lude.Maybe [CapacityReservation]) (\s a -> s {capacityReservations = a} :: DescribeCapacityReservationsResponse)
{-# DEPRECATED dcrrsCapacityReservations "Use generic-lens or generic-optics with 'capacityReservations' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsNextToken :: Lens.Lens' DescribeCapacityReservationsResponse (Lude.Maybe Lude.Text)
dcrrsNextToken = Lens.lens (nextToken :: DescribeCapacityReservationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeCapacityReservationsResponse)
{-# DEPRECATED dcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeCapacityReservationsResponse Lude.Int
dcrrsResponseStatus = Lens.lens (responseStatus :: DescribeCapacityReservationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCapacityReservationsResponse)
{-# DEPRECATED dcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
