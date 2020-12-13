{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeHostReservations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes reservations that are associated with Dedicated Hosts in your account.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeHostReservations
  ( -- * Creating a request
    DescribeHostReservations (..),
    mkDescribeHostReservations,

    -- ** Request lenses
    dhrNextToken,
    dhrHostReservationIdSet,
    dhrFilter,
    dhrMaxResults,

    -- * Destructuring the response
    DescribeHostReservationsResponse (..),
    mkDescribeHostReservationsResponse,

    -- ** Response lenses
    dhrrsNextToken,
    dhrrsHostReservationSet,
    dhrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeHostReservations' smart constructor.
data DescribeHostReservations = DescribeHostReservations'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The host reservation IDs.
    hostReservationIdSet :: Lude.Maybe [Lude.Text],
    -- | The filters.
    --
    --
    --     * @instance-family@ - The instance family (for example, @m4@ ).
    --
    --
    --     * @payment-option@ - The payment option (@NoUpfront@ | @PartialUpfront@ | @AllUpfront@ ).
    --
    --
    --     * @state@ - The state of the reservation (@payment-pending@ | @payment-failed@ | @active@ | @retired@ ).
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    filter :: Lude.Maybe [Filter],
    -- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHostReservations' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results.
-- * 'hostReservationIdSet' - The host reservation IDs.
-- * 'filter' - The filters.
--
--
--     * @instance-family@ - The instance family (for example, @m4@ ).
--
--
--     * @payment-option@ - The payment option (@NoUpfront@ | @PartialUpfront@ | @AllUpfront@ ).
--
--
--     * @state@ - The state of the reservation (@payment-pending@ | @payment-failed@ | @active@ | @retired@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
mkDescribeHostReservations ::
  DescribeHostReservations
mkDescribeHostReservations =
  DescribeHostReservations'
    { nextToken = Lude.Nothing,
      hostReservationIdSet = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrNextToken :: Lens.Lens' DescribeHostReservations (Lude.Maybe Lude.Text)
dhrNextToken = Lens.lens (nextToken :: DescribeHostReservations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeHostReservations)
{-# DEPRECATED dhrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The host reservation IDs.
--
-- /Note:/ Consider using 'hostReservationIdSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrHostReservationIdSet :: Lens.Lens' DescribeHostReservations (Lude.Maybe [Lude.Text])
dhrHostReservationIdSet = Lens.lens (hostReservationIdSet :: DescribeHostReservations -> Lude.Maybe [Lude.Text]) (\s a -> s {hostReservationIdSet = a} :: DescribeHostReservations)
{-# DEPRECATED dhrHostReservationIdSet "Use generic-lens or generic-optics with 'hostReservationIdSet' instead." #-}

-- | The filters.
--
--
--     * @instance-family@ - The instance family (for example, @m4@ ).
--
--
--     * @payment-option@ - The payment option (@NoUpfront@ | @PartialUpfront@ | @AllUpfront@ ).
--
--
--     * @state@ - The state of the reservation (@payment-pending@ | @payment-failed@ | @active@ | @retired@ ).
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
dhrFilter :: Lens.Lens' DescribeHostReservations (Lude.Maybe [Filter])
dhrFilter = Lens.lens (filter :: DescribeHostReservations -> Lude.Maybe [Filter]) (\s a -> s {filter = a} :: DescribeHostReservations)
{-# DEPRECATED dhrFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrMaxResults :: Lens.Lens' DescribeHostReservations (Lude.Maybe Lude.Int)
dhrMaxResults = Lens.lens (maxResults :: DescribeHostReservations -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeHostReservations)
{-# DEPRECATED dhrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeHostReservations where
  page rq rs
    | Page.stop (rs Lens.^. dhrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dhrrsHostReservationSet) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dhrNextToken Lens..~ rs Lens.^. dhrrsNextToken

instance Lude.AWSRequest DescribeHostReservations where
  type Rs DescribeHostReservations = DescribeHostReservationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeHostReservationsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "hostReservationSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeHostReservations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeHostReservations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeHostReservations where
  toQuery DescribeHostReservations' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeHostReservations" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          ( Lude.toQueryList "HostReservationIdSet"
              Lude.<$> hostReservationIdSet
          ),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filter),
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeHostReservationsResponse' smart constructor.
data DescribeHostReservationsResponse = DescribeHostReservationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Details about the reservation's configuration.
    hostReservationSet :: Lude.Maybe [HostReservation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHostReservationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'hostReservationSet' - Details about the reservation's configuration.
-- * 'responseStatus' - The response status code.
mkDescribeHostReservationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeHostReservationsResponse
mkDescribeHostReservationsResponse pResponseStatus_ =
  DescribeHostReservationsResponse'
    { nextToken = Lude.Nothing,
      hostReservationSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrsNextToken :: Lens.Lens' DescribeHostReservationsResponse (Lude.Maybe Lude.Text)
dhrrsNextToken = Lens.lens (nextToken :: DescribeHostReservationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeHostReservationsResponse)
{-# DEPRECATED dhrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Details about the reservation's configuration.
--
-- /Note:/ Consider using 'hostReservationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrsHostReservationSet :: Lens.Lens' DescribeHostReservationsResponse (Lude.Maybe [HostReservation])
dhrrsHostReservationSet = Lens.lens (hostReservationSet :: DescribeHostReservationsResponse -> Lude.Maybe [HostReservation]) (\s a -> s {hostReservationSet = a} :: DescribeHostReservationsResponse)
{-# DEPRECATED dhrrsHostReservationSet "Use generic-lens or generic-optics with 'hostReservationSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrsResponseStatus :: Lens.Lens' DescribeHostReservationsResponse Lude.Int
dhrrsResponseStatus = Lens.lens (responseStatus :: DescribeHostReservationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeHostReservationsResponse)
{-# DEPRECATED dhrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
