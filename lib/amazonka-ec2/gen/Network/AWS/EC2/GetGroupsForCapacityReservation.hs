{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetGroupsForCapacityReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource groups to which a Capacity Reservation has been added.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetGroupsForCapacityReservation
  ( -- * Creating a request
    GetGroupsForCapacityReservation (..),
    mkGetGroupsForCapacityReservation,

    -- ** Request lenses
    ggfcrCapacityReservationId,
    ggfcrNextToken,
    ggfcrDryRun,
    ggfcrMaxResults,

    -- * Destructuring the response
    GetGroupsForCapacityReservationResponse (..),
    mkGetGroupsForCapacityReservationResponse,

    -- ** Response lenses
    ggfcrrsNextToken,
    ggfcrrsCapacityReservationGroups,
    ggfcrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetGroupsForCapacityReservation' smart constructor.
data GetGroupsForCapacityReservation = GetGroupsForCapacityReservation'
  { -- | The ID of the Capacity Reservation.
    capacityReservationId :: Lude.Text,
    -- | The token to use to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroupsForCapacityReservation' with the minimum fields required to make a request.
--
-- * 'capacityReservationId' - The ID of the Capacity Reservation.
-- * 'nextToken' - The token to use to retrieve the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
mkGetGroupsForCapacityReservation ::
  -- | 'capacityReservationId'
  Lude.Text ->
  GetGroupsForCapacityReservation
mkGetGroupsForCapacityReservation pCapacityReservationId_ =
  GetGroupsForCapacityReservation'
    { capacityReservationId =
        pCapacityReservationId_,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggfcrCapacityReservationId :: Lens.Lens' GetGroupsForCapacityReservation Lude.Text
ggfcrCapacityReservationId = Lens.lens (capacityReservationId :: GetGroupsForCapacityReservation -> Lude.Text) (\s a -> s {capacityReservationId = a} :: GetGroupsForCapacityReservation)
{-# DEPRECATED ggfcrCapacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead." #-}

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggfcrNextToken :: Lens.Lens' GetGroupsForCapacityReservation (Lude.Maybe Lude.Text)
ggfcrNextToken = Lens.lens (nextToken :: GetGroupsForCapacityReservation -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetGroupsForCapacityReservation)
{-# DEPRECATED ggfcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggfcrDryRun :: Lens.Lens' GetGroupsForCapacityReservation (Lude.Maybe Lude.Bool)
ggfcrDryRun = Lens.lens (dryRun :: GetGroupsForCapacityReservation -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetGroupsForCapacityReservation)
{-# DEPRECATED ggfcrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggfcrMaxResults :: Lens.Lens' GetGroupsForCapacityReservation (Lude.Maybe Lude.Natural)
ggfcrMaxResults = Lens.lens (maxResults :: GetGroupsForCapacityReservation -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetGroupsForCapacityReservation)
{-# DEPRECATED ggfcrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetGroupsForCapacityReservation where
  page rq rs
    | Page.stop (rs Lens.^. ggfcrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ggfcrrsCapacityReservationGroups) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ggfcrNextToken Lens..~ rs Lens.^. ggfcrrsNextToken

instance Lude.AWSRequest GetGroupsForCapacityReservation where
  type
    Rs GetGroupsForCapacityReservation =
      GetGroupsForCapacityReservationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetGroupsForCapacityReservationResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "capacityReservationGroupSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGroupsForCapacityReservation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetGroupsForCapacityReservation where
  toPath = Lude.const "/"

instance Lude.ToQuery GetGroupsForCapacityReservation where
  toQuery GetGroupsForCapacityReservation' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetGroupsForCapacityReservation" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "CapacityReservationId" Lude.=: capacityReservationId,
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkGetGroupsForCapacityReservationResponse' smart constructor.
data GetGroupsForCapacityReservationResponse = GetGroupsForCapacityReservationResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the resource groups to which the Capacity Reservation has been added.
    capacityReservationGroups :: Lude.Maybe [CapacityReservationGroup],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroupsForCapacityReservationResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'capacityReservationGroups' - Information about the resource groups to which the Capacity Reservation has been added.
-- * 'responseStatus' - The response status code.
mkGetGroupsForCapacityReservationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetGroupsForCapacityReservationResponse
mkGetGroupsForCapacityReservationResponse pResponseStatus_ =
  GetGroupsForCapacityReservationResponse'
    { nextToken =
        Lude.Nothing,
      capacityReservationGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggfcrrsNextToken :: Lens.Lens' GetGroupsForCapacityReservationResponse (Lude.Maybe Lude.Text)
ggfcrrsNextToken = Lens.lens (nextToken :: GetGroupsForCapacityReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetGroupsForCapacityReservationResponse)
{-# DEPRECATED ggfcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the resource groups to which the Capacity Reservation has been added.
--
-- /Note:/ Consider using 'capacityReservationGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggfcrrsCapacityReservationGroups :: Lens.Lens' GetGroupsForCapacityReservationResponse (Lude.Maybe [CapacityReservationGroup])
ggfcrrsCapacityReservationGroups = Lens.lens (capacityReservationGroups :: GetGroupsForCapacityReservationResponse -> Lude.Maybe [CapacityReservationGroup]) (\s a -> s {capacityReservationGroups = a} :: GetGroupsForCapacityReservationResponse)
{-# DEPRECATED ggfcrrsCapacityReservationGroups "Use generic-lens or generic-optics with 'capacityReservationGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggfcrrsResponseStatus :: Lens.Lens' GetGroupsForCapacityReservationResponse Lude.Int
ggfcrrsResponseStatus = Lens.lens (responseStatus :: GetGroupsForCapacityReservationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGroupsForCapacityReservationResponse)
{-# DEPRECATED ggfcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
