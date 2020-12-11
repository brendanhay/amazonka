{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeReservedInstancesListings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your account's Reserved Instance listings in the Reserved Instance Marketplace.
--
-- The Reserved Instance Marketplace matches sellers who want to resell Reserved Instance capacity that they no longer need with buyers who want to purchase additional capacity. Reserved Instances bought and sold through the Reserved Instance Marketplace work like any other Reserved Instances.
-- As a seller, you choose to list some or all of your Reserved Instances, and you specify the upfront price to receive for them. Your Reserved Instances are then listed in the Reserved Instance Marketplace and are available for purchase.
-- As a buyer, you specify the configuration of the Reserved Instance to purchase, and the Marketplace matches what you're searching for with what's available. The Marketplace first sells the lowest priced Reserved Instances to you, and continues to sell available Reserved Instance listings to you until your demand is met. You are charged based on the total price of all of the listings that you purchase.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DescribeReservedInstancesListings
  ( -- * Creating a request
    DescribeReservedInstancesListings (..),
    mkDescribeReservedInstancesListings,

    -- ** Request lenses
    drilFilters,
    drilReservedInstancesId,
    drilReservedInstancesListingId,

    -- * Destructuring the response
    DescribeReservedInstancesListingsResponse (..),
    mkDescribeReservedInstancesListingsResponse,

    -- ** Response lenses
    drilrsReservedInstancesListings,
    drilrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeReservedInstancesListings.
--
-- /See:/ 'mkDescribeReservedInstancesListings' smart constructor.
data DescribeReservedInstancesListings = DescribeReservedInstancesListings'
  { filters ::
      Lude.Maybe [Filter],
    reservedInstancesId ::
      Lude.Maybe Lude.Text,
    reservedInstancesListingId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedInstancesListings' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
--
--
--     * @reserved-instances-id@ - The ID of the Reserved Instances.
--
--
--     * @reserved-instances-listing-id@ - The ID of the Reserved Instances listing.
--
--
--     * @status@ - The status of the Reserved Instance listing (@pending@ | @active@ | @cancelled@ | @closed@ ).
--
--
--     * @status-message@ - The reason for the status.
--
--
-- * 'reservedInstancesId' - One or more Reserved Instance IDs.
-- * 'reservedInstancesListingId' - One or more Reserved Instance listing IDs.
mkDescribeReservedInstancesListings ::
  DescribeReservedInstancesListings
mkDescribeReservedInstancesListings =
  DescribeReservedInstancesListings'
    { filters = Lude.Nothing,
      reservedInstancesId = Lude.Nothing,
      reservedInstancesListingId = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @reserved-instances-id@ - The ID of the Reserved Instances.
--
--
--     * @reserved-instances-listing-id@ - The ID of the Reserved Instances listing.
--
--
--     * @status@ - The status of the Reserved Instance listing (@pending@ | @active@ | @cancelled@ | @closed@ ).
--
--
--     * @status-message@ - The reason for the status.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drilFilters :: Lens.Lens' DescribeReservedInstancesListings (Lude.Maybe [Filter])
drilFilters = Lens.lens (filters :: DescribeReservedInstancesListings -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeReservedInstancesListings)
{-# DEPRECATED drilFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | One or more Reserved Instance IDs.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drilReservedInstancesId :: Lens.Lens' DescribeReservedInstancesListings (Lude.Maybe Lude.Text)
drilReservedInstancesId = Lens.lens (reservedInstancesId :: DescribeReservedInstancesListings -> Lude.Maybe Lude.Text) (\s a -> s {reservedInstancesId = a} :: DescribeReservedInstancesListings)
{-# DEPRECATED drilReservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead." #-}

-- | One or more Reserved Instance listing IDs.
--
-- /Note:/ Consider using 'reservedInstancesListingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drilReservedInstancesListingId :: Lens.Lens' DescribeReservedInstancesListings (Lude.Maybe Lude.Text)
drilReservedInstancesListingId = Lens.lens (reservedInstancesListingId :: DescribeReservedInstancesListings -> Lude.Maybe Lude.Text) (\s a -> s {reservedInstancesListingId = a} :: DescribeReservedInstancesListings)
{-# DEPRECATED drilReservedInstancesListingId "Use generic-lens or generic-optics with 'reservedInstancesListingId' instead." #-}

instance Lude.AWSRequest DescribeReservedInstancesListings where
  type
    Rs DescribeReservedInstancesListings =
      DescribeReservedInstancesListingsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeReservedInstancesListingsResponse'
            Lude.<$> ( x Lude..@? "reservedInstancesListingsSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReservedInstancesListings where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeReservedInstancesListings where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReservedInstancesListings where
  toQuery DescribeReservedInstancesListings' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeReservedInstancesListings" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "ReservedInstancesId" Lude.=: reservedInstancesId,
        "ReservedInstancesListingId" Lude.=: reservedInstancesListingId
      ]

-- | Contains the output of DescribeReservedInstancesListings.
--
-- /See:/ 'mkDescribeReservedInstancesListingsResponse' smart constructor.
data DescribeReservedInstancesListingsResponse = DescribeReservedInstancesListingsResponse'
  { reservedInstancesListings ::
      Lude.Maybe
        [ReservedInstancesListing],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedInstancesListingsResponse' with the minimum fields required to make a request.
--
-- * 'reservedInstancesListings' - Information about the Reserved Instance listing.
-- * 'responseStatus' - The response status code.
mkDescribeReservedInstancesListingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReservedInstancesListingsResponse
mkDescribeReservedInstancesListingsResponse pResponseStatus_ =
  DescribeReservedInstancesListingsResponse'
    { reservedInstancesListings =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Reserved Instance listing.
--
-- /Note:/ Consider using 'reservedInstancesListings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drilrsReservedInstancesListings :: Lens.Lens' DescribeReservedInstancesListingsResponse (Lude.Maybe [ReservedInstancesListing])
drilrsReservedInstancesListings = Lens.lens (reservedInstancesListings :: DescribeReservedInstancesListingsResponse -> Lude.Maybe [ReservedInstancesListing]) (\s a -> s {reservedInstancesListings = a} :: DescribeReservedInstancesListingsResponse)
{-# DEPRECATED drilrsReservedInstancesListings "Use generic-lens or generic-optics with 'reservedInstancesListings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drilrsResponseStatus :: Lens.Lens' DescribeReservedInstancesListingsResponse Lude.Int
drilrsResponseStatus = Lens.lens (responseStatus :: DescribeReservedInstancesListingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReservedInstancesListingsResponse)
{-# DEPRECATED drilrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
