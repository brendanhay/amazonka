{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelReservedInstancesListing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified Reserved Instance listing in the Reserved Instance Marketplace.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CancelReservedInstancesListing
  ( -- * Creating a request
    CancelReservedInstancesListing (..),
    mkCancelReservedInstancesListing,

    -- ** Request lenses
    crilReservedInstancesListingId,

    -- * Destructuring the response
    CancelReservedInstancesListingResponse (..),
    mkCancelReservedInstancesListingResponse,

    -- ** Response lenses
    crilrsReservedInstancesListings,
    crilrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CancelReservedInstancesListing.
--
-- /See:/ 'mkCancelReservedInstancesListing' smart constructor.
newtype CancelReservedInstancesListing = CancelReservedInstancesListing'
  { -- | The ID of the Reserved Instance listing.
    reservedInstancesListingId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelReservedInstancesListing' with the minimum fields required to make a request.
--
-- * 'reservedInstancesListingId' - The ID of the Reserved Instance listing.
mkCancelReservedInstancesListing ::
  -- | 'reservedInstancesListingId'
  Lude.Text ->
  CancelReservedInstancesListing
mkCancelReservedInstancesListing pReservedInstancesListingId_ =
  CancelReservedInstancesListing'
    { reservedInstancesListingId =
        pReservedInstancesListingId_
    }

-- | The ID of the Reserved Instance listing.
--
-- /Note:/ Consider using 'reservedInstancesListingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilReservedInstancesListingId :: Lens.Lens' CancelReservedInstancesListing Lude.Text
crilReservedInstancesListingId = Lens.lens (reservedInstancesListingId :: CancelReservedInstancesListing -> Lude.Text) (\s a -> s {reservedInstancesListingId = a} :: CancelReservedInstancesListing)
{-# DEPRECATED crilReservedInstancesListingId "Use generic-lens or generic-optics with 'reservedInstancesListingId' instead." #-}

instance Lude.AWSRequest CancelReservedInstancesListing where
  type
    Rs CancelReservedInstancesListing =
      CancelReservedInstancesListingResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CancelReservedInstancesListingResponse'
            Lude.<$> ( x Lude..@? "reservedInstancesListingsSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelReservedInstancesListing where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CancelReservedInstancesListing where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelReservedInstancesListing where
  toQuery CancelReservedInstancesListing' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CancelReservedInstancesListing" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ReservedInstancesListingId" Lude.=: reservedInstancesListingId
      ]

-- | Contains the output of CancelReservedInstancesListing.
--
-- /See:/ 'mkCancelReservedInstancesListingResponse' smart constructor.
data CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse'
  { -- | The Reserved Instance listing.
    reservedInstancesListings :: Lude.Maybe [ReservedInstancesListing],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelReservedInstancesListingResponse' with the minimum fields required to make a request.
--
-- * 'reservedInstancesListings' - The Reserved Instance listing.
-- * 'responseStatus' - The response status code.
mkCancelReservedInstancesListingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelReservedInstancesListingResponse
mkCancelReservedInstancesListingResponse pResponseStatus_ =
  CancelReservedInstancesListingResponse'
    { reservedInstancesListings =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Reserved Instance listing.
--
-- /Note:/ Consider using 'reservedInstancesListings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilrsReservedInstancesListings :: Lens.Lens' CancelReservedInstancesListingResponse (Lude.Maybe [ReservedInstancesListing])
crilrsReservedInstancesListings = Lens.lens (reservedInstancesListings :: CancelReservedInstancesListingResponse -> Lude.Maybe [ReservedInstancesListing]) (\s a -> s {reservedInstancesListings = a} :: CancelReservedInstancesListingResponse)
{-# DEPRECATED crilrsReservedInstancesListings "Use generic-lens or generic-optics with 'reservedInstancesListings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crilrsResponseStatus :: Lens.Lens' CancelReservedInstancesListingResponse Lude.Int
crilrsResponseStatus = Lens.lens (responseStatus :: CancelReservedInstancesListingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelReservedInstancesListingResponse)
{-# DEPRECATED crilrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
