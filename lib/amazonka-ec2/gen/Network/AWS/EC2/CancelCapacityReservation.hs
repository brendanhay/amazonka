{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelCapacityReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified Capacity Reservation, releases the reserved capacity, and changes the Capacity Reservation's state to @cancelled@ .
--
-- Instances running in the reserved capacity continue running until you stop them. Stopped instances that target the Capacity Reservation can no longer launch. Modify these instances to either target a different Capacity Reservation, launch On-Demand Instance capacity, or run in any open Capacity Reservation that has matching attributes and sufficient capacity.
module Network.AWS.EC2.CancelCapacityReservation
  ( -- * Creating a request
    CancelCapacityReservation (..),
    mkCancelCapacityReservation,

    -- ** Request lenses
    canDryRun,
    canCapacityReservationId,

    -- * Destructuring the response
    CancelCapacityReservationResponse (..),
    mkCancelCapacityReservationResponse,

    -- ** Response lenses
    canrsReturn,
    canrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelCapacityReservation' smart constructor.
data CancelCapacityReservation = CancelCapacityReservation'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    capacityReservationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelCapacityReservation' with the minimum fields required to make a request.
--
-- * 'capacityReservationId' - The ID of the Capacity Reservation to be cancelled.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCancelCapacityReservation ::
  -- | 'capacityReservationId'
  Lude.Text ->
  CancelCapacityReservation
mkCancelCapacityReservation pCapacityReservationId_ =
  CancelCapacityReservation'
    { dryRun = Lude.Nothing,
      capacityReservationId = pCapacityReservationId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canDryRun :: Lens.Lens' CancelCapacityReservation (Lude.Maybe Lude.Bool)
canDryRun = Lens.lens (dryRun :: CancelCapacityReservation -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CancelCapacityReservation)
{-# DEPRECATED canDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the Capacity Reservation to be cancelled.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canCapacityReservationId :: Lens.Lens' CancelCapacityReservation Lude.Text
canCapacityReservationId = Lens.lens (capacityReservationId :: CancelCapacityReservation -> Lude.Text) (\s a -> s {capacityReservationId = a} :: CancelCapacityReservation)
{-# DEPRECATED canCapacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead." #-}

instance Lude.AWSRequest CancelCapacityReservation where
  type
    Rs CancelCapacityReservation =
      CancelCapacityReservationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CancelCapacityReservationResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelCapacityReservation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CancelCapacityReservation where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelCapacityReservation where
  toQuery CancelCapacityReservation' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CancelCapacityReservation" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "CapacityReservationId" Lude.=: capacityReservationId
      ]

-- | /See:/ 'mkCancelCapacityReservationResponse' smart constructor.
data CancelCapacityReservationResponse = CancelCapacityReservationResponse'
  { return ::
      Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'CancelCapacityReservationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
mkCancelCapacityReservationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelCapacityReservationResponse
mkCancelCapacityReservationResponse pResponseStatus_ =
  CancelCapacityReservationResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canrsReturn :: Lens.Lens' CancelCapacityReservationResponse (Lude.Maybe Lude.Bool)
canrsReturn = Lens.lens (return :: CancelCapacityReservationResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: CancelCapacityReservationResponse)
{-# DEPRECATED canrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
canrsResponseStatus :: Lens.Lens' CancelCapacityReservationResponse Lude.Int
canrsResponseStatus = Lens.lens (responseStatus :: CancelCapacityReservationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelCapacityReservationResponse)
{-# DEPRECATED canrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
