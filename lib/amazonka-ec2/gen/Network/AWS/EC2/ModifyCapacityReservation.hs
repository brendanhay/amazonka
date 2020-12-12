{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyCapacityReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a Capacity Reservation's capacity and the conditions under which it is to be released. You cannot change a Capacity Reservation's instance type, EBS optimization, instance store settings, platform, Availability Zone, or instance eligibility. If you need to modify any of these attributes, we recommend that you cancel the Capacity Reservation, and then create a new one with the required attributes.
module Network.AWS.EC2.ModifyCapacityReservation
  ( -- * Creating a request
    ModifyCapacityReservation (..),
    mkModifyCapacityReservation,

    -- ** Request lenses
    mcrInstanceCount,
    mcrEndDate,
    mcrEndDateType,
    mcrDryRun,
    mcrCapacityReservationId,

    -- * Destructuring the response
    ModifyCapacityReservationResponse (..),
    mkModifyCapacityReservationResponse,

    -- ** Response lenses
    mcrrsReturn,
    mcrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyCapacityReservation' smart constructor.
data ModifyCapacityReservation = ModifyCapacityReservation'
  { instanceCount ::
      Lude.Maybe Lude.Int,
    endDate :: Lude.Maybe Lude.DateTime,
    endDateType :: Lude.Maybe EndDateType,
    dryRun :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'ModifyCapacityReservation' with the minimum fields required to make a request.
--
-- * 'capacityReservationId' - The ID of the Capacity Reservation.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'endDate' - The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time.
--
-- The Capacity Reservation is cancelled within an hour from the specified time. For example, if you specify 5/31/2019, 13:30:55, the Capacity Reservation is guaranteed to end between 13:30:55 and 14:30:55 on 5/31/2019.
-- You must provide an @EndDate@ value if @EndDateType@ is @limited@ . Omit @EndDate@ if @EndDateType@ is @unlimited@ .
-- * 'endDateType' - Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:
--
--
--     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it. Do not provide an @EndDate@ value if @EndDateType@ is @unlimited@ .
--
--
--     * @limited@ - The Capacity Reservation expires automatically at a specified date and time. You must provide an @EndDate@ value if @EndDateType@ is @limited@ .
--
--
-- * 'instanceCount' - The number of instances for which to reserve capacity.
mkModifyCapacityReservation ::
  -- | 'capacityReservationId'
  Lude.Text ->
  ModifyCapacityReservation
mkModifyCapacityReservation pCapacityReservationId_ =
  ModifyCapacityReservation'
    { instanceCount = Lude.Nothing,
      endDate = Lude.Nothing,
      endDateType = Lude.Nothing,
      dryRun = Lude.Nothing,
      capacityReservationId = pCapacityReservationId_
    }

-- | The number of instances for which to reserve capacity.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrInstanceCount :: Lens.Lens' ModifyCapacityReservation (Lude.Maybe Lude.Int)
mcrInstanceCount = Lens.lens (instanceCount :: ModifyCapacityReservation -> Lude.Maybe Lude.Int) (\s a -> s {instanceCount = a} :: ModifyCapacityReservation)
{-# DEPRECATED mcrInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to @expired@ when it reaches its end date and time.
--
-- The Capacity Reservation is cancelled within an hour from the specified time. For example, if you specify 5/31/2019, 13:30:55, the Capacity Reservation is guaranteed to end between 13:30:55 and 14:30:55 on 5/31/2019.
-- You must provide an @EndDate@ value if @EndDateType@ is @limited@ . Omit @EndDate@ if @EndDateType@ is @unlimited@ .
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrEndDate :: Lens.Lens' ModifyCapacityReservation (Lude.Maybe Lude.DateTime)
mcrEndDate = Lens.lens (endDate :: ModifyCapacityReservation -> Lude.Maybe Lude.DateTime) (\s a -> s {endDate = a} :: ModifyCapacityReservation)
{-# DEPRECATED mcrEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:
--
--
--     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it. Do not provide an @EndDate@ value if @EndDateType@ is @unlimited@ .
--
--
--     * @limited@ - The Capacity Reservation expires automatically at a specified date and time. You must provide an @EndDate@ value if @EndDateType@ is @limited@ .
--
--
--
-- /Note:/ Consider using 'endDateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrEndDateType :: Lens.Lens' ModifyCapacityReservation (Lude.Maybe EndDateType)
mcrEndDateType = Lens.lens (endDateType :: ModifyCapacityReservation -> Lude.Maybe EndDateType) (\s a -> s {endDateType = a} :: ModifyCapacityReservation)
{-# DEPRECATED mcrEndDateType "Use generic-lens or generic-optics with 'endDateType' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrDryRun :: Lens.Lens' ModifyCapacityReservation (Lude.Maybe Lude.Bool)
mcrDryRun = Lens.lens (dryRun :: ModifyCapacityReservation -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyCapacityReservation)
{-# DEPRECATED mcrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrCapacityReservationId :: Lens.Lens' ModifyCapacityReservation Lude.Text
mcrCapacityReservationId = Lens.lens (capacityReservationId :: ModifyCapacityReservation -> Lude.Text) (\s a -> s {capacityReservationId = a} :: ModifyCapacityReservation)
{-# DEPRECATED mcrCapacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead." #-}

instance Lude.AWSRequest ModifyCapacityReservation where
  type
    Rs ModifyCapacityReservation =
      ModifyCapacityReservationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyCapacityReservationResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyCapacityReservation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyCapacityReservation where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyCapacityReservation where
  toQuery ModifyCapacityReservation' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyCapacityReservation" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceCount" Lude.=: instanceCount,
        "EndDate" Lude.=: endDate,
        "EndDateType" Lude.=: endDateType,
        "DryRun" Lude.=: dryRun,
        "CapacityReservationId" Lude.=: capacityReservationId
      ]

-- | /See:/ 'mkModifyCapacityReservationResponse' smart constructor.
data ModifyCapacityReservationResponse = ModifyCapacityReservationResponse'
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

-- | Creates a value of 'ModifyCapacityReservationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
mkModifyCapacityReservationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyCapacityReservationResponse
mkModifyCapacityReservationResponse pResponseStatus_ =
  ModifyCapacityReservationResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrrsReturn :: Lens.Lens' ModifyCapacityReservationResponse (Lude.Maybe Lude.Bool)
mcrrsReturn = Lens.lens (return :: ModifyCapacityReservationResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: ModifyCapacityReservationResponse)
{-# DEPRECATED mcrrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrrsResponseStatus :: Lens.Lens' ModifyCapacityReservationResponse Lude.Int
mcrrsResponseStatus = Lens.lens (responseStatus :: ModifyCapacityReservationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyCapacityReservationResponse)
{-# DEPRECATED mcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
