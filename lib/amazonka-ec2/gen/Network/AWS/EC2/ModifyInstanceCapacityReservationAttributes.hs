{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyInstanceCapacityReservationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the Capacity Reservation settings for a stopped instance. Use this action to configure an instance to target a specific Capacity Reservation, run in any @open@ Capacity Reservation with matching attributes, or run On-Demand Instance capacity.
module Network.AWS.EC2.ModifyInstanceCapacityReservationAttributes
  ( -- * Creating a request
    ModifyInstanceCapacityReservationAttributes (..),
    mkModifyInstanceCapacityReservationAttributes,

    -- ** Request lenses
    micraDryRun,
    micraInstanceId,
    micraCapacityReservationSpecification,

    -- * Destructuring the response
    ModifyInstanceCapacityReservationAttributesResponse (..),
    mkModifyInstanceCapacityReservationAttributesResponse,

    -- ** Response lenses
    micrarsReturn,
    micrarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyInstanceCapacityReservationAttributes' smart constructor.
data ModifyInstanceCapacityReservationAttributes = ModifyInstanceCapacityReservationAttributes'
  { dryRun ::
      Lude.Maybe
        Lude.Bool,
    instanceId ::
      Lude.Text,
    capacityReservationSpecification ::
      CapacityReservationSpecification
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyInstanceCapacityReservationAttributes' with the minimum fields required to make a request.
--
-- * 'capacityReservationSpecification' - Information about the Capacity Reservation targeting option.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'instanceId' - The ID of the instance to be modified.
mkModifyInstanceCapacityReservationAttributes ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'capacityReservationSpecification'
  CapacityReservationSpecification ->
  ModifyInstanceCapacityReservationAttributes
mkModifyInstanceCapacityReservationAttributes
  pInstanceId_
  pCapacityReservationSpecification_ =
    ModifyInstanceCapacityReservationAttributes'
      { dryRun =
          Lude.Nothing,
        instanceId = pInstanceId_,
        capacityReservationSpecification =
          pCapacityReservationSpecification_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micraDryRun :: Lens.Lens' ModifyInstanceCapacityReservationAttributes (Lude.Maybe Lude.Bool)
micraDryRun = Lens.lens (dryRun :: ModifyInstanceCapacityReservationAttributes -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyInstanceCapacityReservationAttributes)
{-# DEPRECATED micraDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the instance to be modified.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micraInstanceId :: Lens.Lens' ModifyInstanceCapacityReservationAttributes Lude.Text
micraInstanceId = Lens.lens (instanceId :: ModifyInstanceCapacityReservationAttributes -> Lude.Text) (\s a -> s {instanceId = a} :: ModifyInstanceCapacityReservationAttributes)
{-# DEPRECATED micraInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Information about the Capacity Reservation targeting option.
--
-- /Note:/ Consider using 'capacityReservationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micraCapacityReservationSpecification :: Lens.Lens' ModifyInstanceCapacityReservationAttributes CapacityReservationSpecification
micraCapacityReservationSpecification = Lens.lens (capacityReservationSpecification :: ModifyInstanceCapacityReservationAttributes -> CapacityReservationSpecification) (\s a -> s {capacityReservationSpecification = a} :: ModifyInstanceCapacityReservationAttributes)
{-# DEPRECATED micraCapacityReservationSpecification "Use generic-lens or generic-optics with 'capacityReservationSpecification' instead." #-}

instance
  Lude.AWSRequest
    ModifyInstanceCapacityReservationAttributes
  where
  type
    Rs ModifyInstanceCapacityReservationAttributes =
      ModifyInstanceCapacityReservationAttributesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyInstanceCapacityReservationAttributesResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyInstanceCapacityReservationAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyInstanceCapacityReservationAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyInstanceCapacityReservationAttributes where
  toQuery ModifyInstanceCapacityReservationAttributes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyInstanceCapacityReservationAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "InstanceId" Lude.=: instanceId,
        "CapacityReservationSpecification"
          Lude.=: capacityReservationSpecification
      ]

-- | /See:/ 'mkModifyInstanceCapacityReservationAttributesResponse' smart constructor.
data ModifyInstanceCapacityReservationAttributesResponse = ModifyInstanceCapacityReservationAttributesResponse'
  { return ::
      Lude.Maybe
        Lude.Bool,
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'ModifyInstanceCapacityReservationAttributesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
mkModifyInstanceCapacityReservationAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyInstanceCapacityReservationAttributesResponse
mkModifyInstanceCapacityReservationAttributesResponse
  pResponseStatus_ =
    ModifyInstanceCapacityReservationAttributesResponse'
      { return =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micrarsReturn :: Lens.Lens' ModifyInstanceCapacityReservationAttributesResponse (Lude.Maybe Lude.Bool)
micrarsReturn = Lens.lens (return :: ModifyInstanceCapacityReservationAttributesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: ModifyInstanceCapacityReservationAttributesResponse)
{-# DEPRECATED micrarsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micrarsResponseStatus :: Lens.Lens' ModifyInstanceCapacityReservationAttributesResponse Lude.Int
micrarsResponseStatus = Lens.lens (responseStatus :: ModifyInstanceCapacityReservationAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyInstanceCapacityReservationAttributesResponse)
{-# DEPRECATED micrarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
