{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RequestSpotFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Spot Fleet request.
--
-- The Spot Fleet request specifies the total target capacity and the On-Demand target capacity. Amazon EC2 calculates the difference between the total capacity and On-Demand capacity, and launches the difference as Spot capacity.
-- You can submit a single request that includes multiple launch specifications that vary by instance type, AMI, Availability Zone, or subnet.
-- By default, the Spot Fleet requests Spot Instances in the Spot Instance pool where the price per unit is the lowest. Each launch specification can include its own instance weighting that reflects the value of the instance type to your application workload.
-- Alternatively, you can specify that the Spot Fleet distribute the target capacity across the Spot pools included in its launch specifications. By ensuring that the Spot Instances in your Spot Fleet are in different Spot pools, you can improve the availability of your fleet.
-- You can specify tags for the Spot Fleet request and instances launched by the fleet. You cannot tag other resource types in a Spot Fleet request because only the @spot-fleet-request@ and @instance@ resource types are supported.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-requests.html Spot Fleet requests> in the /Amazon EC2 User Guide for Linux Instances/ .
module Network.AWS.EC2.RequestSpotFleet
  ( -- * Creating a request
    RequestSpotFleet (..),
    mkRequestSpotFleet,

    -- ** Request lenses
    rsfDryRun,
    rsfSpotFleetRequestConfig,

    -- * Destructuring the response
    RequestSpotFleetResponse (..),
    mkRequestSpotFleetResponse,

    -- ** Response lenses
    rsfrsSpotFleetRequestId,
    rsfrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for RequestSpotFleet.
--
-- /See:/ 'mkRequestSpotFleet' smart constructor.
data RequestSpotFleet = RequestSpotFleet'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    spotFleetRequestConfig :: SpotFleetRequestConfigData
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RequestSpotFleet' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'spotFleetRequestConfig' - The configuration for the Spot Fleet request.
mkRequestSpotFleet ::
  -- | 'spotFleetRequestConfig'
  SpotFleetRequestConfigData ->
  RequestSpotFleet
mkRequestSpotFleet pSpotFleetRequestConfig_ =
  RequestSpotFleet'
    { dryRun = Lude.Nothing,
      spotFleetRequestConfig = pSpotFleetRequestConfig_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsfDryRun :: Lens.Lens' RequestSpotFleet (Lude.Maybe Lude.Bool)
rsfDryRun = Lens.lens (dryRun :: RequestSpotFleet -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: RequestSpotFleet)
{-# DEPRECATED rsfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The configuration for the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsfSpotFleetRequestConfig :: Lens.Lens' RequestSpotFleet SpotFleetRequestConfigData
rsfSpotFleetRequestConfig = Lens.lens (spotFleetRequestConfig :: RequestSpotFleet -> SpotFleetRequestConfigData) (\s a -> s {spotFleetRequestConfig = a} :: RequestSpotFleet)
{-# DEPRECATED rsfSpotFleetRequestConfig "Use generic-lens or generic-optics with 'spotFleetRequestConfig' instead." #-}

instance Lude.AWSRequest RequestSpotFleet where
  type Rs RequestSpotFleet = RequestSpotFleetResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          RequestSpotFleetResponse'
            Lude.<$> (x Lude..@? "spotFleetRequestId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RequestSpotFleet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RequestSpotFleet where
  toPath = Lude.const "/"

instance Lude.ToQuery RequestSpotFleet where
  toQuery RequestSpotFleet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RequestSpotFleet" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "SpotFleetRequestConfig" Lude.=: spotFleetRequestConfig
      ]

-- | Contains the output of RequestSpotFleet.
--
-- /See:/ 'mkRequestSpotFleetResponse' smart constructor.
data RequestSpotFleetResponse = RequestSpotFleetResponse'
  { spotFleetRequestId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'RequestSpotFleetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'spotFleetRequestId' - The ID of the Spot Fleet request.
mkRequestSpotFleetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RequestSpotFleetResponse
mkRequestSpotFleetResponse pResponseStatus_ =
  RequestSpotFleetResponse'
    { spotFleetRequestId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsfrsSpotFleetRequestId :: Lens.Lens' RequestSpotFleetResponse (Lude.Maybe Lude.Text)
rsfrsSpotFleetRequestId = Lens.lens (spotFleetRequestId :: RequestSpotFleetResponse -> Lude.Maybe Lude.Text) (\s a -> s {spotFleetRequestId = a} :: RequestSpotFleetResponse)
{-# DEPRECATED rsfrsSpotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsfrsResponseStatus :: Lens.Lens' RequestSpotFleetResponse Lude.Int
rsfrsResponseStatus = Lens.lens (responseStatus :: RequestSpotFleetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RequestSpotFleetResponse)
{-# DEPRECATED rsfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
