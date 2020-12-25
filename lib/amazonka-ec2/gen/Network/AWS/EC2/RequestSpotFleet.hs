{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    rsfSpotFleetRequestConfig,
    rsfDryRun,

    -- * Destructuring the response
    RequestSpotFleetResponse (..),
    mkRequestSpotFleetResponse,

    -- ** Response lenses
    rsfrrsSpotFleetRequestId,
    rsfrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for RequestSpotFleet.
--
-- /See:/ 'mkRequestSpotFleet' smart constructor.
data RequestSpotFleet = RequestSpotFleet'
  { -- | The configuration for the Spot Fleet request.
    spotFleetRequestConfig :: Types.SpotFleetRequestConfigData,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RequestSpotFleet' value with any optional fields omitted.
mkRequestSpotFleet ::
  -- | 'spotFleetRequestConfig'
  Types.SpotFleetRequestConfigData ->
  RequestSpotFleet
mkRequestSpotFleet spotFleetRequestConfig =
  RequestSpotFleet' {spotFleetRequestConfig, dryRun = Core.Nothing}

-- | The configuration for the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsfSpotFleetRequestConfig :: Lens.Lens' RequestSpotFleet Types.SpotFleetRequestConfigData
rsfSpotFleetRequestConfig = Lens.field @"spotFleetRequestConfig"
{-# DEPRECATED rsfSpotFleetRequestConfig "Use generic-lens or generic-optics with 'spotFleetRequestConfig' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsfDryRun :: Lens.Lens' RequestSpotFleet (Core.Maybe Core.Bool)
rsfDryRun = Lens.field @"dryRun"
{-# DEPRECATED rsfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest RequestSpotFleet where
  type Rs RequestSpotFleet = RequestSpotFleetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "RequestSpotFleet")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "SpotFleetRequestConfig" spotFleetRequestConfig)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          RequestSpotFleetResponse'
            Core.<$> (x Core..@? "spotFleetRequestId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of RequestSpotFleet.
--
-- /See:/ 'mkRequestSpotFleetResponse' smart constructor.
data RequestSpotFleetResponse = RequestSpotFleetResponse'
  { -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RequestSpotFleetResponse' value with any optional fields omitted.
mkRequestSpotFleetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RequestSpotFleetResponse
mkRequestSpotFleetResponse responseStatus =
  RequestSpotFleetResponse'
    { spotFleetRequestId = Core.Nothing,
      responseStatus
    }

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsfrrsSpotFleetRequestId :: Lens.Lens' RequestSpotFleetResponse (Core.Maybe Types.String)
rsfrrsSpotFleetRequestId = Lens.field @"spotFleetRequestId"
{-# DEPRECATED rsfrrsSpotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsfrrsResponseStatus :: Lens.Lens' RequestSpotFleetResponse Core.Int
rsfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rsfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
