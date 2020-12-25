{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.UpdateJobShipmentState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the state when a the shipment states changes to a different state.
module Network.AWS.Snowball.UpdateJobShipmentState
  ( -- * Creating a request
    UpdateJobShipmentState (..),
    mkUpdateJobShipmentState,

    -- ** Request lenses
    ujssJobId,
    ujssShipmentState,

    -- * Destructuring the response
    UpdateJobShipmentStateResponse (..),
    mkUpdateJobShipmentStateResponse,

    -- ** Response lenses
    ujssrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkUpdateJobShipmentState' smart constructor.
data UpdateJobShipmentState = UpdateJobShipmentState'
  { -- | The job ID of the job whose shipment date you want to update, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
    jobId :: Types.JobId,
    -- | The state of a device when it is being shipped.
    --
    -- Set to @RECEIVED@ when the device arrives at your location.
    -- Set to @RETURNED@ when you have returned the device to AWS.
    shipmentState :: Types.ShipmentState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJobShipmentState' value with any optional fields omitted.
mkUpdateJobShipmentState ::
  -- | 'jobId'
  Types.JobId ->
  -- | 'shipmentState'
  Types.ShipmentState ->
  UpdateJobShipmentState
mkUpdateJobShipmentState jobId shipmentState =
  UpdateJobShipmentState' {jobId, shipmentState}

-- | The job ID of the job whose shipment date you want to update, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujssJobId :: Lens.Lens' UpdateJobShipmentState Types.JobId
ujssJobId = Lens.field @"jobId"
{-# DEPRECATED ujssJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The state of a device when it is being shipped.
--
-- Set to @RECEIVED@ when the device arrives at your location.
-- Set to @RETURNED@ when you have returned the device to AWS.
--
-- /Note:/ Consider using 'shipmentState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujssShipmentState :: Lens.Lens' UpdateJobShipmentState Types.ShipmentState
ujssShipmentState = Lens.field @"shipmentState"
{-# DEPRECATED ujssShipmentState "Use generic-lens or generic-optics with 'shipmentState' instead." #-}

instance Core.FromJSON UpdateJobShipmentState where
  toJSON UpdateJobShipmentState {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobId" Core..= jobId),
            Core.Just ("ShipmentState" Core..= shipmentState)
          ]
      )

instance Core.AWSRequest UpdateJobShipmentState where
  type Rs UpdateJobShipmentState = UpdateJobShipmentStateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSIESnowballJobManagementService.UpdateJobShipmentState"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateJobShipmentStateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateJobShipmentStateResponse' smart constructor.
newtype UpdateJobShipmentStateResponse = UpdateJobShipmentStateResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJobShipmentStateResponse' value with any optional fields omitted.
mkUpdateJobShipmentStateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateJobShipmentStateResponse
mkUpdateJobShipmentStateResponse responseStatus =
  UpdateJobShipmentStateResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujssrrsResponseStatus :: Lens.Lens' UpdateJobShipmentStateResponse Core.Int
ujssrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ujssrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
