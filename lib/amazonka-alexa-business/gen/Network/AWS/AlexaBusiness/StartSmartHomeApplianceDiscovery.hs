{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.StartSmartHomeApplianceDiscovery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the discovery of any smart home appliances associated with the room.
module Network.AWS.AlexaBusiness.StartSmartHomeApplianceDiscovery
  ( -- * Creating a request
    StartSmartHomeApplianceDiscovery (..),
    mkStartSmartHomeApplianceDiscovery,

    -- ** Request lenses
    sshadRoomArn,

    -- * Destructuring the response
    StartSmartHomeApplianceDiscoveryResponse (..),
    mkStartSmartHomeApplianceDiscoveryResponse,

    -- ** Response lenses
    sshadrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartSmartHomeApplianceDiscovery' smart constructor.
newtype StartSmartHomeApplianceDiscovery = StartSmartHomeApplianceDiscovery'
  { -- | The room where smart home appliance discovery was initiated.
    roomArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartSmartHomeApplianceDiscovery' value with any optional fields omitted.
mkStartSmartHomeApplianceDiscovery ::
  -- | 'roomArn'
  Types.Arn ->
  StartSmartHomeApplianceDiscovery
mkStartSmartHomeApplianceDiscovery roomArn =
  StartSmartHomeApplianceDiscovery' {roomArn}

-- | The room where smart home appliance discovery was initiated.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sshadRoomArn :: Lens.Lens' StartSmartHomeApplianceDiscovery Types.Arn
sshadRoomArn = Lens.field @"roomArn"
{-# DEPRECATED sshadRoomArn "Use generic-lens or generic-optics with 'roomArn' instead." #-}

instance Core.FromJSON StartSmartHomeApplianceDiscovery where
  toJSON StartSmartHomeApplianceDiscovery {..} =
    Core.object
      (Core.catMaybes [Core.Just ("RoomArn" Core..= roomArn)])

instance Core.AWSRequest StartSmartHomeApplianceDiscovery where
  type
    Rs StartSmartHomeApplianceDiscovery =
      StartSmartHomeApplianceDiscoveryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AlexaForBusiness.StartSmartHomeApplianceDiscovery"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartSmartHomeApplianceDiscoveryResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartSmartHomeApplianceDiscoveryResponse' smart constructor.
newtype StartSmartHomeApplianceDiscoveryResponse = StartSmartHomeApplianceDiscoveryResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartSmartHomeApplianceDiscoveryResponse' value with any optional fields omitted.
mkStartSmartHomeApplianceDiscoveryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartSmartHomeApplianceDiscoveryResponse
mkStartSmartHomeApplianceDiscoveryResponse responseStatus =
  StartSmartHomeApplianceDiscoveryResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sshadrrsResponseStatus :: Lens.Lens' StartSmartHomeApplianceDiscoveryResponse Core.Int
sshadrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sshadrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
