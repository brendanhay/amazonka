{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StartSmartHomeApplianceDiscovery (..)
    , mkStartSmartHomeApplianceDiscovery
    -- ** Request lenses
    , sshadRoomArn

    -- * Destructuring the response
    , StartSmartHomeApplianceDiscoveryResponse (..)
    , mkStartSmartHomeApplianceDiscoveryResponse
    -- ** Response lenses
    , sshadrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartSmartHomeApplianceDiscovery' smart constructor.
newtype StartSmartHomeApplianceDiscovery = StartSmartHomeApplianceDiscovery'
  { roomArn :: Types.Arn
    -- ^ The room where smart home appliance discovery was initiated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartSmartHomeApplianceDiscovery' value with any optional fields omitted.
mkStartSmartHomeApplianceDiscovery
    :: Types.Arn -- ^ 'roomArn'
    -> StartSmartHomeApplianceDiscovery
mkStartSmartHomeApplianceDiscovery roomArn
  = StartSmartHomeApplianceDiscovery'{roomArn}

-- | The room where smart home appliance discovery was initiated.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sshadRoomArn :: Lens.Lens' StartSmartHomeApplianceDiscovery Types.Arn
sshadRoomArn = Lens.field @"roomArn"
{-# INLINEABLE sshadRoomArn #-}
{-# DEPRECATED roomArn "Use generic-lens or generic-optics with 'roomArn' instead"  #-}

instance Core.ToQuery StartSmartHomeApplianceDiscovery where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartSmartHomeApplianceDiscovery where
        toHeaders StartSmartHomeApplianceDiscovery{..}
          = Core.pure
              ("X-Amz-Target",
               "AlexaForBusiness.StartSmartHomeApplianceDiscovery")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartSmartHomeApplianceDiscovery where
        toJSON StartSmartHomeApplianceDiscovery{..}
          = Core.object
              (Core.catMaybes [Core.Just ("RoomArn" Core..= roomArn)])

instance Core.AWSRequest StartSmartHomeApplianceDiscovery where
        type Rs StartSmartHomeApplianceDiscovery =
             StartSmartHomeApplianceDiscoveryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StartSmartHomeApplianceDiscoveryResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartSmartHomeApplianceDiscoveryResponse' smart constructor.
newtype StartSmartHomeApplianceDiscoveryResponse = StartSmartHomeApplianceDiscoveryResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartSmartHomeApplianceDiscoveryResponse' value with any optional fields omitted.
mkStartSmartHomeApplianceDiscoveryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartSmartHomeApplianceDiscoveryResponse
mkStartSmartHomeApplianceDiscoveryResponse responseStatus
  = StartSmartHomeApplianceDiscoveryResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sshadrrsResponseStatus :: Lens.Lens' StartSmartHomeApplianceDiscoveryResponse Core.Int
sshadrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sshadrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
