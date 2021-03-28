{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.AssociateDeviceWithRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a device with a given room. This applies all the settings from the room profile to the device, and all the skills in any skill groups added to that room. This operation requires the device to be online, or else a manual sync is required. 
module Network.AWS.AlexaBusiness.AssociateDeviceWithRoom
    (
    -- * Creating a request
      AssociateDeviceWithRoom (..)
    , mkAssociateDeviceWithRoom
    -- ** Request lenses
    , adwrDeviceArn
    , adwrRoomArn

    -- * Destructuring the response
    , AssociateDeviceWithRoomResponse (..)
    , mkAssociateDeviceWithRoomResponse
    -- ** Response lenses
    , adwrrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateDeviceWithRoom' smart constructor.
data AssociateDeviceWithRoom = AssociateDeviceWithRoom'
  { deviceArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the device to associate to a room. Required.
  , roomArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the room with which to associate the device. Required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateDeviceWithRoom' value with any optional fields omitted.
mkAssociateDeviceWithRoom
    :: AssociateDeviceWithRoom
mkAssociateDeviceWithRoom
  = AssociateDeviceWithRoom'{deviceArn = Core.Nothing,
                             roomArn = Core.Nothing}

-- | The ARN of the device to associate to a room. Required.
--
-- /Note:/ Consider using 'deviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adwrDeviceArn :: Lens.Lens' AssociateDeviceWithRoom (Core.Maybe Types.Arn)
adwrDeviceArn = Lens.field @"deviceArn"
{-# INLINEABLE adwrDeviceArn #-}
{-# DEPRECATED deviceArn "Use generic-lens or generic-optics with 'deviceArn' instead"  #-}

-- | The ARN of the room with which to associate the device. Required.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adwrRoomArn :: Lens.Lens' AssociateDeviceWithRoom (Core.Maybe Types.Arn)
adwrRoomArn = Lens.field @"roomArn"
{-# INLINEABLE adwrRoomArn #-}
{-# DEPRECATED roomArn "Use generic-lens or generic-optics with 'roomArn' instead"  #-}

instance Core.ToQuery AssociateDeviceWithRoom where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateDeviceWithRoom where
        toHeaders AssociateDeviceWithRoom{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.AssociateDeviceWithRoom")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateDeviceWithRoom where
        toJSON AssociateDeviceWithRoom{..}
          = Core.object
              (Core.catMaybes
                 [("DeviceArn" Core..=) Core.<$> deviceArn,
                  ("RoomArn" Core..=) Core.<$> roomArn])

instance Core.AWSRequest AssociateDeviceWithRoom where
        type Rs AssociateDeviceWithRoom = AssociateDeviceWithRoomResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AssociateDeviceWithRoomResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateDeviceWithRoomResponse' smart constructor.
newtype AssociateDeviceWithRoomResponse = AssociateDeviceWithRoomResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateDeviceWithRoomResponse' value with any optional fields omitted.
mkAssociateDeviceWithRoomResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateDeviceWithRoomResponse
mkAssociateDeviceWithRoomResponse responseStatus
  = AssociateDeviceWithRoomResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adwrrrsResponseStatus :: Lens.Lens' AssociateDeviceWithRoomResponse Core.Int
adwrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE adwrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
