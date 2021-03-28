{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.StartDeviceSync
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a device and its account to the known default settings. This clears all information and settings set by previous users in the following ways:
--
--
--     * Bluetooth - This unpairs all bluetooth devices paired with your echo device.
--
--
--     * Volume - This resets the echo device's volume to the default value.
--
--
--     * Notifications - This clears all notifications from your echo device.
--
--
--     * Lists - This clears all to-do items from your echo device.
--
--
--     * Settings - This internally syncs the room's profile (if the device is assigned to a room), contacts, address books, delegation access for account linking, and communications (if enabled on the room profile).
--
--
module Network.AWS.AlexaBusiness.StartDeviceSync
    (
    -- * Creating a request
      StartDeviceSync (..)
    , mkStartDeviceSync
    -- ** Request lenses
    , sdsFeatures
    , sdsDeviceArn
    , sdsRoomArn

    -- * Destructuring the response
    , StartDeviceSyncResponse (..)
    , mkStartDeviceSyncResponse
    -- ** Response lenses
    , sdsrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartDeviceSync' smart constructor.
data StartDeviceSync = StartDeviceSync'
  { features :: [Types.Feature]
    -- ^ Request structure to start the device sync. Required.
  , deviceArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the device to sync. Required.
  , roomArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the room with which the device to sync is associated. Required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartDeviceSync' value with any optional fields omitted.
mkStartDeviceSync
    :: StartDeviceSync
mkStartDeviceSync
  = StartDeviceSync'{features = Core.mempty,
                     deviceArn = Core.Nothing, roomArn = Core.Nothing}

-- | Request structure to start the device sync. Required.
--
-- /Note:/ Consider using 'features' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsFeatures :: Lens.Lens' StartDeviceSync [Types.Feature]
sdsFeatures = Lens.field @"features"
{-# INLINEABLE sdsFeatures #-}
{-# DEPRECATED features "Use generic-lens or generic-optics with 'features' instead"  #-}

-- | The ARN of the device to sync. Required.
--
-- /Note:/ Consider using 'deviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsDeviceArn :: Lens.Lens' StartDeviceSync (Core.Maybe Types.Arn)
sdsDeviceArn = Lens.field @"deviceArn"
{-# INLINEABLE sdsDeviceArn #-}
{-# DEPRECATED deviceArn "Use generic-lens or generic-optics with 'deviceArn' instead"  #-}

-- | The ARN of the room with which the device to sync is associated. Required.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsRoomArn :: Lens.Lens' StartDeviceSync (Core.Maybe Types.Arn)
sdsRoomArn = Lens.field @"roomArn"
{-# INLINEABLE sdsRoomArn #-}
{-# DEPRECATED roomArn "Use generic-lens or generic-optics with 'roomArn' instead"  #-}

instance Core.ToQuery StartDeviceSync where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartDeviceSync where
        toHeaders StartDeviceSync{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.StartDeviceSync")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartDeviceSync where
        toJSON StartDeviceSync{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Features" Core..= features),
                  ("DeviceArn" Core..=) Core.<$> deviceArn,
                  ("RoomArn" Core..=) Core.<$> roomArn])

instance Core.AWSRequest StartDeviceSync where
        type Rs StartDeviceSync = StartDeviceSyncResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StartDeviceSyncResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartDeviceSyncResponse' smart constructor.
newtype StartDeviceSyncResponse = StartDeviceSyncResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartDeviceSyncResponse' value with any optional fields omitted.
mkStartDeviceSyncResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartDeviceSyncResponse
mkStartDeviceSyncResponse responseStatus
  = StartDeviceSyncResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsrrsResponseStatus :: Lens.Lens' StartDeviceSyncResponse Core.Int
sdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
