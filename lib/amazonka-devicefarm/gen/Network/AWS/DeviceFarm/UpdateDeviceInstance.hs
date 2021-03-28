{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.UpdateDeviceInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about a private device instance.
module Network.AWS.DeviceFarm.UpdateDeviceInstance
    (
    -- * Creating a request
      UpdateDeviceInstance (..)
    , mkUpdateDeviceInstance
    -- ** Request lenses
    , udiArn
    , udiLabels
    , udiProfileArn

    -- * Destructuring the response
    , UpdateDeviceInstanceResponse (..)
    , mkUpdateDeviceInstanceResponse
    -- ** Response lenses
    , udirrsDeviceInstance
    , udirrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDeviceInstance' smart constructor.
data UpdateDeviceInstance = UpdateDeviceInstance'
  { arn :: Types.AmazonResourceName
    -- ^ The Amazon Resource Name (ARN) of the device instance.
  , labels :: Core.Maybe [Core.Text]
    -- ^ An array of strings that you want to associate with the device instance.
  , profileArn :: Core.Maybe Types.AmazonResourceName
    -- ^ The ARN of the profile that you want to associate with the device instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDeviceInstance' value with any optional fields omitted.
mkUpdateDeviceInstance
    :: Types.AmazonResourceName -- ^ 'arn'
    -> UpdateDeviceInstance
mkUpdateDeviceInstance arn
  = UpdateDeviceInstance'{arn, labels = Core.Nothing,
                          profileArn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the device instance.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udiArn :: Lens.Lens' UpdateDeviceInstance Types.AmazonResourceName
udiArn = Lens.field @"arn"
{-# INLINEABLE udiArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | An array of strings that you want to associate with the device instance.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udiLabels :: Lens.Lens' UpdateDeviceInstance (Core.Maybe [Core.Text])
udiLabels = Lens.field @"labels"
{-# INLINEABLE udiLabels #-}
{-# DEPRECATED labels "Use generic-lens or generic-optics with 'labels' instead"  #-}

-- | The ARN of the profile that you want to associate with the device instance.
--
-- /Note:/ Consider using 'profileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udiProfileArn :: Lens.Lens' UpdateDeviceInstance (Core.Maybe Types.AmazonResourceName)
udiProfileArn = Lens.field @"profileArn"
{-# INLINEABLE udiProfileArn #-}
{-# DEPRECATED profileArn "Use generic-lens or generic-optics with 'profileArn' instead"  #-}

instance Core.ToQuery UpdateDeviceInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDeviceInstance where
        toHeaders UpdateDeviceInstance{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.UpdateDeviceInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateDeviceInstance where
        toJSON UpdateDeviceInstance{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("arn" Core..= arn), ("labels" Core..=) Core.<$> labels,
                  ("profileArn" Core..=) Core.<$> profileArn])

instance Core.AWSRequest UpdateDeviceInstance where
        type Rs UpdateDeviceInstance = UpdateDeviceInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateDeviceInstanceResponse' Core.<$>
                   (x Core..:? "deviceInstance") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateDeviceInstanceResponse' smart constructor.
data UpdateDeviceInstanceResponse = UpdateDeviceInstanceResponse'
  { deviceInstance :: Core.Maybe Types.DeviceInstance
    -- ^ An object that contains information about your device instance.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDeviceInstanceResponse' value with any optional fields omitted.
mkUpdateDeviceInstanceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateDeviceInstanceResponse
mkUpdateDeviceInstanceResponse responseStatus
  = UpdateDeviceInstanceResponse'{deviceInstance = Core.Nothing,
                                  responseStatus}

-- | An object that contains information about your device instance.
--
-- /Note:/ Consider using 'deviceInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udirrsDeviceInstance :: Lens.Lens' UpdateDeviceInstanceResponse (Core.Maybe Types.DeviceInstance)
udirrsDeviceInstance = Lens.field @"deviceInstance"
{-# INLINEABLE udirrsDeviceInstance #-}
{-# DEPRECATED deviceInstance "Use generic-lens or generic-optics with 'deviceInstance' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udirrsResponseStatus :: Lens.Lens' UpdateDeviceInstanceResponse Core.Int
udirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE udirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
