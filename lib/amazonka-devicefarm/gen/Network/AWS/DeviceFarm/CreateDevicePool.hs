{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateDevicePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a device pool.
module Network.AWS.DeviceFarm.CreateDevicePool
    (
    -- * Creating a request
      CreateDevicePool (..)
    , mkCreateDevicePool
    -- ** Request lenses
    , cdpProjectArn
    , cdpName
    , cdpRules
    , cdpDescription
    , cdpMaxDevices

    -- * Destructuring the response
    , CreateDevicePoolResponse (..)
    , mkCreateDevicePoolResponse
    -- ** Response lenses
    , cdprrsDevicePool
    , cdprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the create device pool operation.
--
-- /See:/ 'mkCreateDevicePool' smart constructor.
data CreateDevicePool = CreateDevicePool'
  { projectArn :: Types.AmazonResourceName
    -- ^ The ARN of the project for the device pool.
  , name :: Types.Name
    -- ^ The device pool's name.
  , rules :: [Types.Rule]
    -- ^ The device pool's rules.
  , description :: Core.Maybe Types.Message
    -- ^ The device pool's description.
  , maxDevices :: Core.Maybe Core.Int
    -- ^ The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs that you incur by running tests.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDevicePool' value with any optional fields omitted.
mkCreateDevicePool
    :: Types.AmazonResourceName -- ^ 'projectArn'
    -> Types.Name -- ^ 'name'
    -> CreateDevicePool
mkCreateDevicePool projectArn name
  = CreateDevicePool'{projectArn, name, rules = Core.mempty,
                      description = Core.Nothing, maxDevices = Core.Nothing}

-- | The ARN of the project for the device pool.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpProjectArn :: Lens.Lens' CreateDevicePool Types.AmazonResourceName
cdpProjectArn = Lens.field @"projectArn"
{-# INLINEABLE cdpProjectArn #-}
{-# DEPRECATED projectArn "Use generic-lens or generic-optics with 'projectArn' instead"  #-}

-- | The device pool's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpName :: Lens.Lens' CreateDevicePool Types.Name
cdpName = Lens.field @"name"
{-# INLINEABLE cdpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The device pool's rules.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpRules :: Lens.Lens' CreateDevicePool [Types.Rule]
cdpRules = Lens.field @"rules"
{-# INLINEABLE cdpRules #-}
{-# DEPRECATED rules "Use generic-lens or generic-optics with 'rules' instead"  #-}

-- | The device pool's description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpDescription :: Lens.Lens' CreateDevicePool (Core.Maybe Types.Message)
cdpDescription = Lens.field @"description"
{-# INLINEABLE cdpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs that you incur by running tests.
--
-- /Note:/ Consider using 'maxDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdpMaxDevices :: Lens.Lens' CreateDevicePool (Core.Maybe Core.Int)
cdpMaxDevices = Lens.field @"maxDevices"
{-# INLINEABLE cdpMaxDevices #-}
{-# DEPRECATED maxDevices "Use generic-lens or generic-optics with 'maxDevices' instead"  #-}

instance Core.ToQuery CreateDevicePool where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDevicePool where
        toHeaders CreateDevicePool{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.CreateDevicePool")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDevicePool where
        toJSON CreateDevicePool{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("projectArn" Core..= projectArn),
                  Core.Just ("name" Core..= name), Core.Just ("rules" Core..= rules),
                  ("description" Core..=) Core.<$> description,
                  ("maxDevices" Core..=) Core.<$> maxDevices])

instance Core.AWSRequest CreateDevicePool where
        type Rs CreateDevicePool = CreateDevicePoolResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDevicePoolResponse' Core.<$>
                   (x Core..:? "devicePool") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the result of a create device pool request.
--
-- /See:/ 'mkCreateDevicePoolResponse' smart constructor.
data CreateDevicePoolResponse = CreateDevicePoolResponse'
  { devicePool :: Core.Maybe Types.DevicePool
    -- ^ The newly created device pool.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDevicePoolResponse' value with any optional fields omitted.
mkCreateDevicePoolResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDevicePoolResponse
mkCreateDevicePoolResponse responseStatus
  = CreateDevicePoolResponse'{devicePool = Core.Nothing,
                              responseStatus}

-- | The newly created device pool.
--
-- /Note:/ Consider using 'devicePool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdprrsDevicePool :: Lens.Lens' CreateDevicePoolResponse (Core.Maybe Types.DevicePool)
cdprrsDevicePool = Lens.field @"devicePool"
{-# INLINEABLE cdprrsDevicePool #-}
{-# DEPRECATED devicePool "Use generic-lens or generic-optics with 'devicePool' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdprrsResponseStatus :: Lens.Lens' CreateDevicePoolResponse Core.Int
cdprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
