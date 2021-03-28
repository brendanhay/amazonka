{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a profile that can be applied to one or more private fleet device instances.
module Network.AWS.DeviceFarm.CreateInstanceProfile
    (
    -- * Creating a request
      CreateInstanceProfile (..)
    , mkCreateInstanceProfile
    -- ** Request lenses
    , cipName
    , cipDescription
    , cipExcludeAppPackagesFromCleanup
    , cipPackageCleanup
    , cipRebootAfterUse

    -- * Destructuring the response
    , CreateInstanceProfileResponse (..)
    , mkCreateInstanceProfileResponse
    -- ** Response lenses
    , ciprrsInstanceProfile
    , ciprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateInstanceProfile' smart constructor.
data CreateInstanceProfile = CreateInstanceProfile'
  { name :: Types.Name
    -- ^ The name of your instance profile.
  , description :: Core.Maybe Types.Description
    -- ^ The description of your instance profile.
  , excludeAppPackagesFromCleanup :: Core.Maybe [Core.Text]
    -- ^ An array of strings that specifies the list of app packages that should not be cleaned up from the device after a test run.
--
-- The list of packages is considered only if you set @packageCleanup@ to @true@ .
  , packageCleanup :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , Device Farm removes app packages after a test run. The default value is @false@ for private devices.
  , rebootAfterUse :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , Device Farm reboots the instance after a test run. The default value is @true@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInstanceProfile' value with any optional fields omitted.
mkCreateInstanceProfile
    :: Types.Name -- ^ 'name'
    -> CreateInstanceProfile
mkCreateInstanceProfile name
  = CreateInstanceProfile'{name, description = Core.Nothing,
                           excludeAppPackagesFromCleanup = Core.Nothing,
                           packageCleanup = Core.Nothing, rebootAfterUse = Core.Nothing}

-- | The name of your instance profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipName :: Lens.Lens' CreateInstanceProfile Types.Name
cipName = Lens.field @"name"
{-# INLINEABLE cipName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The description of your instance profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipDescription :: Lens.Lens' CreateInstanceProfile (Core.Maybe Types.Description)
cipDescription = Lens.field @"description"
{-# INLINEABLE cipDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | An array of strings that specifies the list of app packages that should not be cleaned up from the device after a test run.
--
-- The list of packages is considered only if you set @packageCleanup@ to @true@ .
--
-- /Note:/ Consider using 'excludeAppPackagesFromCleanup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipExcludeAppPackagesFromCleanup :: Lens.Lens' CreateInstanceProfile (Core.Maybe [Core.Text])
cipExcludeAppPackagesFromCleanup = Lens.field @"excludeAppPackagesFromCleanup"
{-# INLINEABLE cipExcludeAppPackagesFromCleanup #-}
{-# DEPRECATED excludeAppPackagesFromCleanup "Use generic-lens or generic-optics with 'excludeAppPackagesFromCleanup' instead"  #-}

-- | When set to @true@ , Device Farm removes app packages after a test run. The default value is @false@ for private devices.
--
-- /Note:/ Consider using 'packageCleanup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipPackageCleanup :: Lens.Lens' CreateInstanceProfile (Core.Maybe Core.Bool)
cipPackageCleanup = Lens.field @"packageCleanup"
{-# INLINEABLE cipPackageCleanup #-}
{-# DEPRECATED packageCleanup "Use generic-lens or generic-optics with 'packageCleanup' instead"  #-}

-- | When set to @true@ , Device Farm reboots the instance after a test run. The default value is @true@ .
--
-- /Note:/ Consider using 'rebootAfterUse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipRebootAfterUse :: Lens.Lens' CreateInstanceProfile (Core.Maybe Core.Bool)
cipRebootAfterUse = Lens.field @"rebootAfterUse"
{-# INLINEABLE cipRebootAfterUse #-}
{-# DEPRECATED rebootAfterUse "Use generic-lens or generic-optics with 'rebootAfterUse' instead"  #-}

instance Core.ToQuery CreateInstanceProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateInstanceProfile where
        toHeaders CreateInstanceProfile{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.CreateInstanceProfile")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateInstanceProfile where
        toJSON CreateInstanceProfile{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  ("description" Core..=) Core.<$> description,
                  ("excludeAppPackagesFromCleanup" Core..=) Core.<$>
                    excludeAppPackagesFromCleanup,
                  ("packageCleanup" Core..=) Core.<$> packageCleanup,
                  ("rebootAfterUse" Core..=) Core.<$> rebootAfterUse])

instance Core.AWSRequest CreateInstanceProfile where
        type Rs CreateInstanceProfile = CreateInstanceProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateInstanceProfileResponse' Core.<$>
                   (x Core..:? "instanceProfile") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateInstanceProfileResponse' smart constructor.
data CreateInstanceProfileResponse = CreateInstanceProfileResponse'
  { instanceProfile :: Core.Maybe Types.InstanceProfile
    -- ^ An object that contains information about your instance profile.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInstanceProfileResponse' value with any optional fields omitted.
mkCreateInstanceProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateInstanceProfileResponse
mkCreateInstanceProfileResponse responseStatus
  = CreateInstanceProfileResponse'{instanceProfile = Core.Nothing,
                                   responseStatus}

-- | An object that contains information about your instance profile.
--
-- /Note:/ Consider using 'instanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciprrsInstanceProfile :: Lens.Lens' CreateInstanceProfileResponse (Core.Maybe Types.InstanceProfile)
ciprrsInstanceProfile = Lens.field @"instanceProfile"
{-# INLINEABLE ciprrsInstanceProfile #-}
{-# DEPRECATED instanceProfile "Use generic-lens or generic-optics with 'instanceProfile' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciprrsResponseStatus :: Lens.Lens' CreateInstanceProfileResponse Core.Int
ciprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ciprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
