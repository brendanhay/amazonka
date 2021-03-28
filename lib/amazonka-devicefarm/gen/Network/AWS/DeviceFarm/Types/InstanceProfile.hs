{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.InstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.InstanceProfile
  ( InstanceProfile (..)
  -- * Smart constructor
  , mkInstanceProfile
  -- * Lenses
  , ipArn
  , ipDescription
  , ipExcludeAppPackagesFromCleanup
  , ipName
  , ipPackageCleanup
  , ipRebootAfterUse
  ) where

import qualified Network.AWS.DeviceFarm.Types.Arn as Types
import qualified Network.AWS.DeviceFarm.Types.Description as Types
import qualified Network.AWS.DeviceFarm.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the instance profile.
--
-- /See:/ 'mkInstanceProfile' smart constructor.
data InstanceProfile = InstanceProfile'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the instance profile.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the instance profile.
  , excludeAppPackagesFromCleanup :: Core.Maybe [Core.Text]
    -- ^ An array of strings containing the list of app packages that should not be cleaned up from the device after a test run completes.
--
-- The list of packages is considered only if you set @packageCleanup@ to @true@ .
  , name :: Core.Maybe Types.Name
    -- ^ The name of the instance profile.
  , packageCleanup :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , Device Farm removes app packages after a test run. The default value is @false@ for private devices.
  , rebootAfterUse :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , Device Farm reboots the instance after a test run. The default value is @true@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceProfile' value with any optional fields omitted.
mkInstanceProfile
    :: InstanceProfile
mkInstanceProfile
  = InstanceProfile'{arn = Core.Nothing, description = Core.Nothing,
                     excludeAppPackagesFromCleanup = Core.Nothing, name = Core.Nothing,
                     packageCleanup = Core.Nothing, rebootAfterUse = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the instance profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipArn :: Lens.Lens' InstanceProfile (Core.Maybe Types.Arn)
ipArn = Lens.field @"arn"
{-# INLINEABLE ipArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The description of the instance profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipDescription :: Lens.Lens' InstanceProfile (Core.Maybe Types.Description)
ipDescription = Lens.field @"description"
{-# INLINEABLE ipDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | An array of strings containing the list of app packages that should not be cleaned up from the device after a test run completes.
--
-- The list of packages is considered only if you set @packageCleanup@ to @true@ .
--
-- /Note:/ Consider using 'excludeAppPackagesFromCleanup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipExcludeAppPackagesFromCleanup :: Lens.Lens' InstanceProfile (Core.Maybe [Core.Text])
ipExcludeAppPackagesFromCleanup = Lens.field @"excludeAppPackagesFromCleanup"
{-# INLINEABLE ipExcludeAppPackagesFromCleanup #-}
{-# DEPRECATED excludeAppPackagesFromCleanup "Use generic-lens or generic-optics with 'excludeAppPackagesFromCleanup' instead"  #-}

-- | The name of the instance profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipName :: Lens.Lens' InstanceProfile (Core.Maybe Types.Name)
ipName = Lens.field @"name"
{-# INLINEABLE ipName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | When set to @true@ , Device Farm removes app packages after a test run. The default value is @false@ for private devices.
--
-- /Note:/ Consider using 'packageCleanup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipPackageCleanup :: Lens.Lens' InstanceProfile (Core.Maybe Core.Bool)
ipPackageCleanup = Lens.field @"packageCleanup"
{-# INLINEABLE ipPackageCleanup #-}
{-# DEPRECATED packageCleanup "Use generic-lens or generic-optics with 'packageCleanup' instead"  #-}

-- | When set to @true@ , Device Farm reboots the instance after a test run. The default value is @true@ .
--
-- /Note:/ Consider using 'rebootAfterUse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipRebootAfterUse :: Lens.Lens' InstanceProfile (Core.Maybe Core.Bool)
ipRebootAfterUse = Lens.field @"rebootAfterUse"
{-# INLINEABLE ipRebootAfterUse #-}
{-# DEPRECATED rebootAfterUse "Use generic-lens or generic-optics with 'rebootAfterUse' instead"  #-}

instance Core.FromJSON InstanceProfile where
        parseJSON
          = Core.withObject "InstanceProfile" Core.$
              \ x ->
                InstanceProfile' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "description" Core.<*>
                    x Core..:? "excludeAppPackagesFromCleanup"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "packageCleanup"
                    Core.<*> x Core..:? "rebootAfterUse"
