{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DescribeFleetsInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DescribeFleetsInstances
  ( DescribeFleetsInstances (..)
  -- * Smart constructor
  , mkDescribeFleetsInstances
  -- * Lenses
  , dfiInstanceIds
  , dfiInstanceType
  , dfiLaunchTemplateAndOverrides
  , dfiLifecycle
  , dfiPlatform
  ) where

import qualified Network.AWS.EC2.Types.InstanceId as Types
import qualified Network.AWS.EC2.Types.InstanceLifecycle as Types
import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse as Types
import qualified Network.AWS.EC2.Types.PlatformValues as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the instances that were launched by the fleet.
--
-- /See:/ 'mkDescribeFleetsInstances' smart constructor.
data DescribeFleetsInstances = DescribeFleetsInstances'
  { instanceIds :: Core.Maybe [Types.InstanceId]
    -- ^ The IDs of the instances.
  , instanceType :: Core.Maybe Types.InstanceType
    -- ^ The instance type.
  , launchTemplateAndOverrides :: Core.Maybe Types.LaunchTemplateAndOverridesResponse
    -- ^ The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
  , lifecycle :: Core.Maybe Types.InstanceLifecycle
    -- ^ Indicates if the instance that was launched is a Spot Instance or On-Demand Instance.
  , platform :: Core.Maybe Types.PlatformValues
    -- ^ The value is @Windows@ for Windows instances. Otherwise, the value is blank.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeFleetsInstances' value with any optional fields omitted.
mkDescribeFleetsInstances
    :: DescribeFleetsInstances
mkDescribeFleetsInstances
  = DescribeFleetsInstances'{instanceIds = Core.Nothing,
                             instanceType = Core.Nothing,
                             launchTemplateAndOverrides = Core.Nothing,
                             lifecycle = Core.Nothing, platform = Core.Nothing}

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiInstanceIds :: Lens.Lens' DescribeFleetsInstances (Core.Maybe [Types.InstanceId])
dfiInstanceIds = Lens.field @"instanceIds"
{-# INLINEABLE dfiInstanceIds #-}
{-# DEPRECATED instanceIds "Use generic-lens or generic-optics with 'instanceIds' instead"  #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiInstanceType :: Lens.Lens' DescribeFleetsInstances (Core.Maybe Types.InstanceType)
dfiInstanceType = Lens.field @"instanceType"
{-# INLINEABLE dfiInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
--
-- /Note:/ Consider using 'launchTemplateAndOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiLaunchTemplateAndOverrides :: Lens.Lens' DescribeFleetsInstances (Core.Maybe Types.LaunchTemplateAndOverridesResponse)
dfiLaunchTemplateAndOverrides = Lens.field @"launchTemplateAndOverrides"
{-# INLINEABLE dfiLaunchTemplateAndOverrides #-}
{-# DEPRECATED launchTemplateAndOverrides "Use generic-lens or generic-optics with 'launchTemplateAndOverrides' instead"  #-}

-- | Indicates if the instance that was launched is a Spot Instance or On-Demand Instance.
--
-- /Note:/ Consider using 'lifecycle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiLifecycle :: Lens.Lens' DescribeFleetsInstances (Core.Maybe Types.InstanceLifecycle)
dfiLifecycle = Lens.field @"lifecycle"
{-# INLINEABLE dfiLifecycle #-}
{-# DEPRECATED lifecycle "Use generic-lens or generic-optics with 'lifecycle' instead"  #-}

-- | The value is @Windows@ for Windows instances. Otherwise, the value is blank.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiPlatform :: Lens.Lens' DescribeFleetsInstances (Core.Maybe Types.PlatformValues)
dfiPlatform = Lens.field @"platform"
{-# INLINEABLE dfiPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

instance Core.FromXML DescribeFleetsInstances where
        parseXML x
          = DescribeFleetsInstances' Core.<$>
              (x Core..@? "instanceIds" Core..<@> Core.parseXMLList "item")
                Core.<*> x Core..@? "instanceType"
                Core.<*> x Core..@? "launchTemplateAndOverrides"
                Core.<*> x Core..@? "lifecycle"
                Core.<*> x Core..@? "platform"
