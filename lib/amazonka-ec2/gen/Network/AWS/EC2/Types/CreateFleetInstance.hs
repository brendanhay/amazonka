{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreateFleetInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CreateFleetInstance
  ( CreateFleetInstance (..)
  -- * Smart constructor
  , mkCreateFleetInstance
  -- * Lenses
  , cfiInstanceIds
  , cfiInstanceType
  , cfiLaunchTemplateAndOverrides
  , cfiLifecycle
  , cfiPlatform
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
-- /See:/ 'mkCreateFleetInstance' smart constructor.
data CreateFleetInstance = CreateFleetInstance'
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

-- | Creates a 'CreateFleetInstance' value with any optional fields omitted.
mkCreateFleetInstance
    :: CreateFleetInstance
mkCreateFleetInstance
  = CreateFleetInstance'{instanceIds = Core.Nothing,
                         instanceType = Core.Nothing,
                         launchTemplateAndOverrides = Core.Nothing,
                         lifecycle = Core.Nothing, platform = Core.Nothing}

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfiInstanceIds :: Lens.Lens' CreateFleetInstance (Core.Maybe [Types.InstanceId])
cfiInstanceIds = Lens.field @"instanceIds"
{-# INLINEABLE cfiInstanceIds #-}
{-# DEPRECATED instanceIds "Use generic-lens or generic-optics with 'instanceIds' instead"  #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfiInstanceType :: Lens.Lens' CreateFleetInstance (Core.Maybe Types.InstanceType)
cfiInstanceType = Lens.field @"instanceType"
{-# INLINEABLE cfiInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
--
-- /Note:/ Consider using 'launchTemplateAndOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfiLaunchTemplateAndOverrides :: Lens.Lens' CreateFleetInstance (Core.Maybe Types.LaunchTemplateAndOverridesResponse)
cfiLaunchTemplateAndOverrides = Lens.field @"launchTemplateAndOverrides"
{-# INLINEABLE cfiLaunchTemplateAndOverrides #-}
{-# DEPRECATED launchTemplateAndOverrides "Use generic-lens or generic-optics with 'launchTemplateAndOverrides' instead"  #-}

-- | Indicates if the instance that was launched is a Spot Instance or On-Demand Instance.
--
-- /Note:/ Consider using 'lifecycle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfiLifecycle :: Lens.Lens' CreateFleetInstance (Core.Maybe Types.InstanceLifecycle)
cfiLifecycle = Lens.field @"lifecycle"
{-# INLINEABLE cfiLifecycle #-}
{-# DEPRECATED lifecycle "Use generic-lens or generic-optics with 'lifecycle' instead"  #-}

-- | The value is @Windows@ for Windows instances. Otherwise, the value is blank.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfiPlatform :: Lens.Lens' CreateFleetInstance (Core.Maybe Types.PlatformValues)
cfiPlatform = Lens.field @"platform"
{-# INLINEABLE cfiPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

instance Core.FromXML CreateFleetInstance where
        parseXML x
          = CreateFleetInstance' Core.<$>
              (x Core..@? "instanceIds" Core..<@> Core.parseXMLList "item")
                Core.<*> x Core..@? "instanceType"
                Core.<*> x Core..@? "launchTemplateAndOverrides"
                Core.<*> x Core..@? "lifecycle"
                Core.<*> x Core..@? "platform"
