{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.MaintenanceWindowTarget
  ( MaintenanceWindowTarget (..)
  -- * Smart constructor
  , mkMaintenanceWindowTarget
  -- * Lenses
  , mDescription
  , mName
  , mOwnerInformation
  , mResourceType
  , mTargets
  , mWindowId
  , mWindowTargetId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.MaintenanceWindowDescription as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowId as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowName as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowResourceType as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowTargetId as Types
import qualified Network.AWS.SSM.Types.OwnerInformation as Types
import qualified Network.AWS.SSM.Types.Target as Types

-- | The target registered with the maintenance window.
--
-- /See:/ 'mkMaintenanceWindowTarget' smart constructor.
data MaintenanceWindowTarget = MaintenanceWindowTarget'
  { description :: Core.Maybe Types.MaintenanceWindowDescription
    -- ^ A description for the target.
  , name :: Core.Maybe Types.MaintenanceWindowName
    -- ^ The name for the maintenance window target.
  , ownerInformation :: Core.Maybe Types.OwnerInformation
    -- ^ A user-provided value that will be included in any CloudWatch events that are raised while running tasks for these targets in this maintenance window.
  , resourceType :: Core.Maybe Types.MaintenanceWindowResourceType
    -- ^ The type of target that is being registered with the maintenance window.
  , targets :: Core.Maybe [Types.Target]
    -- ^ The targets, either instances or tags.
--
-- Specify instances using the following format:
-- @Key=instanceids,Values=<instanceid1>,<instanceid2>@ 
-- Tags are specified using the following format:
-- @Key=<tag name>,Values=<tag value>@ .
  , windowId :: Core.Maybe Types.MaintenanceWindowId
    -- ^ The ID of the maintenance window to register the target with.
  , windowTargetId :: Core.Maybe Types.MaintenanceWindowTargetId
    -- ^ The ID of the target.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MaintenanceWindowTarget' value with any optional fields omitted.
mkMaintenanceWindowTarget
    :: MaintenanceWindowTarget
mkMaintenanceWindowTarget
  = MaintenanceWindowTarget'{description = Core.Nothing,
                             name = Core.Nothing, ownerInformation = Core.Nothing,
                             resourceType = Core.Nothing, targets = Core.Nothing,
                             windowId = Core.Nothing, windowTargetId = Core.Nothing}

-- | A description for the target.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDescription :: Lens.Lens' MaintenanceWindowTarget (Core.Maybe Types.MaintenanceWindowDescription)
mDescription = Lens.field @"description"
{-# INLINEABLE mDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name for the maintenance window target.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mName :: Lens.Lens' MaintenanceWindowTarget (Core.Maybe Types.MaintenanceWindowName)
mName = Lens.field @"name"
{-# INLINEABLE mName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A user-provided value that will be included in any CloudWatch events that are raised while running tasks for these targets in this maintenance window.
--
-- /Note:/ Consider using 'ownerInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mOwnerInformation :: Lens.Lens' MaintenanceWindowTarget (Core.Maybe Types.OwnerInformation)
mOwnerInformation = Lens.field @"ownerInformation"
{-# INLINEABLE mOwnerInformation #-}
{-# DEPRECATED ownerInformation "Use generic-lens or generic-optics with 'ownerInformation' instead"  #-}

-- | The type of target that is being registered with the maintenance window.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mResourceType :: Lens.Lens' MaintenanceWindowTarget (Core.Maybe Types.MaintenanceWindowResourceType)
mResourceType = Lens.field @"resourceType"
{-# INLINEABLE mResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The targets, either instances or tags.
--
-- Specify instances using the following format:
-- @Key=instanceids,Values=<instanceid1>,<instanceid2>@ 
-- Tags are specified using the following format:
-- @Key=<tag name>,Values=<tag value>@ .
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTargets :: Lens.Lens' MaintenanceWindowTarget (Core.Maybe [Types.Target])
mTargets = Lens.field @"targets"
{-# INLINEABLE mTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

-- | The ID of the maintenance window to register the target with.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mWindowId :: Lens.Lens' MaintenanceWindowTarget (Core.Maybe Types.MaintenanceWindowId)
mWindowId = Lens.field @"windowId"
{-# INLINEABLE mWindowId #-}
{-# DEPRECATED windowId "Use generic-lens or generic-optics with 'windowId' instead"  #-}

-- | The ID of the target.
--
-- /Note:/ Consider using 'windowTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mWindowTargetId :: Lens.Lens' MaintenanceWindowTarget (Core.Maybe Types.MaintenanceWindowTargetId)
mWindowTargetId = Lens.field @"windowTargetId"
{-# INLINEABLE mWindowTargetId #-}
{-# DEPRECATED windowTargetId "Use generic-lens or generic-optics with 'windowTargetId' instead"  #-}

instance Core.FromJSON MaintenanceWindowTarget where
        parseJSON
          = Core.withObject "MaintenanceWindowTarget" Core.$
              \ x ->
                MaintenanceWindowTarget' Core.<$>
                  (x Core..:? "Description") Core.<*> x Core..:? "Name" Core.<*>
                    x Core..:? "OwnerInformation"
                    Core.<*> x Core..:? "ResourceType"
                    Core.<*> x Core..:? "Targets"
                    Core.<*> x Core..:? "WindowId"
                    Core.<*> x Core..:? "WindowTargetId"
