{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingGroupMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.ThingGroupMetadata
  ( ThingGroupMetadata (..)
  -- * Smart constructor
  , mkThingGroupMetadata
  -- * Lenses
  , tgmCreationDate
  , tgmParentGroupName
  , tgmRootToParentThingGroups
  ) where

import qualified Network.AWS.IoT.Types.GroupNameAndArn as Types
import qualified Network.AWS.IoT.Types.ParentGroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Thing group metadata.
--
-- /See:/ 'mkThingGroupMetadata' smart constructor.
data ThingGroupMetadata = ThingGroupMetadata'
  { creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The UNIX timestamp of when the thing group was created.
  , parentGroupName :: Core.Maybe Types.ParentGroupName
    -- ^ The parent thing group name.
  , rootToParentThingGroups :: Core.Maybe [Types.GroupNameAndArn]
    -- ^ The root parent thing group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ThingGroupMetadata' value with any optional fields omitted.
mkThingGroupMetadata
    :: ThingGroupMetadata
mkThingGroupMetadata
  = ThingGroupMetadata'{creationDate = Core.Nothing,
                        parentGroupName = Core.Nothing,
                        rootToParentThingGroups = Core.Nothing}

-- | The UNIX timestamp of when the thing group was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmCreationDate :: Lens.Lens' ThingGroupMetadata (Core.Maybe Core.NominalDiffTime)
tgmCreationDate = Lens.field @"creationDate"
{-# INLINEABLE tgmCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The parent thing group name.
--
-- /Note:/ Consider using 'parentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmParentGroupName :: Lens.Lens' ThingGroupMetadata (Core.Maybe Types.ParentGroupName)
tgmParentGroupName = Lens.field @"parentGroupName"
{-# INLINEABLE tgmParentGroupName #-}
{-# DEPRECATED parentGroupName "Use generic-lens or generic-optics with 'parentGroupName' instead"  #-}

-- | The root parent thing group.
--
-- /Note:/ Consider using 'rootToParentThingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmRootToParentThingGroups :: Lens.Lens' ThingGroupMetadata (Core.Maybe [Types.GroupNameAndArn])
tgmRootToParentThingGroups = Lens.field @"rootToParentThingGroups"
{-# INLINEABLE tgmRootToParentThingGroups #-}
{-# DEPRECATED rootToParentThingGroups "Use generic-lens or generic-optics with 'rootToParentThingGroups' instead"  #-}

instance Core.FromJSON ThingGroupMetadata where
        parseJSON
          = Core.withObject "ThingGroupMetadata" Core.$
              \ x ->
                ThingGroupMetadata' Core.<$>
                  (x Core..:? "creationDate") Core.<*> x Core..:? "parentGroupName"
                    Core.<*> x Core..:? "rootToParentThingGroups"
