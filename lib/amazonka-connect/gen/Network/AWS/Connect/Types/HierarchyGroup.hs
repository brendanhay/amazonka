{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.HierarchyGroup
  ( HierarchyGroup (..)
  -- * Smart constructor
  , mkHierarchyGroup
  -- * Lenses
  , hgArn
  , hgHierarchyPath
  , hgId
  , hgLevelId
  , hgName
  ) where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Connect.Types.HierarchyPath as Types
import qualified Network.AWS.Connect.Types.Id as Types
import qualified Network.AWS.Connect.Types.LevelId as Types
import qualified Network.AWS.Connect.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a hierarchy group.
--
-- /See:/ 'mkHierarchyGroup' smart constructor.
data HierarchyGroup = HierarchyGroup'
  { arn :: Core.Maybe Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the hierarchy group.
  , hierarchyPath :: Core.Maybe Types.HierarchyPath
    -- ^ Information about the levels in the hierarchy group.
  , id :: Core.Maybe Types.Id
    -- ^ The identifier of the hierarchy group.
  , levelId :: Core.Maybe Types.LevelId
    -- ^ The identifier of the level in the hierarchy group.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the hierarchy group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HierarchyGroup' value with any optional fields omitted.
mkHierarchyGroup
    :: HierarchyGroup
mkHierarchyGroup
  = HierarchyGroup'{arn = Core.Nothing, hierarchyPath = Core.Nothing,
                    id = Core.Nothing, levelId = Core.Nothing, name = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the hierarchy group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgArn :: Lens.Lens' HierarchyGroup (Core.Maybe Types.ARN)
hgArn = Lens.field @"arn"
{-# INLINEABLE hgArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | Information about the levels in the hierarchy group.
--
-- /Note:/ Consider using 'hierarchyPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgHierarchyPath :: Lens.Lens' HierarchyGroup (Core.Maybe Types.HierarchyPath)
hgHierarchyPath = Lens.field @"hierarchyPath"
{-# INLINEABLE hgHierarchyPath #-}
{-# DEPRECATED hierarchyPath "Use generic-lens or generic-optics with 'hierarchyPath' instead"  #-}

-- | The identifier of the hierarchy group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgId :: Lens.Lens' HierarchyGroup (Core.Maybe Types.Id)
hgId = Lens.field @"id"
{-# INLINEABLE hgId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The identifier of the level in the hierarchy group.
--
-- /Note:/ Consider using 'levelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgLevelId :: Lens.Lens' HierarchyGroup (Core.Maybe Types.LevelId)
hgLevelId = Lens.field @"levelId"
{-# INLINEABLE hgLevelId #-}
{-# DEPRECATED levelId "Use generic-lens or generic-optics with 'levelId' instead"  #-}

-- | The name of the hierarchy group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgName :: Lens.Lens' HierarchyGroup (Core.Maybe Types.Name)
hgName = Lens.field @"name"
{-# INLINEABLE hgName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON HierarchyGroup where
        parseJSON
          = Core.withObject "HierarchyGroup" Core.$
              \ x ->
                HierarchyGroup' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "HierarchyPath" Core.<*>
                    x Core..:? "Id"
                    Core.<*> x Core..:? "LevelId"
                    Core.<*> x Core..:? "Name"
