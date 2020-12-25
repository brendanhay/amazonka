{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyStructureUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyStructureUpdate
  ( HierarchyStructureUpdate (..),

    -- * Smart constructor
    mkHierarchyStructureUpdate,

    -- * Lenses
    hsuLevelFive,
    hsuLevelFour,
    hsuLevelOne,
    hsuLevelThree,
    hsuLevelTwo,
  )
where

import qualified Network.AWS.Connect.Types.HierarchyLevelUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the level hierarchy to update.
--
-- /See:/ 'mkHierarchyStructureUpdate' smart constructor.
data HierarchyStructureUpdate = HierarchyStructureUpdate'
  { -- | The update for level five.
    levelFive :: Core.Maybe Types.HierarchyLevelUpdate,
    -- | The update for level four.
    levelFour :: Core.Maybe Types.HierarchyLevelUpdate,
    -- | The update for level one.
    levelOne :: Core.Maybe Types.HierarchyLevelUpdate,
    -- | The update for level three.
    levelThree :: Core.Maybe Types.HierarchyLevelUpdate,
    -- | The update for level two.
    levelTwo :: Core.Maybe Types.HierarchyLevelUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HierarchyStructureUpdate' value with any optional fields omitted.
mkHierarchyStructureUpdate ::
  HierarchyStructureUpdate
mkHierarchyStructureUpdate =
  HierarchyStructureUpdate'
    { levelFive = Core.Nothing,
      levelFour = Core.Nothing,
      levelOne = Core.Nothing,
      levelThree = Core.Nothing,
      levelTwo = Core.Nothing
    }

-- | The update for level five.
--
-- /Note:/ Consider using 'levelFive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsuLevelFive :: Lens.Lens' HierarchyStructureUpdate (Core.Maybe Types.HierarchyLevelUpdate)
hsuLevelFive = Lens.field @"levelFive"
{-# DEPRECATED hsuLevelFive "Use generic-lens or generic-optics with 'levelFive' instead." #-}

-- | The update for level four.
--
-- /Note:/ Consider using 'levelFour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsuLevelFour :: Lens.Lens' HierarchyStructureUpdate (Core.Maybe Types.HierarchyLevelUpdate)
hsuLevelFour = Lens.field @"levelFour"
{-# DEPRECATED hsuLevelFour "Use generic-lens or generic-optics with 'levelFour' instead." #-}

-- | The update for level one.
--
-- /Note:/ Consider using 'levelOne' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsuLevelOne :: Lens.Lens' HierarchyStructureUpdate (Core.Maybe Types.HierarchyLevelUpdate)
hsuLevelOne = Lens.field @"levelOne"
{-# DEPRECATED hsuLevelOne "Use generic-lens or generic-optics with 'levelOne' instead." #-}

-- | The update for level three.
--
-- /Note:/ Consider using 'levelThree' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsuLevelThree :: Lens.Lens' HierarchyStructureUpdate (Core.Maybe Types.HierarchyLevelUpdate)
hsuLevelThree = Lens.field @"levelThree"
{-# DEPRECATED hsuLevelThree "Use generic-lens or generic-optics with 'levelThree' instead." #-}

-- | The update for level two.
--
-- /Note:/ Consider using 'levelTwo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsuLevelTwo :: Lens.Lens' HierarchyStructureUpdate (Core.Maybe Types.HierarchyLevelUpdate)
hsuLevelTwo = Lens.field @"levelTwo"
{-# DEPRECATED hsuLevelTwo "Use generic-lens or generic-optics with 'levelTwo' instead." #-}

instance Core.FromJSON HierarchyStructureUpdate where
  toJSON HierarchyStructureUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ ("LevelFive" Core..=) Core.<$> levelFive,
            ("LevelFour" Core..=) Core.<$> levelFour,
            ("LevelOne" Core..=) Core.<$> levelOne,
            ("LevelThree" Core..=) Core.<$> levelThree,
            ("LevelTwo" Core..=) Core.<$> levelTwo
          ]
      )
