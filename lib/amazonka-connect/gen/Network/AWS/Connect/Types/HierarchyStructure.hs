{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyStructure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyStructure
  ( HierarchyStructure (..),

    -- * Smart constructor
    mkHierarchyStructure,

    -- * Lenses
    hsLevelFive,
    hsLevelFour,
    hsLevelOne,
    hsLevelThree,
    hsLevelTwo,
  )
where

import qualified Network.AWS.Connect.Types.HierarchyLevel as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a hierarchy structure.
--
-- /See:/ 'mkHierarchyStructure' smart constructor.
data HierarchyStructure = HierarchyStructure'
  { -- | Information about level five.
    levelFive :: Core.Maybe Types.HierarchyLevel,
    -- | Information about level four.
    levelFour :: Core.Maybe Types.HierarchyLevel,
    -- | Information about level one.
    levelOne :: Core.Maybe Types.HierarchyLevel,
    -- | Information about level three.
    levelThree :: Core.Maybe Types.HierarchyLevel,
    -- | Information about level two.
    levelTwo :: Core.Maybe Types.HierarchyLevel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HierarchyStructure' value with any optional fields omitted.
mkHierarchyStructure ::
  HierarchyStructure
mkHierarchyStructure =
  HierarchyStructure'
    { levelFive = Core.Nothing,
      levelFour = Core.Nothing,
      levelOne = Core.Nothing,
      levelThree = Core.Nothing,
      levelTwo = Core.Nothing
    }

-- | Information about level five.
--
-- /Note:/ Consider using 'levelFive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsLevelFive :: Lens.Lens' HierarchyStructure (Core.Maybe Types.HierarchyLevel)
hsLevelFive = Lens.field @"levelFive"
{-# DEPRECATED hsLevelFive "Use generic-lens or generic-optics with 'levelFive' instead." #-}

-- | Information about level four.
--
-- /Note:/ Consider using 'levelFour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsLevelFour :: Lens.Lens' HierarchyStructure (Core.Maybe Types.HierarchyLevel)
hsLevelFour = Lens.field @"levelFour"
{-# DEPRECATED hsLevelFour "Use generic-lens or generic-optics with 'levelFour' instead." #-}

-- | Information about level one.
--
-- /Note:/ Consider using 'levelOne' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsLevelOne :: Lens.Lens' HierarchyStructure (Core.Maybe Types.HierarchyLevel)
hsLevelOne = Lens.field @"levelOne"
{-# DEPRECATED hsLevelOne "Use generic-lens or generic-optics with 'levelOne' instead." #-}

-- | Information about level three.
--
-- /Note:/ Consider using 'levelThree' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsLevelThree :: Lens.Lens' HierarchyStructure (Core.Maybe Types.HierarchyLevel)
hsLevelThree = Lens.field @"levelThree"
{-# DEPRECATED hsLevelThree "Use generic-lens or generic-optics with 'levelThree' instead." #-}

-- | Information about level two.
--
-- /Note:/ Consider using 'levelTwo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsLevelTwo :: Lens.Lens' HierarchyStructure (Core.Maybe Types.HierarchyLevel)
hsLevelTwo = Lens.field @"levelTwo"
{-# DEPRECATED hsLevelTwo "Use generic-lens or generic-optics with 'levelTwo' instead." #-}

instance Core.FromJSON HierarchyStructure where
  parseJSON =
    Core.withObject "HierarchyStructure" Core.$
      \x ->
        HierarchyStructure'
          Core.<$> (x Core..:? "LevelFive")
          Core.<*> (x Core..:? "LevelFour")
          Core.<*> (x Core..:? "LevelOne")
          Core.<*> (x Core..:? "LevelThree")
          Core.<*> (x Core..:? "LevelTwo")
