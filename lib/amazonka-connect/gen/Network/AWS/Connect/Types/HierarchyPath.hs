{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.HierarchyPath
  ( HierarchyPath (..)
  -- * Smart constructor
  , mkHierarchyPath
  -- * Lenses
  , hpLevelFive
  , hpLevelFour
  , hpLevelOne
  , hpLevelThree
  , hpLevelTwo
  ) where

import qualified Network.AWS.Connect.Types.HierarchyGroupSummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the levels of a hierarchy group.
--
-- /See:/ 'mkHierarchyPath' smart constructor.
data HierarchyPath = HierarchyPath'
  { levelFive :: Core.Maybe Types.HierarchyGroupSummary
    -- ^ Information about level five.
  , levelFour :: Core.Maybe Types.HierarchyGroupSummary
    -- ^ Information about level four.
  , levelOne :: Core.Maybe Types.HierarchyGroupSummary
    -- ^ Information about level one.
  , levelThree :: Core.Maybe Types.HierarchyGroupSummary
    -- ^ Information about level three.
  , levelTwo :: Core.Maybe Types.HierarchyGroupSummary
    -- ^ Information about level two.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HierarchyPath' value with any optional fields omitted.
mkHierarchyPath
    :: HierarchyPath
mkHierarchyPath
  = HierarchyPath'{levelFive = Core.Nothing,
                   levelFour = Core.Nothing, levelOne = Core.Nothing,
                   levelThree = Core.Nothing, levelTwo = Core.Nothing}

-- | Information about level five.
--
-- /Note:/ Consider using 'levelFive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpLevelFive :: Lens.Lens' HierarchyPath (Core.Maybe Types.HierarchyGroupSummary)
hpLevelFive = Lens.field @"levelFive"
{-# INLINEABLE hpLevelFive #-}
{-# DEPRECATED levelFive "Use generic-lens or generic-optics with 'levelFive' instead"  #-}

-- | Information about level four.
--
-- /Note:/ Consider using 'levelFour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpLevelFour :: Lens.Lens' HierarchyPath (Core.Maybe Types.HierarchyGroupSummary)
hpLevelFour = Lens.field @"levelFour"
{-# INLINEABLE hpLevelFour #-}
{-# DEPRECATED levelFour "Use generic-lens or generic-optics with 'levelFour' instead"  #-}

-- | Information about level one.
--
-- /Note:/ Consider using 'levelOne' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpLevelOne :: Lens.Lens' HierarchyPath (Core.Maybe Types.HierarchyGroupSummary)
hpLevelOne = Lens.field @"levelOne"
{-# INLINEABLE hpLevelOne #-}
{-# DEPRECATED levelOne "Use generic-lens or generic-optics with 'levelOne' instead"  #-}

-- | Information about level three.
--
-- /Note:/ Consider using 'levelThree' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpLevelThree :: Lens.Lens' HierarchyPath (Core.Maybe Types.HierarchyGroupSummary)
hpLevelThree = Lens.field @"levelThree"
{-# INLINEABLE hpLevelThree #-}
{-# DEPRECATED levelThree "Use generic-lens or generic-optics with 'levelThree' instead"  #-}

-- | Information about level two.
--
-- /Note:/ Consider using 'levelTwo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpLevelTwo :: Lens.Lens' HierarchyPath (Core.Maybe Types.HierarchyGroupSummary)
hpLevelTwo = Lens.field @"levelTwo"
{-# INLINEABLE hpLevelTwo #-}
{-# DEPRECATED levelTwo "Use generic-lens or generic-optics with 'levelTwo' instead"  #-}

instance Core.FromJSON HierarchyPath where
        parseJSON
          = Core.withObject "HierarchyPath" Core.$
              \ x ->
                HierarchyPath' Core.<$>
                  (x Core..:? "LevelFive") Core.<*> x Core..:? "LevelFour" Core.<*>
                    x Core..:? "LevelOne"
                    Core.<*> x Core..:? "LevelThree"
                    Core.<*> x Core..:? "LevelTwo"
