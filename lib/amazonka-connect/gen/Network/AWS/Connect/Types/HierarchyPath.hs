-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyPath
  ( HierarchyPath (..),

    -- * Smart constructor
    mkHierarchyPath,

    -- * Lenses
    hpLevelFive,
    hpLevelThree,
    hpLevelFour,
    hpLevelTwo,
    hpLevelOne,
  )
where

import Network.AWS.Connect.Types.HierarchyGroupSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the levels of a hierarchy group.
--
-- /See:/ 'mkHierarchyPath' smart constructor.
data HierarchyPath = HierarchyPath'
  { levelFive ::
      Lude.Maybe HierarchyGroupSummary,
    levelThree :: Lude.Maybe HierarchyGroupSummary,
    levelFour :: Lude.Maybe HierarchyGroupSummary,
    levelTwo :: Lude.Maybe HierarchyGroupSummary,
    levelOne :: Lude.Maybe HierarchyGroupSummary
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HierarchyPath' with the minimum fields required to make a request.
--
-- * 'levelFive' - Information about level five.
-- * 'levelFour' - Information about level four.
-- * 'levelOne' - Information about level one.
-- * 'levelThree' - Information about level three.
-- * 'levelTwo' - Information about level two.
mkHierarchyPath ::
  HierarchyPath
mkHierarchyPath =
  HierarchyPath'
    { levelFive = Lude.Nothing,
      levelThree = Lude.Nothing,
      levelFour = Lude.Nothing,
      levelTwo = Lude.Nothing,
      levelOne = Lude.Nothing
    }

-- | Information about level five.
--
-- /Note:/ Consider using 'levelFive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpLevelFive :: Lens.Lens' HierarchyPath (Lude.Maybe HierarchyGroupSummary)
hpLevelFive = Lens.lens (levelFive :: HierarchyPath -> Lude.Maybe HierarchyGroupSummary) (\s a -> s {levelFive = a} :: HierarchyPath)
{-# DEPRECATED hpLevelFive "Use generic-lens or generic-optics with 'levelFive' instead." #-}

-- | Information about level three.
--
-- /Note:/ Consider using 'levelThree' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpLevelThree :: Lens.Lens' HierarchyPath (Lude.Maybe HierarchyGroupSummary)
hpLevelThree = Lens.lens (levelThree :: HierarchyPath -> Lude.Maybe HierarchyGroupSummary) (\s a -> s {levelThree = a} :: HierarchyPath)
{-# DEPRECATED hpLevelThree "Use generic-lens or generic-optics with 'levelThree' instead." #-}

-- | Information about level four.
--
-- /Note:/ Consider using 'levelFour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpLevelFour :: Lens.Lens' HierarchyPath (Lude.Maybe HierarchyGroupSummary)
hpLevelFour = Lens.lens (levelFour :: HierarchyPath -> Lude.Maybe HierarchyGroupSummary) (\s a -> s {levelFour = a} :: HierarchyPath)
{-# DEPRECATED hpLevelFour "Use generic-lens or generic-optics with 'levelFour' instead." #-}

-- | Information about level two.
--
-- /Note:/ Consider using 'levelTwo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpLevelTwo :: Lens.Lens' HierarchyPath (Lude.Maybe HierarchyGroupSummary)
hpLevelTwo = Lens.lens (levelTwo :: HierarchyPath -> Lude.Maybe HierarchyGroupSummary) (\s a -> s {levelTwo = a} :: HierarchyPath)
{-# DEPRECATED hpLevelTwo "Use generic-lens or generic-optics with 'levelTwo' instead." #-}

-- | Information about level one.
--
-- /Note:/ Consider using 'levelOne' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpLevelOne :: Lens.Lens' HierarchyPath (Lude.Maybe HierarchyGroupSummary)
hpLevelOne = Lens.lens (levelOne :: HierarchyPath -> Lude.Maybe HierarchyGroupSummary) (\s a -> s {levelOne = a} :: HierarchyPath)
{-# DEPRECATED hpLevelOne "Use generic-lens or generic-optics with 'levelOne' instead." #-}

instance Lude.FromJSON HierarchyPath where
  parseJSON =
    Lude.withObject
      "HierarchyPath"
      ( \x ->
          HierarchyPath'
            Lude.<$> (x Lude..:? "LevelFive")
            Lude.<*> (x Lude..:? "LevelThree")
            Lude.<*> (x Lude..:? "LevelFour")
            Lude.<*> (x Lude..:? "LevelTwo")
            Lude.<*> (x Lude..:? "LevelOne")
      )
