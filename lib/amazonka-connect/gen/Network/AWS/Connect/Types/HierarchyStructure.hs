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
    hsLevelThree,
    hsLevelFour,
    hsLevelTwo,
    hsLevelOne,
  )
where

import Network.AWS.Connect.Types.HierarchyLevel
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a hierarchy structure.
--
-- /See:/ 'mkHierarchyStructure' smart constructor.
data HierarchyStructure = HierarchyStructure'
  { levelFive ::
      Lude.Maybe HierarchyLevel,
    levelThree :: Lude.Maybe HierarchyLevel,
    levelFour :: Lude.Maybe HierarchyLevel,
    levelTwo :: Lude.Maybe HierarchyLevel,
    levelOne :: Lude.Maybe HierarchyLevel
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HierarchyStructure' with the minimum fields required to make a request.
--
-- * 'levelFive' - Information about level five.
-- * 'levelFour' - Information about level four.
-- * 'levelOne' - Information about level one.
-- * 'levelThree' - Information about level three.
-- * 'levelTwo' - Information about level two.
mkHierarchyStructure ::
  HierarchyStructure
mkHierarchyStructure =
  HierarchyStructure'
    { levelFive = Lude.Nothing,
      levelThree = Lude.Nothing,
      levelFour = Lude.Nothing,
      levelTwo = Lude.Nothing,
      levelOne = Lude.Nothing
    }

-- | Information about level five.
--
-- /Note:/ Consider using 'levelFive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsLevelFive :: Lens.Lens' HierarchyStructure (Lude.Maybe HierarchyLevel)
hsLevelFive = Lens.lens (levelFive :: HierarchyStructure -> Lude.Maybe HierarchyLevel) (\s a -> s {levelFive = a} :: HierarchyStructure)
{-# DEPRECATED hsLevelFive "Use generic-lens or generic-optics with 'levelFive' instead." #-}

-- | Information about level three.
--
-- /Note:/ Consider using 'levelThree' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsLevelThree :: Lens.Lens' HierarchyStructure (Lude.Maybe HierarchyLevel)
hsLevelThree = Lens.lens (levelThree :: HierarchyStructure -> Lude.Maybe HierarchyLevel) (\s a -> s {levelThree = a} :: HierarchyStructure)
{-# DEPRECATED hsLevelThree "Use generic-lens or generic-optics with 'levelThree' instead." #-}

-- | Information about level four.
--
-- /Note:/ Consider using 'levelFour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsLevelFour :: Lens.Lens' HierarchyStructure (Lude.Maybe HierarchyLevel)
hsLevelFour = Lens.lens (levelFour :: HierarchyStructure -> Lude.Maybe HierarchyLevel) (\s a -> s {levelFour = a} :: HierarchyStructure)
{-# DEPRECATED hsLevelFour "Use generic-lens or generic-optics with 'levelFour' instead." #-}

-- | Information about level two.
--
-- /Note:/ Consider using 'levelTwo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsLevelTwo :: Lens.Lens' HierarchyStructure (Lude.Maybe HierarchyLevel)
hsLevelTwo = Lens.lens (levelTwo :: HierarchyStructure -> Lude.Maybe HierarchyLevel) (\s a -> s {levelTwo = a} :: HierarchyStructure)
{-# DEPRECATED hsLevelTwo "Use generic-lens or generic-optics with 'levelTwo' instead." #-}

-- | Information about level one.
--
-- /Note:/ Consider using 'levelOne' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsLevelOne :: Lens.Lens' HierarchyStructure (Lude.Maybe HierarchyLevel)
hsLevelOne = Lens.lens (levelOne :: HierarchyStructure -> Lude.Maybe HierarchyLevel) (\s a -> s {levelOne = a} :: HierarchyStructure)
{-# DEPRECATED hsLevelOne "Use generic-lens or generic-optics with 'levelOne' instead." #-}

instance Lude.FromJSON HierarchyStructure where
  parseJSON =
    Lude.withObject
      "HierarchyStructure"
      ( \x ->
          HierarchyStructure'
            Lude.<$> (x Lude..:? "LevelFive")
            Lude.<*> (x Lude..:? "LevelThree")
            Lude.<*> (x Lude..:? "LevelFour")
            Lude.<*> (x Lude..:? "LevelTwo")
            Lude.<*> (x Lude..:? "LevelOne")
      )
