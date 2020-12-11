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
    hsuLevelThree,
    hsuLevelFour,
    hsuLevelTwo,
    hsuLevelOne,
  )
where

import Network.AWS.Connect.Types.HierarchyLevelUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the level hierarchy to update.
--
-- /See:/ 'mkHierarchyStructureUpdate' smart constructor.
data HierarchyStructureUpdate = HierarchyStructureUpdate'
  { levelFive ::
      Lude.Maybe HierarchyLevelUpdate,
    levelThree ::
      Lude.Maybe HierarchyLevelUpdate,
    levelFour ::
      Lude.Maybe HierarchyLevelUpdate,
    levelTwo ::
      Lude.Maybe HierarchyLevelUpdate,
    levelOne ::
      Lude.Maybe HierarchyLevelUpdate
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HierarchyStructureUpdate' with the minimum fields required to make a request.
--
-- * 'levelFive' - The update for level five.
-- * 'levelFour' - The update for level four.
-- * 'levelOne' - The update for level one.
-- * 'levelThree' - The update for level three.
-- * 'levelTwo' - The update for level two.
mkHierarchyStructureUpdate ::
  HierarchyStructureUpdate
mkHierarchyStructureUpdate =
  HierarchyStructureUpdate'
    { levelFive = Lude.Nothing,
      levelThree = Lude.Nothing,
      levelFour = Lude.Nothing,
      levelTwo = Lude.Nothing,
      levelOne = Lude.Nothing
    }

-- | The update for level five.
--
-- /Note:/ Consider using 'levelFive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsuLevelFive :: Lens.Lens' HierarchyStructureUpdate (Lude.Maybe HierarchyLevelUpdate)
hsuLevelFive = Lens.lens (levelFive :: HierarchyStructureUpdate -> Lude.Maybe HierarchyLevelUpdate) (\s a -> s {levelFive = a} :: HierarchyStructureUpdate)
{-# DEPRECATED hsuLevelFive "Use generic-lens or generic-optics with 'levelFive' instead." #-}

-- | The update for level three.
--
-- /Note:/ Consider using 'levelThree' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsuLevelThree :: Lens.Lens' HierarchyStructureUpdate (Lude.Maybe HierarchyLevelUpdate)
hsuLevelThree = Lens.lens (levelThree :: HierarchyStructureUpdate -> Lude.Maybe HierarchyLevelUpdate) (\s a -> s {levelThree = a} :: HierarchyStructureUpdate)
{-# DEPRECATED hsuLevelThree "Use generic-lens or generic-optics with 'levelThree' instead." #-}

-- | The update for level four.
--
-- /Note:/ Consider using 'levelFour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsuLevelFour :: Lens.Lens' HierarchyStructureUpdate (Lude.Maybe HierarchyLevelUpdate)
hsuLevelFour = Lens.lens (levelFour :: HierarchyStructureUpdate -> Lude.Maybe HierarchyLevelUpdate) (\s a -> s {levelFour = a} :: HierarchyStructureUpdate)
{-# DEPRECATED hsuLevelFour "Use generic-lens or generic-optics with 'levelFour' instead." #-}

-- | The update for level two.
--
-- /Note:/ Consider using 'levelTwo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsuLevelTwo :: Lens.Lens' HierarchyStructureUpdate (Lude.Maybe HierarchyLevelUpdate)
hsuLevelTwo = Lens.lens (levelTwo :: HierarchyStructureUpdate -> Lude.Maybe HierarchyLevelUpdate) (\s a -> s {levelTwo = a} :: HierarchyStructureUpdate)
{-# DEPRECATED hsuLevelTwo "Use generic-lens or generic-optics with 'levelTwo' instead." #-}

-- | The update for level one.
--
-- /Note:/ Consider using 'levelOne' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsuLevelOne :: Lens.Lens' HierarchyStructureUpdate (Lude.Maybe HierarchyLevelUpdate)
hsuLevelOne = Lens.lens (levelOne :: HierarchyStructureUpdate -> Lude.Maybe HierarchyLevelUpdate) (\s a -> s {levelOne = a} :: HierarchyStructureUpdate)
{-# DEPRECATED hsuLevelOne "Use generic-lens or generic-optics with 'levelOne' instead." #-}

instance Lude.ToJSON HierarchyStructureUpdate where
  toJSON HierarchyStructureUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LevelFive" Lude..=) Lude.<$> levelFive,
            ("LevelThree" Lude..=) Lude.<$> levelThree,
            ("LevelFour" Lude..=) Lude.<$> levelFour,
            ("LevelTwo" Lude..=) Lude.<$> levelTwo,
            ("LevelOne" Lude..=) Lude.<$> levelOne
          ]
      )
