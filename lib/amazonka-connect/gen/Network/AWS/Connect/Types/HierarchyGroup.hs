{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyGroup
  ( HierarchyGroup (..),

    -- * Smart constructor
    mkHierarchyGroup,

    -- * Lenses
    hgARN,
    hgName,
    hgHierarchyPath,
    hgId,
    hgLevelId,
  )
where

import Network.AWS.Connect.Types.HierarchyPath
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a hierarchy group.
--
-- /See:/ 'mkHierarchyGroup' smart constructor.
data HierarchyGroup = HierarchyGroup'
  { -- | The Amazon Resource Name (ARN) of the hierarchy group.
    arn :: Lude.Maybe Lude.Text,
    -- | The name of the hierarchy group.
    name :: Lude.Maybe Lude.Text,
    -- | Information about the levels in the hierarchy group.
    hierarchyPath :: Lude.Maybe HierarchyPath,
    -- | The identifier of the hierarchy group.
    id :: Lude.Maybe Lude.Text,
    -- | The identifier of the level in the hierarchy group.
    levelId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HierarchyGroup' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the hierarchy group.
-- * 'name' - The name of the hierarchy group.
-- * 'hierarchyPath' - Information about the levels in the hierarchy group.
-- * 'id' - The identifier of the hierarchy group.
-- * 'levelId' - The identifier of the level in the hierarchy group.
mkHierarchyGroup ::
  HierarchyGroup
mkHierarchyGroup =
  HierarchyGroup'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      hierarchyPath = Lude.Nothing,
      id = Lude.Nothing,
      levelId = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the hierarchy group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgARN :: Lens.Lens' HierarchyGroup (Lude.Maybe Lude.Text)
hgARN = Lens.lens (arn :: HierarchyGroup -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: HierarchyGroup)
{-# DEPRECATED hgARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the hierarchy group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgName :: Lens.Lens' HierarchyGroup (Lude.Maybe Lude.Text)
hgName = Lens.lens (name :: HierarchyGroup -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: HierarchyGroup)
{-# DEPRECATED hgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Information about the levels in the hierarchy group.
--
-- /Note:/ Consider using 'hierarchyPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgHierarchyPath :: Lens.Lens' HierarchyGroup (Lude.Maybe HierarchyPath)
hgHierarchyPath = Lens.lens (hierarchyPath :: HierarchyGroup -> Lude.Maybe HierarchyPath) (\s a -> s {hierarchyPath = a} :: HierarchyGroup)
{-# DEPRECATED hgHierarchyPath "Use generic-lens or generic-optics with 'hierarchyPath' instead." #-}

-- | The identifier of the hierarchy group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgId :: Lens.Lens' HierarchyGroup (Lude.Maybe Lude.Text)
hgId = Lens.lens (id :: HierarchyGroup -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: HierarchyGroup)
{-# DEPRECATED hgId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The identifier of the level in the hierarchy group.
--
-- /Note:/ Consider using 'levelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgLevelId :: Lens.Lens' HierarchyGroup (Lude.Maybe Lude.Text)
hgLevelId = Lens.lens (levelId :: HierarchyGroup -> Lude.Maybe Lude.Text) (\s a -> s {levelId = a} :: HierarchyGroup)
{-# DEPRECATED hgLevelId "Use generic-lens or generic-optics with 'levelId' instead." #-}

instance Lude.FromJSON HierarchyGroup where
  parseJSON =
    Lude.withObject
      "HierarchyGroup"
      ( \x ->
          HierarchyGroup'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "HierarchyPath")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "LevelId")
      )
