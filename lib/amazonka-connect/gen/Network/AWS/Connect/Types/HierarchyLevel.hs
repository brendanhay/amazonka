{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyLevel
  ( HierarchyLevel (..),

    -- * Smart constructor
    mkHierarchyLevel,

    -- * Lenses
    hlARN,
    hlName,
    hlId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a hierarchy level.
--
-- /See:/ 'mkHierarchyLevel' smart constructor.
data HierarchyLevel = HierarchyLevel'
  { arn :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HierarchyLevel' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the hierarchy level.
-- * 'id' - The identifier of the hierarchy level.
-- * 'name' - The name of the hierarchy level.
mkHierarchyLevel ::
  HierarchyLevel
mkHierarchyLevel =
  HierarchyLevel'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the hierarchy level.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlARN :: Lens.Lens' HierarchyLevel (Lude.Maybe Lude.Text)
hlARN = Lens.lens (arn :: HierarchyLevel -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: HierarchyLevel)
{-# DEPRECATED hlARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the hierarchy level.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlName :: Lens.Lens' HierarchyLevel (Lude.Maybe Lude.Text)
hlName = Lens.lens (name :: HierarchyLevel -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: HierarchyLevel)
{-# DEPRECATED hlName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the hierarchy level.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlId :: Lens.Lens' HierarchyLevel (Lude.Maybe Lude.Text)
hlId = Lens.lens (id :: HierarchyLevel -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: HierarchyLevel)
{-# DEPRECATED hlId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON HierarchyLevel where
  parseJSON =
    Lude.withObject
      "HierarchyLevel"
      ( \x ->
          HierarchyLevel'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
      )
