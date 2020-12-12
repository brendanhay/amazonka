{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyLevelUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyLevelUpdate
  ( HierarchyLevelUpdate (..),

    -- * Smart constructor
    mkHierarchyLevelUpdate,

    -- * Lenses
    hluName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the hierarchy level to update.
--
-- /See:/ 'mkHierarchyLevelUpdate' smart constructor.
newtype HierarchyLevelUpdate = HierarchyLevelUpdate'
  { name ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HierarchyLevelUpdate' with the minimum fields required to make a request.
--
-- * 'name' - The name of the user hierarchy level. Must not be more than 50 characters.
mkHierarchyLevelUpdate ::
  -- | 'name'
  Lude.Text ->
  HierarchyLevelUpdate
mkHierarchyLevelUpdate pName_ =
  HierarchyLevelUpdate' {name = pName_}

-- | The name of the user hierarchy level. Must not be more than 50 characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hluName :: Lens.Lens' HierarchyLevelUpdate Lude.Text
hluName = Lens.lens (name :: HierarchyLevelUpdate -> Lude.Text) (\s a -> s {name = a} :: HierarchyLevelUpdate)
{-# DEPRECATED hluName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON HierarchyLevelUpdate where
  toJSON HierarchyLevelUpdate' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])
