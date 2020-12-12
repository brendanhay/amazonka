{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryGroup
  ( InventoryGroup (..),

    -- * Smart constructor
    mkInventoryGroup,

    -- * Lenses
    igName,
    igFilters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.InventoryFilter

-- | A user-defined set of one or more filters on which to aggregate inventory data. Groups return a count of resources that match and don't match the specified criteria.
--
-- /See:/ 'mkInventoryGroup' smart constructor.
data InventoryGroup = InventoryGroup'
  { name :: Lude.Text,
    filters :: Lude.NonEmpty InventoryFilter
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryGroup' with the minimum fields required to make a request.
--
-- * 'filters' - Filters define the criteria for the group. The @matchingCount@ field displays the number of resources that match the criteria. The @notMatchingCount@ field displays the number of resources that don't match the criteria.
-- * 'name' - The name of the group.
mkInventoryGroup ::
  -- | 'name'
  Lude.Text ->
  -- | 'filters'
  Lude.NonEmpty InventoryFilter ->
  InventoryGroup
mkInventoryGroup pName_ pFilters_ =
  InventoryGroup' {name = pName_, filters = pFilters_}

-- | The name of the group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igName :: Lens.Lens' InventoryGroup Lude.Text
igName = Lens.lens (name :: InventoryGroup -> Lude.Text) (\s a -> s {name = a} :: InventoryGroup)
{-# DEPRECATED igName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Filters define the criteria for the group. The @matchingCount@ field displays the number of resources that match the criteria. The @notMatchingCount@ field displays the number of resources that don't match the criteria.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igFilters :: Lens.Lens' InventoryGroup (Lude.NonEmpty InventoryFilter)
igFilters = Lens.lens (filters :: InventoryGroup -> Lude.NonEmpty InventoryFilter) (\s a -> s {filters = a} :: InventoryGroup)
{-# DEPRECATED igFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

instance Lude.ToJSON InventoryGroup where
  toJSON InventoryGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("Filters" Lude..= filters)
          ]
      )
