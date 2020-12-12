{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SnapshotSortingEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SnapshotSortingEntity
  ( SnapshotSortingEntity (..),

    -- * Smart constructor
    mkSnapshotSortingEntity,

    -- * Lenses
    sseSortOrder,
    sseAttribute,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.SnapshotAttributeToSortBy
import Network.AWS.Redshift.Types.SortByOrder

-- | Describes a sorting entity
--
-- /See:/ 'mkSnapshotSortingEntity' smart constructor.
data SnapshotSortingEntity = SnapshotSortingEntity'
  { sortOrder ::
      Lude.Maybe SortByOrder,
    attribute :: SnapshotAttributeToSortBy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SnapshotSortingEntity' with the minimum fields required to make a request.
--
-- * 'attribute' - The category for sorting the snapshots.
-- * 'sortOrder' - The order for listing the attributes.
mkSnapshotSortingEntity ::
  -- | 'attribute'
  SnapshotAttributeToSortBy ->
  SnapshotSortingEntity
mkSnapshotSortingEntity pAttribute_ =
  SnapshotSortingEntity'
    { sortOrder = Lude.Nothing,
      attribute = pAttribute_
    }

-- | The order for listing the attributes.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseSortOrder :: Lens.Lens' SnapshotSortingEntity (Lude.Maybe SortByOrder)
sseSortOrder = Lens.lens (sortOrder :: SnapshotSortingEntity -> Lude.Maybe SortByOrder) (\s a -> s {sortOrder = a} :: SnapshotSortingEntity)
{-# DEPRECATED sseSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The category for sorting the snapshots.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseAttribute :: Lens.Lens' SnapshotSortingEntity SnapshotAttributeToSortBy
sseAttribute = Lens.lens (attribute :: SnapshotSortingEntity -> SnapshotAttributeToSortBy) (\s a -> s {attribute = a} :: SnapshotSortingEntity)
{-# DEPRECATED sseAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

instance Lude.ToQuery SnapshotSortingEntity where
  toQuery SnapshotSortingEntity' {..} =
    Lude.mconcat
      ["SortOrder" Lude.=: sortOrder, "Attribute" Lude.=: attribute]
