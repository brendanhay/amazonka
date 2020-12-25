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
    sseAttribute,
    sseSortOrder,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.SnapshotAttributeToSortBy as Types
import qualified Network.AWS.Redshift.Types.SortByOrder as Types

-- | Describes a sorting entity
--
-- /See:/ 'mkSnapshotSortingEntity' smart constructor.
data SnapshotSortingEntity = SnapshotSortingEntity'
  { -- | The category for sorting the snapshots.
    attribute :: Types.SnapshotAttributeToSortBy,
    -- | The order for listing the attributes.
    sortOrder :: Core.Maybe Types.SortByOrder
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SnapshotSortingEntity' value with any optional fields omitted.
mkSnapshotSortingEntity ::
  -- | 'attribute'
  Types.SnapshotAttributeToSortBy ->
  SnapshotSortingEntity
mkSnapshotSortingEntity attribute =
  SnapshotSortingEntity' {attribute, sortOrder = Core.Nothing}

-- | The category for sorting the snapshots.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseAttribute :: Lens.Lens' SnapshotSortingEntity Types.SnapshotAttributeToSortBy
sseAttribute = Lens.field @"attribute"
{-# DEPRECATED sseAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The order for listing the attributes.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseSortOrder :: Lens.Lens' SnapshotSortingEntity (Core.Maybe Types.SortByOrder)
sseSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED sseSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}
