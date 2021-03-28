{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SnapshotSortingEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.SnapshotSortingEntity
  ( SnapshotSortingEntity (..)
  -- * Smart constructor
  , mkSnapshotSortingEntity
  -- * Lenses
  , sseAttribute
  , sseSortOrder
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.SnapshotAttributeToSortBy as Types
import qualified Network.AWS.Redshift.Types.SortByOrder as Types

-- | Describes a sorting entity
--
-- /See:/ 'mkSnapshotSortingEntity' smart constructor.
data SnapshotSortingEntity = SnapshotSortingEntity'
  { attribute :: Types.SnapshotAttributeToSortBy
    -- ^ The category for sorting the snapshots.
  , sortOrder :: Core.Maybe Types.SortByOrder
    -- ^ The order for listing the attributes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SnapshotSortingEntity' value with any optional fields omitted.
mkSnapshotSortingEntity
    :: Types.SnapshotAttributeToSortBy -- ^ 'attribute'
    -> SnapshotSortingEntity
mkSnapshotSortingEntity attribute
  = SnapshotSortingEntity'{attribute, sortOrder = Core.Nothing}

-- | The category for sorting the snapshots.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseAttribute :: Lens.Lens' SnapshotSortingEntity Types.SnapshotAttributeToSortBy
sseAttribute = Lens.field @"attribute"
{-# INLINEABLE sseAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | The order for listing the attributes.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseSortOrder :: Lens.Lens' SnapshotSortingEntity (Core.Maybe Types.SortByOrder)
sseSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE sseSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery SnapshotSortingEntity where
        toQuery SnapshotSortingEntity{..}
          = Core.toQueryPair "Attribute" attribute Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SortOrder") sortOrder
