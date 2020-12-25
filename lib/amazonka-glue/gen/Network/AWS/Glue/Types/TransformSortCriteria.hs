{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformSortCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformSortCriteria
  ( TransformSortCriteria (..),

    -- * Smart constructor
    mkTransformSortCriteria,

    -- * Lenses
    tscColumn,
    tscSortDirection,
  )
where

import qualified Network.AWS.Glue.Types.SortDirectionType as Types
import qualified Network.AWS.Glue.Types.TransformSortColumnType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The sorting criteria that are associated with the machine learning transform.
--
-- /See:/ 'mkTransformSortCriteria' smart constructor.
data TransformSortCriteria = TransformSortCriteria'
  { -- | The column to be used in the sorting criteria that are associated with the machine learning transform.
    column :: Types.TransformSortColumnType,
    -- | The sort direction to be used in the sorting criteria that are associated with the machine learning transform.
    sortDirection :: Types.SortDirectionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransformSortCriteria' value with any optional fields omitted.
mkTransformSortCriteria ::
  -- | 'column'
  Types.TransformSortColumnType ->
  -- | 'sortDirection'
  Types.SortDirectionType ->
  TransformSortCriteria
mkTransformSortCriteria column sortDirection =
  TransformSortCriteria' {column, sortDirection}

-- | The column to be used in the sorting criteria that are associated with the machine learning transform.
--
-- /Note:/ Consider using 'column' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tscColumn :: Lens.Lens' TransformSortCriteria Types.TransformSortColumnType
tscColumn = Lens.field @"column"
{-# DEPRECATED tscColumn "Use generic-lens or generic-optics with 'column' instead." #-}

-- | The sort direction to be used in the sorting criteria that are associated with the machine learning transform.
--
-- /Note:/ Consider using 'sortDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tscSortDirection :: Lens.Lens' TransformSortCriteria Types.SortDirectionType
tscSortDirection = Lens.field @"sortDirection"
{-# DEPRECATED tscSortDirection "Use generic-lens or generic-optics with 'sortDirection' instead." #-}

instance Core.FromJSON TransformSortCriteria where
  toJSON TransformSortCriteria {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Column" Core..= column),
            Core.Just ("SortDirection" Core..= sortDirection)
          ]
      )
