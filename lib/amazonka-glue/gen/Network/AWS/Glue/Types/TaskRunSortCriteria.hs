{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskRunSortCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskRunSortCriteria
  ( TaskRunSortCriteria (..),

    -- * Smart constructor
    mkTaskRunSortCriteria,

    -- * Lenses
    trscColumn,
    trscSortDirection,
  )
where

import qualified Network.AWS.Glue.Types.SortDirectionType as Types
import qualified Network.AWS.Glue.Types.TaskRunSortColumnType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The sorting criteria that are used to sort the list of task runs for the machine learning transform.
--
-- /See:/ 'mkTaskRunSortCriteria' smart constructor.
data TaskRunSortCriteria = TaskRunSortCriteria'
  { -- | The column to be used to sort the list of task runs for the machine learning transform.
    column :: Types.TaskRunSortColumnType,
    -- | The sort direction to be used to sort the list of task runs for the machine learning transform.
    sortDirection :: Types.SortDirectionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaskRunSortCriteria' value with any optional fields omitted.
mkTaskRunSortCriteria ::
  -- | 'column'
  Types.TaskRunSortColumnType ->
  -- | 'sortDirection'
  Types.SortDirectionType ->
  TaskRunSortCriteria
mkTaskRunSortCriteria column sortDirection =
  TaskRunSortCriteria' {column, sortDirection}

-- | The column to be used to sort the list of task runs for the machine learning transform.
--
-- /Note:/ Consider using 'column' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trscColumn :: Lens.Lens' TaskRunSortCriteria Types.TaskRunSortColumnType
trscColumn = Lens.field @"column"
{-# DEPRECATED trscColumn "Use generic-lens or generic-optics with 'column' instead." #-}

-- | The sort direction to be used to sort the list of task runs for the machine learning transform.
--
-- /Note:/ Consider using 'sortDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trscSortDirection :: Lens.Lens' TaskRunSortCriteria Types.SortDirectionType
trscSortDirection = Lens.field @"sortDirection"
{-# DEPRECATED trscSortDirection "Use generic-lens or generic-optics with 'sortDirection' instead." #-}

instance Core.FromJSON TaskRunSortCriteria where
  toJSON TaskRunSortCriteria {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Column" Core..= column),
            Core.Just ("SortDirection" Core..= sortDirection)
          ]
      )
