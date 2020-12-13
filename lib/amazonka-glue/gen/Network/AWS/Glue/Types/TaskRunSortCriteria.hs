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
    trscSortDirection,
    trscColumn,
  )
where

import Network.AWS.Glue.Types.SortDirectionType
import Network.AWS.Glue.Types.TaskRunSortColumnType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The sorting criteria that are used to sort the list of task runs for the machine learning transform.
--
-- /See:/ 'mkTaskRunSortCriteria' smart constructor.
data TaskRunSortCriteria = TaskRunSortCriteria'
  { -- | The sort direction to be used to sort the list of task runs for the machine learning transform.
    sortDirection :: SortDirectionType,
    -- | The column to be used to sort the list of task runs for the machine learning transform.
    column :: TaskRunSortColumnType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskRunSortCriteria' with the minimum fields required to make a request.
--
-- * 'sortDirection' - The sort direction to be used to sort the list of task runs for the machine learning transform.
-- * 'column' - The column to be used to sort the list of task runs for the machine learning transform.
mkTaskRunSortCriteria ::
  -- | 'sortDirection'
  SortDirectionType ->
  -- | 'column'
  TaskRunSortColumnType ->
  TaskRunSortCriteria
mkTaskRunSortCriteria pSortDirection_ pColumn_ =
  TaskRunSortCriteria'
    { sortDirection = pSortDirection_,
      column = pColumn_
    }

-- | The sort direction to be used to sort the list of task runs for the machine learning transform.
--
-- /Note:/ Consider using 'sortDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trscSortDirection :: Lens.Lens' TaskRunSortCriteria SortDirectionType
trscSortDirection = Lens.lens (sortDirection :: TaskRunSortCriteria -> SortDirectionType) (\s a -> s {sortDirection = a} :: TaskRunSortCriteria)
{-# DEPRECATED trscSortDirection "Use generic-lens or generic-optics with 'sortDirection' instead." #-}

-- | The column to be used to sort the list of task runs for the machine learning transform.
--
-- /Note:/ Consider using 'column' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trscColumn :: Lens.Lens' TaskRunSortCriteria TaskRunSortColumnType
trscColumn = Lens.lens (column :: TaskRunSortCriteria -> TaskRunSortColumnType) (\s a -> s {column = a} :: TaskRunSortCriteria)
{-# DEPRECATED trscColumn "Use generic-lens or generic-optics with 'column' instead." #-}

instance Lude.ToJSON TaskRunSortCriteria where
  toJSON TaskRunSortCriteria' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SortDirection" Lude..= sortDirection),
            Lude.Just ("Column" Lude..= column)
          ]
      )
