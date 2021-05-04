{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskRunSortCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskRunSortCriteria where

import Network.AWS.Glue.Types.SortDirectionType
import Network.AWS.Glue.Types.TaskRunSortColumnType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The sorting criteria that are used to sort the list of task runs for the
-- machine learning transform.
--
-- /See:/ 'newTaskRunSortCriteria' smart constructor.
data TaskRunSortCriteria = TaskRunSortCriteria'
  { -- | The column to be used to sort the list of task runs for the machine
    -- learning transform.
    column :: TaskRunSortColumnType,
    -- | The sort direction to be used to sort the list of task runs for the
    -- machine learning transform.
    sortDirection :: SortDirectionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TaskRunSortCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'column', 'taskRunSortCriteria_column' - The column to be used to sort the list of task runs for the machine
-- learning transform.
--
-- 'sortDirection', 'taskRunSortCriteria_sortDirection' - The sort direction to be used to sort the list of task runs for the
-- machine learning transform.
newTaskRunSortCriteria ::
  -- | 'column'
  TaskRunSortColumnType ->
  -- | 'sortDirection'
  SortDirectionType ->
  TaskRunSortCriteria
newTaskRunSortCriteria pColumn_ pSortDirection_ =
  TaskRunSortCriteria'
    { column = pColumn_,
      sortDirection = pSortDirection_
    }

-- | The column to be used to sort the list of task runs for the machine
-- learning transform.
taskRunSortCriteria_column :: Lens.Lens' TaskRunSortCriteria TaskRunSortColumnType
taskRunSortCriteria_column = Lens.lens (\TaskRunSortCriteria' {column} -> column) (\s@TaskRunSortCriteria' {} a -> s {column = a} :: TaskRunSortCriteria)

-- | The sort direction to be used to sort the list of task runs for the
-- machine learning transform.
taskRunSortCriteria_sortDirection :: Lens.Lens' TaskRunSortCriteria SortDirectionType
taskRunSortCriteria_sortDirection = Lens.lens (\TaskRunSortCriteria' {sortDirection} -> sortDirection) (\s@TaskRunSortCriteria' {} a -> s {sortDirection = a} :: TaskRunSortCriteria)

instance Prelude.Hashable TaskRunSortCriteria

instance Prelude.NFData TaskRunSortCriteria

instance Prelude.ToJSON TaskRunSortCriteria where
  toJSON TaskRunSortCriteria' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Column" Prelude..= column),
            Prelude.Just
              ("SortDirection" Prelude..= sortDirection)
          ]
      )
