{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskRunSortCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskRunSortCriteria where

import Network.AWS.Glue.Types.SortDirectionType
import Network.AWS.Glue.Types.TaskRunSortColumnType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The sorting criteria that are used to sort the list of task runs for the machine learning transform.
--
--
--
-- /See:/ 'taskRunSortCriteria' smart constructor.
data TaskRunSortCriteria = TaskRunSortCriteria'
  { _trscColumn ::
      !TaskRunSortColumnType,
    _trscSortDirection :: !SortDirectionType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaskRunSortCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trscColumn' - The column to be used to sort the list of task runs for the machine learning transform.
--
-- * 'trscSortDirection' - The sort direction to be used to sort the list of task runs for the machine learning transform.
taskRunSortCriteria ::
  -- | 'trscColumn'
  TaskRunSortColumnType ->
  -- | 'trscSortDirection'
  SortDirectionType ->
  TaskRunSortCriteria
taskRunSortCriteria pColumn_ pSortDirection_ =
  TaskRunSortCriteria'
    { _trscColumn = pColumn_,
      _trscSortDirection = pSortDirection_
    }

-- | The column to be used to sort the list of task runs for the machine learning transform.
trscColumn :: Lens' TaskRunSortCriteria TaskRunSortColumnType
trscColumn = lens _trscColumn (\s a -> s {_trscColumn = a})

-- | The sort direction to be used to sort the list of task runs for the machine learning transform.
trscSortDirection :: Lens' TaskRunSortCriteria SortDirectionType
trscSortDirection = lens _trscSortDirection (\s a -> s {_trscSortDirection = a})

instance Hashable TaskRunSortCriteria

instance NFData TaskRunSortCriteria

instance ToJSON TaskRunSortCriteria where
  toJSON TaskRunSortCriteria' {..} =
    object
      ( catMaybes
          [ Just ("Column" .= _trscColumn),
            Just ("SortDirection" .= _trscSortDirection)
          ]
      )
