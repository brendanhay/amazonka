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
-- Module      : Network.AWS.SWF.Types.TaskList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.TaskList where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a task list.
--
-- /See:/ 'newTaskList' smart constructor.
data TaskList = TaskList'
  { -- | The name of the task list.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TaskList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'taskList_name' - The name of the task list.
newTaskList ::
  -- | 'name'
  Prelude.Text ->
  TaskList
newTaskList pName_ = TaskList' {name = pName_}

-- | The name of the task list.
taskList_name :: Lens.Lens' TaskList Prelude.Text
taskList_name = Lens.lens (\TaskList' {name} -> name) (\s@TaskList' {} a -> s {name = a} :: TaskList)

instance Prelude.FromJSON TaskList where
  parseJSON =
    Prelude.withObject
      "TaskList"
      (\x -> TaskList' Prelude.<$> (x Prelude..: "name"))

instance Prelude.Hashable TaskList

instance Prelude.NFData TaskList

instance Prelude.ToJSON TaskList where
  toJSON TaskList' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Prelude..= name)]
      )
