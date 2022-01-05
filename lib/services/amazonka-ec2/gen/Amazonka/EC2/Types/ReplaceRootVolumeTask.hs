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
-- Module      : Amazonka.EC2.Types.ReplaceRootVolumeTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReplaceRootVolumeTask where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ReplaceRootVolumeTaskState
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a root volume replacement task.
--
-- /See:/ 'newReplaceRootVolumeTask' smart constructor.
data ReplaceRootVolumeTask = ReplaceRootVolumeTask'
  { -- | The ID of the instance for which the root volume replacement task was
    -- created.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The state of the task. The task can be in one of the following states:
    --
    -- -   @pending@ - the replacement volume is being created.
    --
    -- -   @in-progress@ - the original volume is being detached and the
    --     replacement volume is being attached.
    --
    -- -   @succeeded@ - the replacement volume has been successfully attached
    --     to the instance and the instance is available.
    --
    -- -   @failing@ - the replacement task is in the process of failing.
    --
    -- -   @failed@ - the replacement task has failed but the original root
    --     volume is still attached.
    --
    -- -   @failing-detached@ - the replacement task is in the process of
    --     failing. The instance might have no root volume attached.
    --
    -- -   @failed-detached@ - the replacement task has failed and the instance
    --     has no root volume attached.
    taskState :: Prelude.Maybe ReplaceRootVolumeTaskState,
    -- | The time the task was started.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The time the task completed.
    completeTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the root volume replacement task.
    replaceRootVolumeTaskId :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the task.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplaceRootVolumeTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'replaceRootVolumeTask_instanceId' - The ID of the instance for which the root volume replacement task was
-- created.
--
-- 'taskState', 'replaceRootVolumeTask_taskState' - The state of the task. The task can be in one of the following states:
--
-- -   @pending@ - the replacement volume is being created.
--
-- -   @in-progress@ - the original volume is being detached and the
--     replacement volume is being attached.
--
-- -   @succeeded@ - the replacement volume has been successfully attached
--     to the instance and the instance is available.
--
-- -   @failing@ - the replacement task is in the process of failing.
--
-- -   @failed@ - the replacement task has failed but the original root
--     volume is still attached.
--
-- -   @failing-detached@ - the replacement task is in the process of
--     failing. The instance might have no root volume attached.
--
-- -   @failed-detached@ - the replacement task has failed and the instance
--     has no root volume attached.
--
-- 'startTime', 'replaceRootVolumeTask_startTime' - The time the task was started.
--
-- 'completeTime', 'replaceRootVolumeTask_completeTime' - The time the task completed.
--
-- 'replaceRootVolumeTaskId', 'replaceRootVolumeTask_replaceRootVolumeTaskId' - The ID of the root volume replacement task.
--
-- 'tags', 'replaceRootVolumeTask_tags' - The tags assigned to the task.
newReplaceRootVolumeTask ::
  ReplaceRootVolumeTask
newReplaceRootVolumeTask =
  ReplaceRootVolumeTask'
    { instanceId =
        Prelude.Nothing,
      taskState = Prelude.Nothing,
      startTime = Prelude.Nothing,
      completeTime = Prelude.Nothing,
      replaceRootVolumeTaskId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the instance for which the root volume replacement task was
-- created.
replaceRootVolumeTask_instanceId :: Lens.Lens' ReplaceRootVolumeTask (Prelude.Maybe Prelude.Text)
replaceRootVolumeTask_instanceId = Lens.lens (\ReplaceRootVolumeTask' {instanceId} -> instanceId) (\s@ReplaceRootVolumeTask' {} a -> s {instanceId = a} :: ReplaceRootVolumeTask)

-- | The state of the task. The task can be in one of the following states:
--
-- -   @pending@ - the replacement volume is being created.
--
-- -   @in-progress@ - the original volume is being detached and the
--     replacement volume is being attached.
--
-- -   @succeeded@ - the replacement volume has been successfully attached
--     to the instance and the instance is available.
--
-- -   @failing@ - the replacement task is in the process of failing.
--
-- -   @failed@ - the replacement task has failed but the original root
--     volume is still attached.
--
-- -   @failing-detached@ - the replacement task is in the process of
--     failing. The instance might have no root volume attached.
--
-- -   @failed-detached@ - the replacement task has failed and the instance
--     has no root volume attached.
replaceRootVolumeTask_taskState :: Lens.Lens' ReplaceRootVolumeTask (Prelude.Maybe ReplaceRootVolumeTaskState)
replaceRootVolumeTask_taskState = Lens.lens (\ReplaceRootVolumeTask' {taskState} -> taskState) (\s@ReplaceRootVolumeTask' {} a -> s {taskState = a} :: ReplaceRootVolumeTask)

-- | The time the task was started.
replaceRootVolumeTask_startTime :: Lens.Lens' ReplaceRootVolumeTask (Prelude.Maybe Prelude.Text)
replaceRootVolumeTask_startTime = Lens.lens (\ReplaceRootVolumeTask' {startTime} -> startTime) (\s@ReplaceRootVolumeTask' {} a -> s {startTime = a} :: ReplaceRootVolumeTask)

-- | The time the task completed.
replaceRootVolumeTask_completeTime :: Lens.Lens' ReplaceRootVolumeTask (Prelude.Maybe Prelude.Text)
replaceRootVolumeTask_completeTime = Lens.lens (\ReplaceRootVolumeTask' {completeTime} -> completeTime) (\s@ReplaceRootVolumeTask' {} a -> s {completeTime = a} :: ReplaceRootVolumeTask)

-- | The ID of the root volume replacement task.
replaceRootVolumeTask_replaceRootVolumeTaskId :: Lens.Lens' ReplaceRootVolumeTask (Prelude.Maybe Prelude.Text)
replaceRootVolumeTask_replaceRootVolumeTaskId = Lens.lens (\ReplaceRootVolumeTask' {replaceRootVolumeTaskId} -> replaceRootVolumeTaskId) (\s@ReplaceRootVolumeTask' {} a -> s {replaceRootVolumeTaskId = a} :: ReplaceRootVolumeTask)

-- | The tags assigned to the task.
replaceRootVolumeTask_tags :: Lens.Lens' ReplaceRootVolumeTask (Prelude.Maybe [Tag])
replaceRootVolumeTask_tags = Lens.lens (\ReplaceRootVolumeTask' {tags} -> tags) (\s@ReplaceRootVolumeTask' {} a -> s {tags = a} :: ReplaceRootVolumeTask) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML ReplaceRootVolumeTask where
  parseXML x =
    ReplaceRootVolumeTask'
      Prelude.<$> (x Core..@? "instanceId")
      Prelude.<*> (x Core..@? "taskState")
      Prelude.<*> (x Core..@? "startTime")
      Prelude.<*> (x Core..@? "completeTime")
      Prelude.<*> (x Core..@? "replaceRootVolumeTaskId")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable ReplaceRootVolumeTask where
  hashWithSalt _salt ReplaceRootVolumeTask' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` taskState
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` completeTime
      `Prelude.hashWithSalt` replaceRootVolumeTaskId
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ReplaceRootVolumeTask where
  rnf ReplaceRootVolumeTask' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf taskState
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf completeTime
      `Prelude.seq` Prelude.rnf replaceRootVolumeTaskId
      `Prelude.seq` Prelude.rnf tags
