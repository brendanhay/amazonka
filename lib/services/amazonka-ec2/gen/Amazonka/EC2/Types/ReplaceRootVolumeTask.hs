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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReplaceRootVolumeTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ReplaceRootVolumeTaskState
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Information about a root volume replacement task.
--
-- /See:/ 'newReplaceRootVolumeTask' smart constructor.
data ReplaceRootVolumeTask = ReplaceRootVolumeTask'
  { -- | The tags assigned to the task.
    tags :: Prelude.Maybe [Tag],
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
    -- | The ID of the snapshot used to create the replacement root volume.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the root volume replacement task.
    replaceRootVolumeTaskId :: Prelude.Maybe Prelude.Text,
    -- | The time the task completed.
    completeTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance for which the root volume replacement task was
    -- created.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the original root volume is to be deleted after the
    -- root volume replacement task completes.
    deleteReplacedRootVolume :: Prelude.Maybe Prelude.Bool,
    -- | The time the task was started.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AMI used to create the replacement root volume.
    imageId :: Prelude.Maybe Prelude.Text
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
-- 'tags', 'replaceRootVolumeTask_tags' - The tags assigned to the task.
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
-- 'snapshotId', 'replaceRootVolumeTask_snapshotId' - The ID of the snapshot used to create the replacement root volume.
--
-- 'replaceRootVolumeTaskId', 'replaceRootVolumeTask_replaceRootVolumeTaskId' - The ID of the root volume replacement task.
--
-- 'completeTime', 'replaceRootVolumeTask_completeTime' - The time the task completed.
--
-- 'instanceId', 'replaceRootVolumeTask_instanceId' - The ID of the instance for which the root volume replacement task was
-- created.
--
-- 'deleteReplacedRootVolume', 'replaceRootVolumeTask_deleteReplacedRootVolume' - Indicates whether the original root volume is to be deleted after the
-- root volume replacement task completes.
--
-- 'startTime', 'replaceRootVolumeTask_startTime' - The time the task was started.
--
-- 'imageId', 'replaceRootVolumeTask_imageId' - The ID of the AMI used to create the replacement root volume.
newReplaceRootVolumeTask ::
  ReplaceRootVolumeTask
newReplaceRootVolumeTask =
  ReplaceRootVolumeTask'
    { tags = Prelude.Nothing,
      taskState = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      replaceRootVolumeTaskId = Prelude.Nothing,
      completeTime = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      deleteReplacedRootVolume = Prelude.Nothing,
      startTime = Prelude.Nothing,
      imageId = Prelude.Nothing
    }

-- | The tags assigned to the task.
replaceRootVolumeTask_tags :: Lens.Lens' ReplaceRootVolumeTask (Prelude.Maybe [Tag])
replaceRootVolumeTask_tags = Lens.lens (\ReplaceRootVolumeTask' {tags} -> tags) (\s@ReplaceRootVolumeTask' {} a -> s {tags = a} :: ReplaceRootVolumeTask) Prelude.. Lens.mapping Lens.coerced

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

-- | The ID of the snapshot used to create the replacement root volume.
replaceRootVolumeTask_snapshotId :: Lens.Lens' ReplaceRootVolumeTask (Prelude.Maybe Prelude.Text)
replaceRootVolumeTask_snapshotId = Lens.lens (\ReplaceRootVolumeTask' {snapshotId} -> snapshotId) (\s@ReplaceRootVolumeTask' {} a -> s {snapshotId = a} :: ReplaceRootVolumeTask)

-- | The ID of the root volume replacement task.
replaceRootVolumeTask_replaceRootVolumeTaskId :: Lens.Lens' ReplaceRootVolumeTask (Prelude.Maybe Prelude.Text)
replaceRootVolumeTask_replaceRootVolumeTaskId = Lens.lens (\ReplaceRootVolumeTask' {replaceRootVolumeTaskId} -> replaceRootVolumeTaskId) (\s@ReplaceRootVolumeTask' {} a -> s {replaceRootVolumeTaskId = a} :: ReplaceRootVolumeTask)

-- | The time the task completed.
replaceRootVolumeTask_completeTime :: Lens.Lens' ReplaceRootVolumeTask (Prelude.Maybe Prelude.Text)
replaceRootVolumeTask_completeTime = Lens.lens (\ReplaceRootVolumeTask' {completeTime} -> completeTime) (\s@ReplaceRootVolumeTask' {} a -> s {completeTime = a} :: ReplaceRootVolumeTask)

-- | The ID of the instance for which the root volume replacement task was
-- created.
replaceRootVolumeTask_instanceId :: Lens.Lens' ReplaceRootVolumeTask (Prelude.Maybe Prelude.Text)
replaceRootVolumeTask_instanceId = Lens.lens (\ReplaceRootVolumeTask' {instanceId} -> instanceId) (\s@ReplaceRootVolumeTask' {} a -> s {instanceId = a} :: ReplaceRootVolumeTask)

-- | Indicates whether the original root volume is to be deleted after the
-- root volume replacement task completes.
replaceRootVolumeTask_deleteReplacedRootVolume :: Lens.Lens' ReplaceRootVolumeTask (Prelude.Maybe Prelude.Bool)
replaceRootVolumeTask_deleteReplacedRootVolume = Lens.lens (\ReplaceRootVolumeTask' {deleteReplacedRootVolume} -> deleteReplacedRootVolume) (\s@ReplaceRootVolumeTask' {} a -> s {deleteReplacedRootVolume = a} :: ReplaceRootVolumeTask)

-- | The time the task was started.
replaceRootVolumeTask_startTime :: Lens.Lens' ReplaceRootVolumeTask (Prelude.Maybe Prelude.Text)
replaceRootVolumeTask_startTime = Lens.lens (\ReplaceRootVolumeTask' {startTime} -> startTime) (\s@ReplaceRootVolumeTask' {} a -> s {startTime = a} :: ReplaceRootVolumeTask)

-- | The ID of the AMI used to create the replacement root volume.
replaceRootVolumeTask_imageId :: Lens.Lens' ReplaceRootVolumeTask (Prelude.Maybe Prelude.Text)
replaceRootVolumeTask_imageId = Lens.lens (\ReplaceRootVolumeTask' {imageId} -> imageId) (\s@ReplaceRootVolumeTask' {} a -> s {imageId = a} :: ReplaceRootVolumeTask)

instance Core.FromXML ReplaceRootVolumeTask where
  parseXML x =
    ReplaceRootVolumeTask'
      Prelude.<$> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "taskState")
      Prelude.<*> (x Core..@? "snapshotId")
      Prelude.<*> (x Core..@? "replaceRootVolumeTaskId")
      Prelude.<*> (x Core..@? "completeTime")
      Prelude.<*> (x Core..@? "instanceId")
      Prelude.<*> (x Core..@? "deleteReplacedRootVolume")
      Prelude.<*> (x Core..@? "startTime")
      Prelude.<*> (x Core..@? "imageId")

instance Prelude.Hashable ReplaceRootVolumeTask where
  hashWithSalt _salt ReplaceRootVolumeTask' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` taskState
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` replaceRootVolumeTaskId
      `Prelude.hashWithSalt` completeTime
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` deleteReplacedRootVolume
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` imageId

instance Prelude.NFData ReplaceRootVolumeTask where
  rnf ReplaceRootVolumeTask' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf taskState
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf replaceRootVolumeTaskId
      `Prelude.seq` Prelude.rnf completeTime
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf deleteReplacedRootVolume
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf imageId
