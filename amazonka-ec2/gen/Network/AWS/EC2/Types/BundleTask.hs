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
-- Module      : Network.AWS.EC2.Types.BundleTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.BundleTask where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.BundleTaskError
import Network.AWS.EC2.Types.BundleTaskState
import Network.AWS.EC2.Types.Storage
import qualified Network.AWS.Lens as Lens

-- | Describes a bundle task.
--
-- /See:/ 'newBundleTask' smart constructor.
data BundleTask = BundleTask'
  { -- | If the task fails, a description of the error.
    bundleTaskError :: Core.Maybe BundleTaskError,
    -- | The ID of the bundle task.
    bundleId :: Core.Text,
    -- | The ID of the instance associated with this bundle task.
    instanceId :: Core.Text,
    -- | The level of task completion, as a percent (for example, 20%).
    progress :: Core.Text,
    -- | The time this task started.
    startTime :: Core.ISO8601,
    -- | The state of the task.
    state :: BundleTaskState,
    -- | The Amazon S3 storage locations.
    storage :: Storage,
    -- | The time of the most recent update for the task.
    updateTime :: Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BundleTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundleTaskError', 'bundleTask_bundleTaskError' - If the task fails, a description of the error.
--
-- 'bundleId', 'bundleTask_bundleId' - The ID of the bundle task.
--
-- 'instanceId', 'bundleTask_instanceId' - The ID of the instance associated with this bundle task.
--
-- 'progress', 'bundleTask_progress' - The level of task completion, as a percent (for example, 20%).
--
-- 'startTime', 'bundleTask_startTime' - The time this task started.
--
-- 'state', 'bundleTask_state' - The state of the task.
--
-- 'storage', 'bundleTask_storage' - The Amazon S3 storage locations.
--
-- 'updateTime', 'bundleTask_updateTime' - The time of the most recent update for the task.
newBundleTask ::
  -- | 'bundleId'
  Core.Text ->
  -- | 'instanceId'
  Core.Text ->
  -- | 'progress'
  Core.Text ->
  -- | 'startTime'
  Core.UTCTime ->
  -- | 'state'
  BundleTaskState ->
  -- | 'storage'
  Storage ->
  -- | 'updateTime'
  Core.UTCTime ->
  BundleTask
newBundleTask
  pBundleId_
  pInstanceId_
  pProgress_
  pStartTime_
  pState_
  pStorage_
  pUpdateTime_ =
    BundleTask'
      { bundleTaskError = Core.Nothing,
        bundleId = pBundleId_,
        instanceId = pInstanceId_,
        progress = pProgress_,
        startTime = Core._Time Lens.# pStartTime_,
        state = pState_,
        storage = pStorage_,
        updateTime = Core._Time Lens.# pUpdateTime_
      }

-- | If the task fails, a description of the error.
bundleTask_bundleTaskError :: Lens.Lens' BundleTask (Core.Maybe BundleTaskError)
bundleTask_bundleTaskError = Lens.lens (\BundleTask' {bundleTaskError} -> bundleTaskError) (\s@BundleTask' {} a -> s {bundleTaskError = a} :: BundleTask)

-- | The ID of the bundle task.
bundleTask_bundleId :: Lens.Lens' BundleTask Core.Text
bundleTask_bundleId = Lens.lens (\BundleTask' {bundleId} -> bundleId) (\s@BundleTask' {} a -> s {bundleId = a} :: BundleTask)

-- | The ID of the instance associated with this bundle task.
bundleTask_instanceId :: Lens.Lens' BundleTask Core.Text
bundleTask_instanceId = Lens.lens (\BundleTask' {instanceId} -> instanceId) (\s@BundleTask' {} a -> s {instanceId = a} :: BundleTask)

-- | The level of task completion, as a percent (for example, 20%).
bundleTask_progress :: Lens.Lens' BundleTask Core.Text
bundleTask_progress = Lens.lens (\BundleTask' {progress} -> progress) (\s@BundleTask' {} a -> s {progress = a} :: BundleTask)

-- | The time this task started.
bundleTask_startTime :: Lens.Lens' BundleTask Core.UTCTime
bundleTask_startTime = Lens.lens (\BundleTask' {startTime} -> startTime) (\s@BundleTask' {} a -> s {startTime = a} :: BundleTask) Core.. Core._Time

-- | The state of the task.
bundleTask_state :: Lens.Lens' BundleTask BundleTaskState
bundleTask_state = Lens.lens (\BundleTask' {state} -> state) (\s@BundleTask' {} a -> s {state = a} :: BundleTask)

-- | The Amazon S3 storage locations.
bundleTask_storage :: Lens.Lens' BundleTask Storage
bundleTask_storage = Lens.lens (\BundleTask' {storage} -> storage) (\s@BundleTask' {} a -> s {storage = a} :: BundleTask)

-- | The time of the most recent update for the task.
bundleTask_updateTime :: Lens.Lens' BundleTask Core.UTCTime
bundleTask_updateTime = Lens.lens (\BundleTask' {updateTime} -> updateTime) (\s@BundleTask' {} a -> s {updateTime = a} :: BundleTask) Core.. Core._Time

instance Core.FromXML BundleTask where
  parseXML x =
    BundleTask'
      Core.<$> (x Core..@? "error")
      Core.<*> (x Core..@ "bundleId")
      Core.<*> (x Core..@ "instanceId")
      Core.<*> (x Core..@ "progress")
      Core.<*> (x Core..@ "startTime")
      Core.<*> (x Core..@ "state")
      Core.<*> (x Core..@ "storage")
      Core.<*> (x Core..@ "updateTime")

instance Core.Hashable BundleTask

instance Core.NFData BundleTask
