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
-- Module      : Amazonka.EC2.Types.BundleTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.BundleTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.BundleTaskError
import Amazonka.EC2.Types.BundleTaskState
import Amazonka.EC2.Types.Storage
import qualified Amazonka.Prelude as Prelude

-- | Describes a bundle task.
--
-- /See:/ 'newBundleTask' smart constructor.
data BundleTask = BundleTask'
  { -- | If the task fails, a description of the error.
    bundleTaskError :: Prelude.Maybe BundleTaskError,
    -- | The ID of the bundle task.
    bundleId :: Prelude.Text,
    -- | The ID of the instance associated with this bundle task.
    instanceId :: Prelude.Text,
    -- | The level of task completion, as a percent (for example, 20%).
    progress :: Prelude.Text,
    -- | The time this task started.
    startTime :: Data.ISO8601,
    -- | The state of the task.
    state :: BundleTaskState,
    -- | The Amazon S3 storage locations.
    storage :: Storage,
    -- | The time of the most recent update for the task.
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'progress'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'state'
  BundleTaskState ->
  -- | 'storage'
  Storage ->
  -- | 'updateTime'
  Prelude.UTCTime ->
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
      { bundleTaskError = Prelude.Nothing,
        bundleId = pBundleId_,
        instanceId = pInstanceId_,
        progress = pProgress_,
        startTime = Data._Time Lens.# pStartTime_,
        state = pState_,
        storage = pStorage_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | If the task fails, a description of the error.
bundleTask_bundleTaskError :: Lens.Lens' BundleTask (Prelude.Maybe BundleTaskError)
bundleTask_bundleTaskError = Lens.lens (\BundleTask' {bundleTaskError} -> bundleTaskError) (\s@BundleTask' {} a -> s {bundleTaskError = a} :: BundleTask)

-- | The ID of the bundle task.
bundleTask_bundleId :: Lens.Lens' BundleTask Prelude.Text
bundleTask_bundleId = Lens.lens (\BundleTask' {bundleId} -> bundleId) (\s@BundleTask' {} a -> s {bundleId = a} :: BundleTask)

-- | The ID of the instance associated with this bundle task.
bundleTask_instanceId :: Lens.Lens' BundleTask Prelude.Text
bundleTask_instanceId = Lens.lens (\BundleTask' {instanceId} -> instanceId) (\s@BundleTask' {} a -> s {instanceId = a} :: BundleTask)

-- | The level of task completion, as a percent (for example, 20%).
bundleTask_progress :: Lens.Lens' BundleTask Prelude.Text
bundleTask_progress = Lens.lens (\BundleTask' {progress} -> progress) (\s@BundleTask' {} a -> s {progress = a} :: BundleTask)

-- | The time this task started.
bundleTask_startTime :: Lens.Lens' BundleTask Prelude.UTCTime
bundleTask_startTime = Lens.lens (\BundleTask' {startTime} -> startTime) (\s@BundleTask' {} a -> s {startTime = a} :: BundleTask) Prelude.. Data._Time

-- | The state of the task.
bundleTask_state :: Lens.Lens' BundleTask BundleTaskState
bundleTask_state = Lens.lens (\BundleTask' {state} -> state) (\s@BundleTask' {} a -> s {state = a} :: BundleTask)

-- | The Amazon S3 storage locations.
bundleTask_storage :: Lens.Lens' BundleTask Storage
bundleTask_storage = Lens.lens (\BundleTask' {storage} -> storage) (\s@BundleTask' {} a -> s {storage = a} :: BundleTask)

-- | The time of the most recent update for the task.
bundleTask_updateTime :: Lens.Lens' BundleTask Prelude.UTCTime
bundleTask_updateTime = Lens.lens (\BundleTask' {updateTime} -> updateTime) (\s@BundleTask' {} a -> s {updateTime = a} :: BundleTask) Prelude.. Data._Time

instance Data.FromXML BundleTask where
  parseXML x =
    BundleTask'
      Prelude.<$> (x Data..@? "error")
      Prelude.<*> (x Data..@ "bundleId")
      Prelude.<*> (x Data..@ "instanceId")
      Prelude.<*> (x Data..@ "progress")
      Prelude.<*> (x Data..@ "startTime")
      Prelude.<*> (x Data..@ "state")
      Prelude.<*> (x Data..@ "storage")
      Prelude.<*> (x Data..@ "updateTime")

instance Prelude.Hashable BundleTask where
  hashWithSalt _salt BundleTask' {..} =
    _salt
      `Prelude.hashWithSalt` bundleTaskError
      `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` progress
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` storage
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData BundleTask where
  rnf BundleTask' {..} =
    Prelude.rnf bundleTaskError `Prelude.seq`
      Prelude.rnf bundleId `Prelude.seq`
        Prelude.rnf instanceId `Prelude.seq`
          Prelude.rnf progress `Prelude.seq`
            Prelude.rnf startTime `Prelude.seq`
              Prelude.rnf state `Prelude.seq`
                Prelude.rnf storage `Prelude.seq`
                  Prelude.rnf updateTime
