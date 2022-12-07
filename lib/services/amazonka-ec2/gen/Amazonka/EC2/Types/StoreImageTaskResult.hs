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
-- Module      : Amazonka.EC2.Types.StoreImageTaskResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.StoreImageTaskResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The information about the AMI store task, including the progress of the
-- task.
--
-- /See:/ 'newStoreImageTaskResult' smart constructor.
data StoreImageTaskResult = StoreImageTaskResult'
  { -- | The ID of the AMI that is being stored.
    amiId :: Prelude.Maybe Prelude.Text,
    -- | The time the task started.
    taskStartTime :: Prelude.Maybe Data.ISO8601,
    -- | The name of the Amazon S3 bucket that contains the stored AMI object.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | If the tasks fails, the reason for the failure is returned. If the task
    -- succeeds, @null@ is returned.
    storeTaskFailureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the stored AMI object in the bucket.
    s3objectKey :: Prelude.Maybe Prelude.Text,
    -- | The state of the store task (@InProgress@, @Completed@, or @Failed@).
    storeTaskState :: Prelude.Maybe Prelude.Text,
    -- | The progress of the task as a percentage.
    progressPercentage :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StoreImageTaskResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amiId', 'storeImageTaskResult_amiId' - The ID of the AMI that is being stored.
--
-- 'taskStartTime', 'storeImageTaskResult_taskStartTime' - The time the task started.
--
-- 'bucket', 'storeImageTaskResult_bucket' - The name of the Amazon S3 bucket that contains the stored AMI object.
--
-- 'storeTaskFailureReason', 'storeImageTaskResult_storeTaskFailureReason' - If the tasks fails, the reason for the failure is returned. If the task
-- succeeds, @null@ is returned.
--
-- 's3objectKey', 'storeImageTaskResult_s3objectKey' - The name of the stored AMI object in the bucket.
--
-- 'storeTaskState', 'storeImageTaskResult_storeTaskState' - The state of the store task (@InProgress@, @Completed@, or @Failed@).
--
-- 'progressPercentage', 'storeImageTaskResult_progressPercentage' - The progress of the task as a percentage.
newStoreImageTaskResult ::
  StoreImageTaskResult
newStoreImageTaskResult =
  StoreImageTaskResult'
    { amiId = Prelude.Nothing,
      taskStartTime = Prelude.Nothing,
      bucket = Prelude.Nothing,
      storeTaskFailureReason = Prelude.Nothing,
      s3objectKey = Prelude.Nothing,
      storeTaskState = Prelude.Nothing,
      progressPercentage = Prelude.Nothing
    }

-- | The ID of the AMI that is being stored.
storeImageTaskResult_amiId :: Lens.Lens' StoreImageTaskResult (Prelude.Maybe Prelude.Text)
storeImageTaskResult_amiId = Lens.lens (\StoreImageTaskResult' {amiId} -> amiId) (\s@StoreImageTaskResult' {} a -> s {amiId = a} :: StoreImageTaskResult)

-- | The time the task started.
storeImageTaskResult_taskStartTime :: Lens.Lens' StoreImageTaskResult (Prelude.Maybe Prelude.UTCTime)
storeImageTaskResult_taskStartTime = Lens.lens (\StoreImageTaskResult' {taskStartTime} -> taskStartTime) (\s@StoreImageTaskResult' {} a -> s {taskStartTime = a} :: StoreImageTaskResult) Prelude.. Lens.mapping Data._Time

-- | The name of the Amazon S3 bucket that contains the stored AMI object.
storeImageTaskResult_bucket :: Lens.Lens' StoreImageTaskResult (Prelude.Maybe Prelude.Text)
storeImageTaskResult_bucket = Lens.lens (\StoreImageTaskResult' {bucket} -> bucket) (\s@StoreImageTaskResult' {} a -> s {bucket = a} :: StoreImageTaskResult)

-- | If the tasks fails, the reason for the failure is returned. If the task
-- succeeds, @null@ is returned.
storeImageTaskResult_storeTaskFailureReason :: Lens.Lens' StoreImageTaskResult (Prelude.Maybe Prelude.Text)
storeImageTaskResult_storeTaskFailureReason = Lens.lens (\StoreImageTaskResult' {storeTaskFailureReason} -> storeTaskFailureReason) (\s@StoreImageTaskResult' {} a -> s {storeTaskFailureReason = a} :: StoreImageTaskResult)

-- | The name of the stored AMI object in the bucket.
storeImageTaskResult_s3objectKey :: Lens.Lens' StoreImageTaskResult (Prelude.Maybe Prelude.Text)
storeImageTaskResult_s3objectKey = Lens.lens (\StoreImageTaskResult' {s3objectKey} -> s3objectKey) (\s@StoreImageTaskResult' {} a -> s {s3objectKey = a} :: StoreImageTaskResult)

-- | The state of the store task (@InProgress@, @Completed@, or @Failed@).
storeImageTaskResult_storeTaskState :: Lens.Lens' StoreImageTaskResult (Prelude.Maybe Prelude.Text)
storeImageTaskResult_storeTaskState = Lens.lens (\StoreImageTaskResult' {storeTaskState} -> storeTaskState) (\s@StoreImageTaskResult' {} a -> s {storeTaskState = a} :: StoreImageTaskResult)

-- | The progress of the task as a percentage.
storeImageTaskResult_progressPercentage :: Lens.Lens' StoreImageTaskResult (Prelude.Maybe Prelude.Int)
storeImageTaskResult_progressPercentage = Lens.lens (\StoreImageTaskResult' {progressPercentage} -> progressPercentage) (\s@StoreImageTaskResult' {} a -> s {progressPercentage = a} :: StoreImageTaskResult)

instance Data.FromXML StoreImageTaskResult where
  parseXML x =
    StoreImageTaskResult'
      Prelude.<$> (x Data..@? "amiId")
      Prelude.<*> (x Data..@? "taskStartTime")
      Prelude.<*> (x Data..@? "bucket")
      Prelude.<*> (x Data..@? "storeTaskFailureReason")
      Prelude.<*> (x Data..@? "s3objectKey")
      Prelude.<*> (x Data..@? "storeTaskState")
      Prelude.<*> (x Data..@? "progressPercentage")

instance Prelude.Hashable StoreImageTaskResult where
  hashWithSalt _salt StoreImageTaskResult' {..} =
    _salt `Prelude.hashWithSalt` amiId
      `Prelude.hashWithSalt` taskStartTime
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` storeTaskFailureReason
      `Prelude.hashWithSalt` s3objectKey
      `Prelude.hashWithSalt` storeTaskState
      `Prelude.hashWithSalt` progressPercentage

instance Prelude.NFData StoreImageTaskResult where
  rnf StoreImageTaskResult' {..} =
    Prelude.rnf amiId
      `Prelude.seq` Prelude.rnf taskStartTime
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf storeTaskFailureReason
      `Prelude.seq` Prelude.rnf s3objectKey
      `Prelude.seq` Prelude.rnf storeTaskState
      `Prelude.seq` Prelude.rnf progressPercentage
