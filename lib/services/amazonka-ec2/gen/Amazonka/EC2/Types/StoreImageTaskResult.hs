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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.StoreImageTaskResult where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The information about the AMI store task, including the progress of the
-- task.
--
-- /See:/ 'newStoreImageTaskResult' smart constructor.
data StoreImageTaskResult = StoreImageTaskResult'
  { -- | The name of the stored AMI object in the bucket.
    s3objectKey :: Prelude.Maybe Prelude.Text,
    -- | The state of the store task (@InProgress@, @Completed@, or @Failed@).
    storeTaskState :: Prelude.Maybe Prelude.Text,
    -- | The time the task started.
    taskStartTime :: Prelude.Maybe Core.ISO8601,
    -- | The name of the Amazon S3 bucket that contains the stored AMI object.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The progress of the task as a percentage.
    progressPercentage :: Prelude.Maybe Prelude.Int,
    -- | The ID of the AMI that is being stored.
    amiId :: Prelude.Maybe Prelude.Text,
    -- | If the tasks fails, the reason for the failure is returned. If the task
    -- succeeds, @null@ is returned.
    storeTaskFailureReason :: Prelude.Maybe Prelude.Text
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
-- 's3objectKey', 'storeImageTaskResult_s3objectKey' - The name of the stored AMI object in the bucket.
--
-- 'storeTaskState', 'storeImageTaskResult_storeTaskState' - The state of the store task (@InProgress@, @Completed@, or @Failed@).
--
-- 'taskStartTime', 'storeImageTaskResult_taskStartTime' - The time the task started.
--
-- 'bucket', 'storeImageTaskResult_bucket' - The name of the Amazon S3 bucket that contains the stored AMI object.
--
-- 'progressPercentage', 'storeImageTaskResult_progressPercentage' - The progress of the task as a percentage.
--
-- 'amiId', 'storeImageTaskResult_amiId' - The ID of the AMI that is being stored.
--
-- 'storeTaskFailureReason', 'storeImageTaskResult_storeTaskFailureReason' - If the tasks fails, the reason for the failure is returned. If the task
-- succeeds, @null@ is returned.
newStoreImageTaskResult ::
  StoreImageTaskResult
newStoreImageTaskResult =
  StoreImageTaskResult'
    { s3objectKey =
        Prelude.Nothing,
      storeTaskState = Prelude.Nothing,
      taskStartTime = Prelude.Nothing,
      bucket = Prelude.Nothing,
      progressPercentage = Prelude.Nothing,
      amiId = Prelude.Nothing,
      storeTaskFailureReason = Prelude.Nothing
    }

-- | The name of the stored AMI object in the bucket.
storeImageTaskResult_s3objectKey :: Lens.Lens' StoreImageTaskResult (Prelude.Maybe Prelude.Text)
storeImageTaskResult_s3objectKey = Lens.lens (\StoreImageTaskResult' {s3objectKey} -> s3objectKey) (\s@StoreImageTaskResult' {} a -> s {s3objectKey = a} :: StoreImageTaskResult)

-- | The state of the store task (@InProgress@, @Completed@, or @Failed@).
storeImageTaskResult_storeTaskState :: Lens.Lens' StoreImageTaskResult (Prelude.Maybe Prelude.Text)
storeImageTaskResult_storeTaskState = Lens.lens (\StoreImageTaskResult' {storeTaskState} -> storeTaskState) (\s@StoreImageTaskResult' {} a -> s {storeTaskState = a} :: StoreImageTaskResult)

-- | The time the task started.
storeImageTaskResult_taskStartTime :: Lens.Lens' StoreImageTaskResult (Prelude.Maybe Prelude.UTCTime)
storeImageTaskResult_taskStartTime = Lens.lens (\StoreImageTaskResult' {taskStartTime} -> taskStartTime) (\s@StoreImageTaskResult' {} a -> s {taskStartTime = a} :: StoreImageTaskResult) Prelude.. Lens.mapping Core._Time

-- | The name of the Amazon S3 bucket that contains the stored AMI object.
storeImageTaskResult_bucket :: Lens.Lens' StoreImageTaskResult (Prelude.Maybe Prelude.Text)
storeImageTaskResult_bucket = Lens.lens (\StoreImageTaskResult' {bucket} -> bucket) (\s@StoreImageTaskResult' {} a -> s {bucket = a} :: StoreImageTaskResult)

-- | The progress of the task as a percentage.
storeImageTaskResult_progressPercentage :: Lens.Lens' StoreImageTaskResult (Prelude.Maybe Prelude.Int)
storeImageTaskResult_progressPercentage = Lens.lens (\StoreImageTaskResult' {progressPercentage} -> progressPercentage) (\s@StoreImageTaskResult' {} a -> s {progressPercentage = a} :: StoreImageTaskResult)

-- | The ID of the AMI that is being stored.
storeImageTaskResult_amiId :: Lens.Lens' StoreImageTaskResult (Prelude.Maybe Prelude.Text)
storeImageTaskResult_amiId = Lens.lens (\StoreImageTaskResult' {amiId} -> amiId) (\s@StoreImageTaskResult' {} a -> s {amiId = a} :: StoreImageTaskResult)

-- | If the tasks fails, the reason for the failure is returned. If the task
-- succeeds, @null@ is returned.
storeImageTaskResult_storeTaskFailureReason :: Lens.Lens' StoreImageTaskResult (Prelude.Maybe Prelude.Text)
storeImageTaskResult_storeTaskFailureReason = Lens.lens (\StoreImageTaskResult' {storeTaskFailureReason} -> storeTaskFailureReason) (\s@StoreImageTaskResult' {} a -> s {storeTaskFailureReason = a} :: StoreImageTaskResult)

instance Core.FromXML StoreImageTaskResult where
  parseXML x =
    StoreImageTaskResult'
      Prelude.<$> (x Core..@? "s3objectKey")
      Prelude.<*> (x Core..@? "storeTaskState")
      Prelude.<*> (x Core..@? "taskStartTime")
      Prelude.<*> (x Core..@? "bucket")
      Prelude.<*> (x Core..@? "progressPercentage")
      Prelude.<*> (x Core..@? "amiId")
      Prelude.<*> (x Core..@? "storeTaskFailureReason")

instance Prelude.Hashable StoreImageTaskResult where
  hashWithSalt salt' StoreImageTaskResult' {..} =
    salt' `Prelude.hashWithSalt` storeTaskFailureReason
      `Prelude.hashWithSalt` amiId
      `Prelude.hashWithSalt` progressPercentage
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` taskStartTime
      `Prelude.hashWithSalt` storeTaskState
      `Prelude.hashWithSalt` s3objectKey

instance Prelude.NFData StoreImageTaskResult where
  rnf StoreImageTaskResult' {..} =
    Prelude.rnf s3objectKey
      `Prelude.seq` Prelude.rnf storeTaskFailureReason
      `Prelude.seq` Prelude.rnf amiId
      `Prelude.seq` Prelude.rnf progressPercentage
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf taskStartTime
      `Prelude.seq` Prelude.rnf storeTaskState
