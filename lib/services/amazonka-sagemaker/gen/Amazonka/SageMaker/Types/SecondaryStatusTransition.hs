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
-- Module      : Amazonka.SageMaker.Types.SecondaryStatusTransition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.SecondaryStatusTransition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.SecondaryStatus

-- | An array element of @SecondaryStatusTransitions@ for
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DescribeTrainingJob.html DescribeTrainingJob>.
-- It provides additional details about a status that the training job has
-- transitioned through. A training job can be in one of several states,
-- for example, starting, downloading, training, or uploading. Within each
-- state, there are a number of intermediate states. For example, within
-- the starting state, SageMaker could be starting the training job or
-- launching the ML instances. These transitional states are referred to as
-- the job\'s secondary status.
--
-- /See:/ 'newSecondaryStatusTransition' smart constructor.
data SecondaryStatusTransition = SecondaryStatusTransition'
  { -- | A timestamp that shows when the training job transitioned out of this
    -- secondary status state into another secondary status state or when the
    -- training job has ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | A detailed description of the progress within a secondary status.
    --
    -- SageMaker provides secondary statuses and status messages that apply to
    -- each of them:
    --
    -- [Starting]
    --     -   Starting the training job.
    --
    --     -   Launching requested ML instances.
    --
    --     -   Insufficient capacity error from EC2 while launching instances,
    --         retrying!
    --
    --     -   Launched instance was unhealthy, replacing it!
    --
    --     -   Preparing the instances for training.
    --
    -- [Training]
    --     -   Downloading the training image.
    --
    --     -   Training image download completed. Training in progress.
    --
    -- Status messages are subject to change. Therefore, we recommend not
    -- including them in code that programmatically initiates actions. For
    -- examples, don\'t use status messages in if statements.
    --
    -- To have an overview of your training job\'s progress, view
    -- @TrainingJobStatus@ and @SecondaryStatus@ in
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DescribeTrainingJob.html DescribeTrainingJob>,
    -- and @StatusMessage@ together. For example, at the start of a training
    -- job, you might see the following:
    --
    -- -   @TrainingJobStatus@ - InProgress
    --
    -- -   @SecondaryStatus@ - Training
    --
    -- -   @StatusMessage@ - Downloading the training image
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Contains a secondary status information from a training job.
    --
    -- Status might be one of the following secondary statuses:
    --
    -- [InProgress]
    --     -   @Starting@ - Starting the training job.
    --
    --     -   @Downloading@ - An optional stage for algorithms that support
    --         @File@ training input mode. It indicates that data is being
    --         downloaded to the ML storage volumes.
    --
    --     -   @Training@ - Training is in progress.
    --
    --     -   @Uploading@ - Training is complete and the model artifacts are
    --         being uploaded to the S3 location.
    --
    -- [Completed]
    --     -   @Completed@ - The training job has completed.
    --
    -- [Failed]
    --     -   @Failed@ - The training job has failed. The reason for the
    --         failure is returned in the @FailureReason@ field of
    --         @DescribeTrainingJobResponse@.
    --
    -- [Stopped]
    --     -   @MaxRuntimeExceeded@ - The job stopped because it exceeded the
    --         maximum allowed runtime.
    --
    --     -   @Stopped@ - The training job has stopped.
    --
    -- [Stopping]
    --     -   @Stopping@ - Stopping the training job.
    --
    -- We no longer support the following secondary statuses:
    --
    -- -   @LaunchingMLInstances@
    --
    -- -   @PreparingTrainingStack@
    --
    -- -   @DownloadingTrainingImage@
    status :: SecondaryStatus,
    -- | A timestamp that shows when the training job transitioned to the current
    -- secondary status state.
    startTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecondaryStatusTransition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'secondaryStatusTransition_endTime' - A timestamp that shows when the training job transitioned out of this
-- secondary status state into another secondary status state or when the
-- training job has ended.
--
-- 'statusMessage', 'secondaryStatusTransition_statusMessage' - A detailed description of the progress within a secondary status.
--
-- SageMaker provides secondary statuses and status messages that apply to
-- each of them:
--
-- [Starting]
--     -   Starting the training job.
--
--     -   Launching requested ML instances.
--
--     -   Insufficient capacity error from EC2 while launching instances,
--         retrying!
--
--     -   Launched instance was unhealthy, replacing it!
--
--     -   Preparing the instances for training.
--
-- [Training]
--     -   Downloading the training image.
--
--     -   Training image download completed. Training in progress.
--
-- Status messages are subject to change. Therefore, we recommend not
-- including them in code that programmatically initiates actions. For
-- examples, don\'t use status messages in if statements.
--
-- To have an overview of your training job\'s progress, view
-- @TrainingJobStatus@ and @SecondaryStatus@ in
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DescribeTrainingJob.html DescribeTrainingJob>,
-- and @StatusMessage@ together. For example, at the start of a training
-- job, you might see the following:
--
-- -   @TrainingJobStatus@ - InProgress
--
-- -   @SecondaryStatus@ - Training
--
-- -   @StatusMessage@ - Downloading the training image
--
-- 'status', 'secondaryStatusTransition_status' - Contains a secondary status information from a training job.
--
-- Status might be one of the following secondary statuses:
--
-- [InProgress]
--     -   @Starting@ - Starting the training job.
--
--     -   @Downloading@ - An optional stage for algorithms that support
--         @File@ training input mode. It indicates that data is being
--         downloaded to the ML storage volumes.
--
--     -   @Training@ - Training is in progress.
--
--     -   @Uploading@ - Training is complete and the model artifacts are
--         being uploaded to the S3 location.
--
-- [Completed]
--     -   @Completed@ - The training job has completed.
--
-- [Failed]
--     -   @Failed@ - The training job has failed. The reason for the
--         failure is returned in the @FailureReason@ field of
--         @DescribeTrainingJobResponse@.
--
-- [Stopped]
--     -   @MaxRuntimeExceeded@ - The job stopped because it exceeded the
--         maximum allowed runtime.
--
--     -   @Stopped@ - The training job has stopped.
--
-- [Stopping]
--     -   @Stopping@ - Stopping the training job.
--
-- We no longer support the following secondary statuses:
--
-- -   @LaunchingMLInstances@
--
-- -   @PreparingTrainingStack@
--
-- -   @DownloadingTrainingImage@
--
-- 'startTime', 'secondaryStatusTransition_startTime' - A timestamp that shows when the training job transitioned to the current
-- secondary status state.
newSecondaryStatusTransition ::
  -- | 'status'
  SecondaryStatus ->
  -- | 'startTime'
  Prelude.UTCTime ->
  SecondaryStatusTransition
newSecondaryStatusTransition pStatus_ pStartTime_ =
  SecondaryStatusTransition'
    { endTime =
        Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      status = pStatus_,
      startTime = Data._Time Lens.# pStartTime_
    }

-- | A timestamp that shows when the training job transitioned out of this
-- secondary status state into another secondary status state or when the
-- training job has ended.
secondaryStatusTransition_endTime :: Lens.Lens' SecondaryStatusTransition (Prelude.Maybe Prelude.UTCTime)
secondaryStatusTransition_endTime = Lens.lens (\SecondaryStatusTransition' {endTime} -> endTime) (\s@SecondaryStatusTransition' {} a -> s {endTime = a} :: SecondaryStatusTransition) Prelude.. Lens.mapping Data._Time

-- | A detailed description of the progress within a secondary status.
--
-- SageMaker provides secondary statuses and status messages that apply to
-- each of them:
--
-- [Starting]
--     -   Starting the training job.
--
--     -   Launching requested ML instances.
--
--     -   Insufficient capacity error from EC2 while launching instances,
--         retrying!
--
--     -   Launched instance was unhealthy, replacing it!
--
--     -   Preparing the instances for training.
--
-- [Training]
--     -   Downloading the training image.
--
--     -   Training image download completed. Training in progress.
--
-- Status messages are subject to change. Therefore, we recommend not
-- including them in code that programmatically initiates actions. For
-- examples, don\'t use status messages in if statements.
--
-- To have an overview of your training job\'s progress, view
-- @TrainingJobStatus@ and @SecondaryStatus@ in
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DescribeTrainingJob.html DescribeTrainingJob>,
-- and @StatusMessage@ together. For example, at the start of a training
-- job, you might see the following:
--
-- -   @TrainingJobStatus@ - InProgress
--
-- -   @SecondaryStatus@ - Training
--
-- -   @StatusMessage@ - Downloading the training image
secondaryStatusTransition_statusMessage :: Lens.Lens' SecondaryStatusTransition (Prelude.Maybe Prelude.Text)
secondaryStatusTransition_statusMessage = Lens.lens (\SecondaryStatusTransition' {statusMessage} -> statusMessage) (\s@SecondaryStatusTransition' {} a -> s {statusMessage = a} :: SecondaryStatusTransition)

-- | Contains a secondary status information from a training job.
--
-- Status might be one of the following secondary statuses:
--
-- [InProgress]
--     -   @Starting@ - Starting the training job.
--
--     -   @Downloading@ - An optional stage for algorithms that support
--         @File@ training input mode. It indicates that data is being
--         downloaded to the ML storage volumes.
--
--     -   @Training@ - Training is in progress.
--
--     -   @Uploading@ - Training is complete and the model artifacts are
--         being uploaded to the S3 location.
--
-- [Completed]
--     -   @Completed@ - The training job has completed.
--
-- [Failed]
--     -   @Failed@ - The training job has failed. The reason for the
--         failure is returned in the @FailureReason@ field of
--         @DescribeTrainingJobResponse@.
--
-- [Stopped]
--     -   @MaxRuntimeExceeded@ - The job stopped because it exceeded the
--         maximum allowed runtime.
--
--     -   @Stopped@ - The training job has stopped.
--
-- [Stopping]
--     -   @Stopping@ - Stopping the training job.
--
-- We no longer support the following secondary statuses:
--
-- -   @LaunchingMLInstances@
--
-- -   @PreparingTrainingStack@
--
-- -   @DownloadingTrainingImage@
secondaryStatusTransition_status :: Lens.Lens' SecondaryStatusTransition SecondaryStatus
secondaryStatusTransition_status = Lens.lens (\SecondaryStatusTransition' {status} -> status) (\s@SecondaryStatusTransition' {} a -> s {status = a} :: SecondaryStatusTransition)

-- | A timestamp that shows when the training job transitioned to the current
-- secondary status state.
secondaryStatusTransition_startTime :: Lens.Lens' SecondaryStatusTransition Prelude.UTCTime
secondaryStatusTransition_startTime = Lens.lens (\SecondaryStatusTransition' {startTime} -> startTime) (\s@SecondaryStatusTransition' {} a -> s {startTime = a} :: SecondaryStatusTransition) Prelude.. Data._Time

instance Data.FromJSON SecondaryStatusTransition where
  parseJSON =
    Data.withObject
      "SecondaryStatusTransition"
      ( \x ->
          SecondaryStatusTransition'
            Prelude.<$> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "StartTime")
      )

instance Prelude.Hashable SecondaryStatusTransition where
  hashWithSalt _salt SecondaryStatusTransition' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData SecondaryStatusTransition where
  rnf SecondaryStatusTransition' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf startTime
