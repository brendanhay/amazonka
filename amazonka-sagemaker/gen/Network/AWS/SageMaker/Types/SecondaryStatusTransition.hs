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
-- Module      : Network.AWS.SageMaker.Types.SecondaryStatusTransition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SecondaryStatusTransition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.SecondaryStatus

-- | An array element of
-- DescribeTrainingJobResponse$SecondaryStatusTransitions. It provides
-- additional details about a status that the training job has transitioned
-- through. A training job can be in one of several states, for example,
-- starting, downloading, training, or uploading. Within each state, there
-- are a number of intermediate states. For example, within the starting
-- state, Amazon SageMaker could be starting the training job or launching
-- the ML instances. These transitional states are referred to as the
-- job\'s secondary status.
--
-- /See:/ 'newSecondaryStatusTransition' smart constructor.
data SecondaryStatusTransition = SecondaryStatusTransition'
  { -- | A detailed description of the progress within a secondary status.
    --
    -- Amazon SageMaker provides secondary statuses and status messages that
    -- apply to each of them:
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
    -- @TrainingJobStatus@ and @SecondaryStatus@ in DescribeTrainingJob, and
    -- @StatusMessage@ together. For example, at the start of a training job,
    -- you might see the following:
    --
    -- -   @TrainingJobStatus@ - InProgress
    --
    -- -   @SecondaryStatus@ - Training
    --
    -- -   @StatusMessage@ - Downloading the training image
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that shows when the training job transitioned out of this
    -- secondary status state into another secondary status state or when the
    -- training job has ended.
    endTime :: Prelude.Maybe Prelude.POSIX,
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
    startTime :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SecondaryStatusTransition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'secondaryStatusTransition_statusMessage' - A detailed description of the progress within a secondary status.
--
-- Amazon SageMaker provides secondary statuses and status messages that
-- apply to each of them:
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
-- @TrainingJobStatus@ and @SecondaryStatus@ in DescribeTrainingJob, and
-- @StatusMessage@ together. For example, at the start of a training job,
-- you might see the following:
--
-- -   @TrainingJobStatus@ - InProgress
--
-- -   @SecondaryStatus@ - Training
--
-- -   @StatusMessage@ - Downloading the training image
--
-- 'endTime', 'secondaryStatusTransition_endTime' - A timestamp that shows when the training job transitioned out of this
-- secondary status state into another secondary status state or when the
-- training job has ended.
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
    { statusMessage =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      status = pStatus_,
      startTime = Prelude._Time Lens.# pStartTime_
    }

-- | A detailed description of the progress within a secondary status.
--
-- Amazon SageMaker provides secondary statuses and status messages that
-- apply to each of them:
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
-- @TrainingJobStatus@ and @SecondaryStatus@ in DescribeTrainingJob, and
-- @StatusMessage@ together. For example, at the start of a training job,
-- you might see the following:
--
-- -   @TrainingJobStatus@ - InProgress
--
-- -   @SecondaryStatus@ - Training
--
-- -   @StatusMessage@ - Downloading the training image
secondaryStatusTransition_statusMessage :: Lens.Lens' SecondaryStatusTransition (Prelude.Maybe Prelude.Text)
secondaryStatusTransition_statusMessage = Lens.lens (\SecondaryStatusTransition' {statusMessage} -> statusMessage) (\s@SecondaryStatusTransition' {} a -> s {statusMessage = a} :: SecondaryStatusTransition)

-- | A timestamp that shows when the training job transitioned out of this
-- secondary status state into another secondary status state or when the
-- training job has ended.
secondaryStatusTransition_endTime :: Lens.Lens' SecondaryStatusTransition (Prelude.Maybe Prelude.UTCTime)
secondaryStatusTransition_endTime = Lens.lens (\SecondaryStatusTransition' {endTime} -> endTime) (\s@SecondaryStatusTransition' {} a -> s {endTime = a} :: SecondaryStatusTransition) Prelude.. Lens.mapping Prelude._Time

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
secondaryStatusTransition_startTime = Lens.lens (\SecondaryStatusTransition' {startTime} -> startTime) (\s@SecondaryStatusTransition' {} a -> s {startTime = a} :: SecondaryStatusTransition) Prelude.. Prelude._Time

instance Prelude.FromJSON SecondaryStatusTransition where
  parseJSON =
    Prelude.withObject
      "SecondaryStatusTransition"
      ( \x ->
          SecondaryStatusTransition'
            Prelude.<$> (x Prelude..:? "StatusMessage")
            Prelude.<*> (x Prelude..:? "EndTime")
            Prelude.<*> (x Prelude..: "Status")
            Prelude.<*> (x Prelude..: "StartTime")
      )

instance Prelude.Hashable SecondaryStatusTransition

instance Prelude.NFData SecondaryStatusTransition
