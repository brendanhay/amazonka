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
-- Module      : Network.AWS.Batch.Types.AttemptContainerDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.AttemptContainerDetail where

import Network.AWS.Batch.Types.NetworkInterface
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing the details of a container that\'s part of a job
-- attempt.
--
-- /See:/ 'newAttemptContainerDetail' smart constructor.
data AttemptContainerDetail = AttemptContainerDetail'
  { -- | The name of the CloudWatch Logs log stream associated with the
    -- container. The log group for AWS Batch jobs is @\/aws\/batch\/job@. Each
    -- container attempt receives a log stream name when they reach the
    -- @RUNNING@ status.
    logStreamName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon ECS container instance that
    -- hosts the job attempt.
    containerInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The exit code for the job attempt. A non-zero exit code is considered a
    -- failure.
    exitCode :: Prelude.Maybe Prelude.Int,
    -- | A short (255 max characters) human-readable string to provide additional
    -- details about a running or stopped container.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon ECS task that\'s associated
    -- with the job attempt. Each container attempt receives a task ARN when
    -- they reach the @STARTING@ status.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The network interfaces associated with the job attempt.
    networkInterfaces :: Prelude.Maybe [NetworkInterface]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttemptContainerDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreamName', 'attemptContainerDetail_logStreamName' - The name of the CloudWatch Logs log stream associated with the
-- container. The log group for AWS Batch jobs is @\/aws\/batch\/job@. Each
-- container attempt receives a log stream name when they reach the
-- @RUNNING@ status.
--
-- 'containerInstanceArn', 'attemptContainerDetail_containerInstanceArn' - The Amazon Resource Name (ARN) of the Amazon ECS container instance that
-- hosts the job attempt.
--
-- 'exitCode', 'attemptContainerDetail_exitCode' - The exit code for the job attempt. A non-zero exit code is considered a
-- failure.
--
-- 'reason', 'attemptContainerDetail_reason' - A short (255 max characters) human-readable string to provide additional
-- details about a running or stopped container.
--
-- 'taskArn', 'attemptContainerDetail_taskArn' - The Amazon Resource Name (ARN) of the Amazon ECS task that\'s associated
-- with the job attempt. Each container attempt receives a task ARN when
-- they reach the @STARTING@ status.
--
-- 'networkInterfaces', 'attemptContainerDetail_networkInterfaces' - The network interfaces associated with the job attempt.
newAttemptContainerDetail ::
  AttemptContainerDetail
newAttemptContainerDetail =
  AttemptContainerDetail'
    { logStreamName =
        Prelude.Nothing,
      containerInstanceArn = Prelude.Nothing,
      exitCode = Prelude.Nothing,
      reason = Prelude.Nothing,
      taskArn = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing
    }

-- | The name of the CloudWatch Logs log stream associated with the
-- container. The log group for AWS Batch jobs is @\/aws\/batch\/job@. Each
-- container attempt receives a log stream name when they reach the
-- @RUNNING@ status.
attemptContainerDetail_logStreamName :: Lens.Lens' AttemptContainerDetail (Prelude.Maybe Prelude.Text)
attemptContainerDetail_logStreamName = Lens.lens (\AttemptContainerDetail' {logStreamName} -> logStreamName) (\s@AttemptContainerDetail' {} a -> s {logStreamName = a} :: AttemptContainerDetail)

-- | The Amazon Resource Name (ARN) of the Amazon ECS container instance that
-- hosts the job attempt.
attemptContainerDetail_containerInstanceArn :: Lens.Lens' AttemptContainerDetail (Prelude.Maybe Prelude.Text)
attemptContainerDetail_containerInstanceArn = Lens.lens (\AttemptContainerDetail' {containerInstanceArn} -> containerInstanceArn) (\s@AttemptContainerDetail' {} a -> s {containerInstanceArn = a} :: AttemptContainerDetail)

-- | The exit code for the job attempt. A non-zero exit code is considered a
-- failure.
attemptContainerDetail_exitCode :: Lens.Lens' AttemptContainerDetail (Prelude.Maybe Prelude.Int)
attemptContainerDetail_exitCode = Lens.lens (\AttemptContainerDetail' {exitCode} -> exitCode) (\s@AttemptContainerDetail' {} a -> s {exitCode = a} :: AttemptContainerDetail)

-- | A short (255 max characters) human-readable string to provide additional
-- details about a running or stopped container.
attemptContainerDetail_reason :: Lens.Lens' AttemptContainerDetail (Prelude.Maybe Prelude.Text)
attemptContainerDetail_reason = Lens.lens (\AttemptContainerDetail' {reason} -> reason) (\s@AttemptContainerDetail' {} a -> s {reason = a} :: AttemptContainerDetail)

-- | The Amazon Resource Name (ARN) of the Amazon ECS task that\'s associated
-- with the job attempt. Each container attempt receives a task ARN when
-- they reach the @STARTING@ status.
attemptContainerDetail_taskArn :: Lens.Lens' AttemptContainerDetail (Prelude.Maybe Prelude.Text)
attemptContainerDetail_taskArn = Lens.lens (\AttemptContainerDetail' {taskArn} -> taskArn) (\s@AttemptContainerDetail' {} a -> s {taskArn = a} :: AttemptContainerDetail)

-- | The network interfaces associated with the job attempt.
attemptContainerDetail_networkInterfaces :: Lens.Lens' AttemptContainerDetail (Prelude.Maybe [NetworkInterface])
attemptContainerDetail_networkInterfaces = Lens.lens (\AttemptContainerDetail' {networkInterfaces} -> networkInterfaces) (\s@AttemptContainerDetail' {} a -> s {networkInterfaces = a} :: AttemptContainerDetail) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON AttemptContainerDetail where
  parseJSON =
    Prelude.withObject
      "AttemptContainerDetail"
      ( \x ->
          AttemptContainerDetail'
            Prelude.<$> (x Prelude..:? "logStreamName")
            Prelude.<*> (x Prelude..:? "containerInstanceArn")
            Prelude.<*> (x Prelude..:? "exitCode")
            Prelude.<*> (x Prelude..:? "reason")
            Prelude.<*> (x Prelude..:? "taskArn")
            Prelude.<*> ( x Prelude..:? "networkInterfaces"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AttemptContainerDetail

instance Prelude.NFData AttemptContainerDetail
