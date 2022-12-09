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
-- Module      : Amazonka.Batch.Types.AttemptContainerDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.AttemptContainerDetail where

import Amazonka.Batch.Types.NetworkInterface
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the details of a container that\'s part of a
-- job attempt.
--
-- /See:/ 'newAttemptContainerDetail' smart constructor.
data AttemptContainerDetail = AttemptContainerDetail'
  { -- | The Amazon Resource Name (ARN) of the Amazon ECS container instance that
    -- hosts the job attempt.
    containerInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The exit code for the job attempt. A non-zero exit code is considered
    -- failed.
    exitCode :: Prelude.Maybe Prelude.Int,
    -- | The name of the CloudWatch Logs log stream that\'s associated with the
    -- container. The log group for Batch jobs is @\/aws\/batch\/job@. Each
    -- container attempt receives a log stream name when they reach the
    -- @RUNNING@ status.
    logStreamName :: Prelude.Maybe Prelude.Text,
    -- | The network interfaces that are associated with the job attempt.
    networkInterfaces :: Prelude.Maybe [NetworkInterface],
    -- | A short (255 max characters) human-readable string to provide additional
    -- details for a running or stopped container.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon ECS task that\'s associated
    -- with the job attempt. Each container attempt receives a task ARN when
    -- they reach the @STARTING@ status.
    taskArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttemptContainerDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerInstanceArn', 'attemptContainerDetail_containerInstanceArn' - The Amazon Resource Name (ARN) of the Amazon ECS container instance that
-- hosts the job attempt.
--
-- 'exitCode', 'attemptContainerDetail_exitCode' - The exit code for the job attempt. A non-zero exit code is considered
-- failed.
--
-- 'logStreamName', 'attemptContainerDetail_logStreamName' - The name of the CloudWatch Logs log stream that\'s associated with the
-- container. The log group for Batch jobs is @\/aws\/batch\/job@. Each
-- container attempt receives a log stream name when they reach the
-- @RUNNING@ status.
--
-- 'networkInterfaces', 'attemptContainerDetail_networkInterfaces' - The network interfaces that are associated with the job attempt.
--
-- 'reason', 'attemptContainerDetail_reason' - A short (255 max characters) human-readable string to provide additional
-- details for a running or stopped container.
--
-- 'taskArn', 'attemptContainerDetail_taskArn' - The Amazon Resource Name (ARN) of the Amazon ECS task that\'s associated
-- with the job attempt. Each container attempt receives a task ARN when
-- they reach the @STARTING@ status.
newAttemptContainerDetail ::
  AttemptContainerDetail
newAttemptContainerDetail =
  AttemptContainerDetail'
    { containerInstanceArn =
        Prelude.Nothing,
      exitCode = Prelude.Nothing,
      logStreamName = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      reason = Prelude.Nothing,
      taskArn = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon ECS container instance that
-- hosts the job attempt.
attemptContainerDetail_containerInstanceArn :: Lens.Lens' AttemptContainerDetail (Prelude.Maybe Prelude.Text)
attemptContainerDetail_containerInstanceArn = Lens.lens (\AttemptContainerDetail' {containerInstanceArn} -> containerInstanceArn) (\s@AttemptContainerDetail' {} a -> s {containerInstanceArn = a} :: AttemptContainerDetail)

-- | The exit code for the job attempt. A non-zero exit code is considered
-- failed.
attemptContainerDetail_exitCode :: Lens.Lens' AttemptContainerDetail (Prelude.Maybe Prelude.Int)
attemptContainerDetail_exitCode = Lens.lens (\AttemptContainerDetail' {exitCode} -> exitCode) (\s@AttemptContainerDetail' {} a -> s {exitCode = a} :: AttemptContainerDetail)

-- | The name of the CloudWatch Logs log stream that\'s associated with the
-- container. The log group for Batch jobs is @\/aws\/batch\/job@. Each
-- container attempt receives a log stream name when they reach the
-- @RUNNING@ status.
attemptContainerDetail_logStreamName :: Lens.Lens' AttemptContainerDetail (Prelude.Maybe Prelude.Text)
attemptContainerDetail_logStreamName = Lens.lens (\AttemptContainerDetail' {logStreamName} -> logStreamName) (\s@AttemptContainerDetail' {} a -> s {logStreamName = a} :: AttemptContainerDetail)

-- | The network interfaces that are associated with the job attempt.
attemptContainerDetail_networkInterfaces :: Lens.Lens' AttemptContainerDetail (Prelude.Maybe [NetworkInterface])
attemptContainerDetail_networkInterfaces = Lens.lens (\AttemptContainerDetail' {networkInterfaces} -> networkInterfaces) (\s@AttemptContainerDetail' {} a -> s {networkInterfaces = a} :: AttemptContainerDetail) Prelude.. Lens.mapping Lens.coerced

-- | A short (255 max characters) human-readable string to provide additional
-- details for a running or stopped container.
attemptContainerDetail_reason :: Lens.Lens' AttemptContainerDetail (Prelude.Maybe Prelude.Text)
attemptContainerDetail_reason = Lens.lens (\AttemptContainerDetail' {reason} -> reason) (\s@AttemptContainerDetail' {} a -> s {reason = a} :: AttemptContainerDetail)

-- | The Amazon Resource Name (ARN) of the Amazon ECS task that\'s associated
-- with the job attempt. Each container attempt receives a task ARN when
-- they reach the @STARTING@ status.
attemptContainerDetail_taskArn :: Lens.Lens' AttemptContainerDetail (Prelude.Maybe Prelude.Text)
attemptContainerDetail_taskArn = Lens.lens (\AttemptContainerDetail' {taskArn} -> taskArn) (\s@AttemptContainerDetail' {} a -> s {taskArn = a} :: AttemptContainerDetail)

instance Data.FromJSON AttemptContainerDetail where
  parseJSON =
    Data.withObject
      "AttemptContainerDetail"
      ( \x ->
          AttemptContainerDetail'
            Prelude.<$> (x Data..:? "containerInstanceArn")
            Prelude.<*> (x Data..:? "exitCode")
            Prelude.<*> (x Data..:? "logStreamName")
            Prelude.<*> ( x Data..:? "networkInterfaces"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "reason")
            Prelude.<*> (x Data..:? "taskArn")
      )

instance Prelude.Hashable AttemptContainerDetail where
  hashWithSalt _salt AttemptContainerDetail' {..} =
    _salt `Prelude.hashWithSalt` containerInstanceArn
      `Prelude.hashWithSalt` exitCode
      `Prelude.hashWithSalt` logStreamName
      `Prelude.hashWithSalt` networkInterfaces
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` taskArn

instance Prelude.NFData AttemptContainerDetail where
  rnf AttemptContainerDetail' {..} =
    Prelude.rnf containerInstanceArn
      `Prelude.seq` Prelude.rnf exitCode
      `Prelude.seq` Prelude.rnf logStreamName
      `Prelude.seq` Prelude.rnf networkInterfaces
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf taskArn
