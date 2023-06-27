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
-- Module      : Amazonka.SMS.Types.ReplicationRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ReplicationRun where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.ReplicationRunStageDetails
import Amazonka.SMS.Types.ReplicationRunState
import Amazonka.SMS.Types.ReplicationRunType

-- | Represents a replication run.
--
-- /See:/ 'newReplicationRun' smart constructor.
data ReplicationRun = ReplicationRun'
  { -- | The ID of the Amazon Machine Image (AMI) from the replication run.
    amiId :: Prelude.Maybe Prelude.Text,
    -- | The completion time of the last replication run.
    completedTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the replication run.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the replication run should produce an encrypted AMI.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the KMS key for replication jobs that produce encrypted AMIs.
    -- This value can be any of the following:
    --
    -- -   KMS key ID
    --
    -- -   KMS key alias
    --
    -- -   ARN referring to the KMS key ID
    --
    -- -   ARN referring to the KMS key alias
    --
    -- If encrypted is /true/ but a KMS key ID is not specified, the
    -- customer\'s default KMS key for Amazon EBS is used.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the replication run.
    replicationRunId :: Prelude.Maybe Prelude.Text,
    -- | The start time of the next replication run.
    scheduledStartTime :: Prelude.Maybe Data.POSIX,
    -- | Details about the current stage of the replication run.
    stageDetails :: Prelude.Maybe ReplicationRunStageDetails,
    -- | The state of the replication run.
    state :: Prelude.Maybe ReplicationRunState,
    -- | The description of the current status of the replication job.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The type of replication run.
    type' :: Prelude.Maybe ReplicationRunType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amiId', 'replicationRun_amiId' - The ID of the Amazon Machine Image (AMI) from the replication run.
--
-- 'completedTime', 'replicationRun_completedTime' - The completion time of the last replication run.
--
-- 'description', 'replicationRun_description' - The description of the replication run.
--
-- 'encrypted', 'replicationRun_encrypted' - Indicates whether the replication run should produce an encrypted AMI.
--
-- 'kmsKeyId', 'replicationRun_kmsKeyId' - The ID of the KMS key for replication jobs that produce encrypted AMIs.
-- This value can be any of the following:
--
-- -   KMS key ID
--
-- -   KMS key alias
--
-- -   ARN referring to the KMS key ID
--
-- -   ARN referring to the KMS key alias
--
-- If encrypted is /true/ but a KMS key ID is not specified, the
-- customer\'s default KMS key for Amazon EBS is used.
--
-- 'replicationRunId', 'replicationRun_replicationRunId' - The ID of the replication run.
--
-- 'scheduledStartTime', 'replicationRun_scheduledStartTime' - The start time of the next replication run.
--
-- 'stageDetails', 'replicationRun_stageDetails' - Details about the current stage of the replication run.
--
-- 'state', 'replicationRun_state' - The state of the replication run.
--
-- 'statusMessage', 'replicationRun_statusMessage' - The description of the current status of the replication job.
--
-- 'type'', 'replicationRun_type' - The type of replication run.
newReplicationRun ::
  ReplicationRun
newReplicationRun =
  ReplicationRun'
    { amiId = Prelude.Nothing,
      completedTime = Prelude.Nothing,
      description = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      replicationRunId = Prelude.Nothing,
      scheduledStartTime = Prelude.Nothing,
      stageDetails = Prelude.Nothing,
      state = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ID of the Amazon Machine Image (AMI) from the replication run.
replicationRun_amiId :: Lens.Lens' ReplicationRun (Prelude.Maybe Prelude.Text)
replicationRun_amiId = Lens.lens (\ReplicationRun' {amiId} -> amiId) (\s@ReplicationRun' {} a -> s {amiId = a} :: ReplicationRun)

-- | The completion time of the last replication run.
replicationRun_completedTime :: Lens.Lens' ReplicationRun (Prelude.Maybe Prelude.UTCTime)
replicationRun_completedTime = Lens.lens (\ReplicationRun' {completedTime} -> completedTime) (\s@ReplicationRun' {} a -> s {completedTime = a} :: ReplicationRun) Prelude.. Lens.mapping Data._Time

-- | The description of the replication run.
replicationRun_description :: Lens.Lens' ReplicationRun (Prelude.Maybe Prelude.Text)
replicationRun_description = Lens.lens (\ReplicationRun' {description} -> description) (\s@ReplicationRun' {} a -> s {description = a} :: ReplicationRun)

-- | Indicates whether the replication run should produce an encrypted AMI.
replicationRun_encrypted :: Lens.Lens' ReplicationRun (Prelude.Maybe Prelude.Bool)
replicationRun_encrypted = Lens.lens (\ReplicationRun' {encrypted} -> encrypted) (\s@ReplicationRun' {} a -> s {encrypted = a} :: ReplicationRun)

-- | The ID of the KMS key for replication jobs that produce encrypted AMIs.
-- This value can be any of the following:
--
-- -   KMS key ID
--
-- -   KMS key alias
--
-- -   ARN referring to the KMS key ID
--
-- -   ARN referring to the KMS key alias
--
-- If encrypted is /true/ but a KMS key ID is not specified, the
-- customer\'s default KMS key for Amazon EBS is used.
replicationRun_kmsKeyId :: Lens.Lens' ReplicationRun (Prelude.Maybe Prelude.Text)
replicationRun_kmsKeyId = Lens.lens (\ReplicationRun' {kmsKeyId} -> kmsKeyId) (\s@ReplicationRun' {} a -> s {kmsKeyId = a} :: ReplicationRun)

-- | The ID of the replication run.
replicationRun_replicationRunId :: Lens.Lens' ReplicationRun (Prelude.Maybe Prelude.Text)
replicationRun_replicationRunId = Lens.lens (\ReplicationRun' {replicationRunId} -> replicationRunId) (\s@ReplicationRun' {} a -> s {replicationRunId = a} :: ReplicationRun)

-- | The start time of the next replication run.
replicationRun_scheduledStartTime :: Lens.Lens' ReplicationRun (Prelude.Maybe Prelude.UTCTime)
replicationRun_scheduledStartTime = Lens.lens (\ReplicationRun' {scheduledStartTime} -> scheduledStartTime) (\s@ReplicationRun' {} a -> s {scheduledStartTime = a} :: ReplicationRun) Prelude.. Lens.mapping Data._Time

-- | Details about the current stage of the replication run.
replicationRun_stageDetails :: Lens.Lens' ReplicationRun (Prelude.Maybe ReplicationRunStageDetails)
replicationRun_stageDetails = Lens.lens (\ReplicationRun' {stageDetails} -> stageDetails) (\s@ReplicationRun' {} a -> s {stageDetails = a} :: ReplicationRun)

-- | The state of the replication run.
replicationRun_state :: Lens.Lens' ReplicationRun (Prelude.Maybe ReplicationRunState)
replicationRun_state = Lens.lens (\ReplicationRun' {state} -> state) (\s@ReplicationRun' {} a -> s {state = a} :: ReplicationRun)

-- | The description of the current status of the replication job.
replicationRun_statusMessage :: Lens.Lens' ReplicationRun (Prelude.Maybe Prelude.Text)
replicationRun_statusMessage = Lens.lens (\ReplicationRun' {statusMessage} -> statusMessage) (\s@ReplicationRun' {} a -> s {statusMessage = a} :: ReplicationRun)

-- | The type of replication run.
replicationRun_type :: Lens.Lens' ReplicationRun (Prelude.Maybe ReplicationRunType)
replicationRun_type = Lens.lens (\ReplicationRun' {type'} -> type') (\s@ReplicationRun' {} a -> s {type' = a} :: ReplicationRun)

instance Data.FromJSON ReplicationRun where
  parseJSON =
    Data.withObject
      "ReplicationRun"
      ( \x ->
          ReplicationRun'
            Prelude.<$> (x Data..:? "amiId")
            Prelude.<*> (x Data..:? "completedTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "encrypted")
            Prelude.<*> (x Data..:? "kmsKeyId")
            Prelude.<*> (x Data..:? "replicationRunId")
            Prelude.<*> (x Data..:? "scheduledStartTime")
            Prelude.<*> (x Data..:? "stageDetails")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable ReplicationRun where
  hashWithSalt _salt ReplicationRun' {..} =
    _salt
      `Prelude.hashWithSalt` amiId
      `Prelude.hashWithSalt` completedTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` replicationRunId
      `Prelude.hashWithSalt` scheduledStartTime
      `Prelude.hashWithSalt` stageDetails
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ReplicationRun where
  rnf ReplicationRun' {..} =
    Prelude.rnf amiId
      `Prelude.seq` Prelude.rnf completedTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf replicationRunId
      `Prelude.seq` Prelude.rnf scheduledStartTime
      `Prelude.seq` Prelude.rnf stageDetails
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf type'
