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
-- Module      : Network.AWS.SMS.Types.ReplicationRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ReplicationRun where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SMS.Types.ReplicationRunStageDetails
import Network.AWS.SMS.Types.ReplicationRunState
import Network.AWS.SMS.Types.ReplicationRunType

-- | Represents a replication run.
--
-- /See:/ 'newReplicationRun' smart constructor.
data ReplicationRun = ReplicationRun'
  { -- | The description of the current status of the replication job.
    statusMessage :: Core.Maybe Core.Text,
    -- | Indicates whether the replication run should produce an encrypted AMI.
    encrypted :: Core.Maybe Core.Bool,
    -- | The ID of the replication run.
    replicationRunId :: Core.Maybe Core.Text,
    -- | The ID of the Amazon Machine Image (AMI) from the replication run.
    amiId :: Core.Maybe Core.Text,
    -- | The completion time of the last replication run.
    completedTime :: Core.Maybe Core.POSIX,
    -- | The state of the replication run.
    state :: Core.Maybe ReplicationRunState,
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
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The start time of the next replication run.
    scheduledStartTime :: Core.Maybe Core.POSIX,
    -- | Details about the current stage of the replication run.
    stageDetails :: Core.Maybe ReplicationRunStageDetails,
    -- | The description of the replication run.
    description :: Core.Maybe Core.Text,
    -- | The type of replication run.
    type' :: Core.Maybe ReplicationRunType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicationRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'replicationRun_statusMessage' - The description of the current status of the replication job.
--
-- 'encrypted', 'replicationRun_encrypted' - Indicates whether the replication run should produce an encrypted AMI.
--
-- 'replicationRunId', 'replicationRun_replicationRunId' - The ID of the replication run.
--
-- 'amiId', 'replicationRun_amiId' - The ID of the Amazon Machine Image (AMI) from the replication run.
--
-- 'completedTime', 'replicationRun_completedTime' - The completion time of the last replication run.
--
-- 'state', 'replicationRun_state' - The state of the replication run.
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
-- 'scheduledStartTime', 'replicationRun_scheduledStartTime' - The start time of the next replication run.
--
-- 'stageDetails', 'replicationRun_stageDetails' - Details about the current stage of the replication run.
--
-- 'description', 'replicationRun_description' - The description of the replication run.
--
-- 'type'', 'replicationRun_type' - The type of replication run.
newReplicationRun ::
  ReplicationRun
newReplicationRun =
  ReplicationRun'
    { statusMessage = Core.Nothing,
      encrypted = Core.Nothing,
      replicationRunId = Core.Nothing,
      amiId = Core.Nothing,
      completedTime = Core.Nothing,
      state = Core.Nothing,
      kmsKeyId = Core.Nothing,
      scheduledStartTime = Core.Nothing,
      stageDetails = Core.Nothing,
      description = Core.Nothing,
      type' = Core.Nothing
    }

-- | The description of the current status of the replication job.
replicationRun_statusMessage :: Lens.Lens' ReplicationRun (Core.Maybe Core.Text)
replicationRun_statusMessage = Lens.lens (\ReplicationRun' {statusMessage} -> statusMessage) (\s@ReplicationRun' {} a -> s {statusMessage = a} :: ReplicationRun)

-- | Indicates whether the replication run should produce an encrypted AMI.
replicationRun_encrypted :: Lens.Lens' ReplicationRun (Core.Maybe Core.Bool)
replicationRun_encrypted = Lens.lens (\ReplicationRun' {encrypted} -> encrypted) (\s@ReplicationRun' {} a -> s {encrypted = a} :: ReplicationRun)

-- | The ID of the replication run.
replicationRun_replicationRunId :: Lens.Lens' ReplicationRun (Core.Maybe Core.Text)
replicationRun_replicationRunId = Lens.lens (\ReplicationRun' {replicationRunId} -> replicationRunId) (\s@ReplicationRun' {} a -> s {replicationRunId = a} :: ReplicationRun)

-- | The ID of the Amazon Machine Image (AMI) from the replication run.
replicationRun_amiId :: Lens.Lens' ReplicationRun (Core.Maybe Core.Text)
replicationRun_amiId = Lens.lens (\ReplicationRun' {amiId} -> amiId) (\s@ReplicationRun' {} a -> s {amiId = a} :: ReplicationRun)

-- | The completion time of the last replication run.
replicationRun_completedTime :: Lens.Lens' ReplicationRun (Core.Maybe Core.UTCTime)
replicationRun_completedTime = Lens.lens (\ReplicationRun' {completedTime} -> completedTime) (\s@ReplicationRun' {} a -> s {completedTime = a} :: ReplicationRun) Core.. Lens.mapping Core._Time

-- | The state of the replication run.
replicationRun_state :: Lens.Lens' ReplicationRun (Core.Maybe ReplicationRunState)
replicationRun_state = Lens.lens (\ReplicationRun' {state} -> state) (\s@ReplicationRun' {} a -> s {state = a} :: ReplicationRun)

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
replicationRun_kmsKeyId :: Lens.Lens' ReplicationRun (Core.Maybe Core.Text)
replicationRun_kmsKeyId = Lens.lens (\ReplicationRun' {kmsKeyId} -> kmsKeyId) (\s@ReplicationRun' {} a -> s {kmsKeyId = a} :: ReplicationRun)

-- | The start time of the next replication run.
replicationRun_scheduledStartTime :: Lens.Lens' ReplicationRun (Core.Maybe Core.UTCTime)
replicationRun_scheduledStartTime = Lens.lens (\ReplicationRun' {scheduledStartTime} -> scheduledStartTime) (\s@ReplicationRun' {} a -> s {scheduledStartTime = a} :: ReplicationRun) Core.. Lens.mapping Core._Time

-- | Details about the current stage of the replication run.
replicationRun_stageDetails :: Lens.Lens' ReplicationRun (Core.Maybe ReplicationRunStageDetails)
replicationRun_stageDetails = Lens.lens (\ReplicationRun' {stageDetails} -> stageDetails) (\s@ReplicationRun' {} a -> s {stageDetails = a} :: ReplicationRun)

-- | The description of the replication run.
replicationRun_description :: Lens.Lens' ReplicationRun (Core.Maybe Core.Text)
replicationRun_description = Lens.lens (\ReplicationRun' {description} -> description) (\s@ReplicationRun' {} a -> s {description = a} :: ReplicationRun)

-- | The type of replication run.
replicationRun_type :: Lens.Lens' ReplicationRun (Core.Maybe ReplicationRunType)
replicationRun_type = Lens.lens (\ReplicationRun' {type'} -> type') (\s@ReplicationRun' {} a -> s {type' = a} :: ReplicationRun)

instance Core.FromJSON ReplicationRun where
  parseJSON =
    Core.withObject
      "ReplicationRun"
      ( \x ->
          ReplicationRun'
            Core.<$> (x Core..:? "statusMessage")
            Core.<*> (x Core..:? "encrypted")
            Core.<*> (x Core..:? "replicationRunId")
            Core.<*> (x Core..:? "amiId")
            Core.<*> (x Core..:? "completedTime")
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "kmsKeyId")
            Core.<*> (x Core..:? "scheduledStartTime")
            Core.<*> (x Core..:? "stageDetails")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "type")
      )

instance Core.Hashable ReplicationRun

instance Core.NFData ReplicationRun
