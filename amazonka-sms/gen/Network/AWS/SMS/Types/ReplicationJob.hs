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
-- Module      : Network.AWS.SMS.Types.ReplicationJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ReplicationJob where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SMS.Types.LicenseType
import Network.AWS.SMS.Types.ReplicationJobState
import Network.AWS.SMS.Types.ReplicationRun
import Network.AWS.SMS.Types.ServerType
import Network.AWS.SMS.Types.VmServer

-- | Represents a replication job.
--
-- /See:/ 'newReplicationJob' smart constructor.
data ReplicationJob = ReplicationJob'
  { -- | The start time of the next replication run.
    nextReplicationRunStartTime :: Core.Maybe Core.POSIX,
    -- | The description of the current status of the replication job.
    statusMessage :: Core.Maybe Core.Text,
    -- | The number of recent AMIs to keep in the customer\'s account for a
    -- replication job. By default, the value is set to zero, meaning that all
    -- AMIs are kept.
    numberOfRecentAmisToKeep :: Core.Maybe Core.Int,
    -- | Indicates whether the replication job should produce encrypted AMIs.
    encrypted :: Core.Maybe Core.Bool,
    -- | The ID of the latest Amazon Machine Image (AMI).
    latestAmiId :: Core.Maybe Core.Text,
    -- | The name of the IAM role to be used by AWS SMS.
    roleName :: Core.Maybe Core.Text,
    -- | The ID of the server.
    serverId :: Core.Maybe Core.Text,
    -- | The state of the replication job.
    state :: Core.Maybe ReplicationJobState,
    -- | Information about the replication runs.
    replicationRunList :: Core.Maybe [ReplicationRun],
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
    -- If encrypted is enabled but a KMS key ID is not specified, the
    -- customer\'s default KMS key for Amazon EBS is used.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The time between consecutive replication runs, in hours.
    frequency :: Core.Maybe Core.Int,
    -- | Indicates whether to run the replication job one time.
    runOnce :: Core.Maybe Core.Bool,
    -- | The description of the replication job.
    description :: Core.Maybe Core.Text,
    -- | The ID of the replication job.
    replicationJobId :: Core.Maybe Core.Text,
    -- | The seed replication time.
    seedReplicationTime :: Core.Maybe Core.POSIX,
    -- | Information about the VM server.
    vmServer :: Core.Maybe VmServer,
    -- | The license type to be used for the AMI created by a successful
    -- replication run.
    licenseType :: Core.Maybe LicenseType,
    -- | The type of server.
    serverType :: Core.Maybe ServerType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextReplicationRunStartTime', 'replicationJob_nextReplicationRunStartTime' - The start time of the next replication run.
--
-- 'statusMessage', 'replicationJob_statusMessage' - The description of the current status of the replication job.
--
-- 'numberOfRecentAmisToKeep', 'replicationJob_numberOfRecentAmisToKeep' - The number of recent AMIs to keep in the customer\'s account for a
-- replication job. By default, the value is set to zero, meaning that all
-- AMIs are kept.
--
-- 'encrypted', 'replicationJob_encrypted' - Indicates whether the replication job should produce encrypted AMIs.
--
-- 'latestAmiId', 'replicationJob_latestAmiId' - The ID of the latest Amazon Machine Image (AMI).
--
-- 'roleName', 'replicationJob_roleName' - The name of the IAM role to be used by AWS SMS.
--
-- 'serverId', 'replicationJob_serverId' - The ID of the server.
--
-- 'state', 'replicationJob_state' - The state of the replication job.
--
-- 'replicationRunList', 'replicationJob_replicationRunList' - Information about the replication runs.
--
-- 'kmsKeyId', 'replicationJob_kmsKeyId' - The ID of the KMS key for replication jobs that produce encrypted AMIs.
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
-- If encrypted is enabled but a KMS key ID is not specified, the
-- customer\'s default KMS key for Amazon EBS is used.
--
-- 'frequency', 'replicationJob_frequency' - The time between consecutive replication runs, in hours.
--
-- 'runOnce', 'replicationJob_runOnce' - Indicates whether to run the replication job one time.
--
-- 'description', 'replicationJob_description' - The description of the replication job.
--
-- 'replicationJobId', 'replicationJob_replicationJobId' - The ID of the replication job.
--
-- 'seedReplicationTime', 'replicationJob_seedReplicationTime' - The seed replication time.
--
-- 'vmServer', 'replicationJob_vmServer' - Information about the VM server.
--
-- 'licenseType', 'replicationJob_licenseType' - The license type to be used for the AMI created by a successful
-- replication run.
--
-- 'serverType', 'replicationJob_serverType' - The type of server.
newReplicationJob ::
  ReplicationJob
newReplicationJob =
  ReplicationJob'
    { nextReplicationRunStartTime =
        Core.Nothing,
      statusMessage = Core.Nothing,
      numberOfRecentAmisToKeep = Core.Nothing,
      encrypted = Core.Nothing,
      latestAmiId = Core.Nothing,
      roleName = Core.Nothing,
      serverId = Core.Nothing,
      state = Core.Nothing,
      replicationRunList = Core.Nothing,
      kmsKeyId = Core.Nothing,
      frequency = Core.Nothing,
      runOnce = Core.Nothing,
      description = Core.Nothing,
      replicationJobId = Core.Nothing,
      seedReplicationTime = Core.Nothing,
      vmServer = Core.Nothing,
      licenseType = Core.Nothing,
      serverType = Core.Nothing
    }

-- | The start time of the next replication run.
replicationJob_nextReplicationRunStartTime :: Lens.Lens' ReplicationJob (Core.Maybe Core.UTCTime)
replicationJob_nextReplicationRunStartTime = Lens.lens (\ReplicationJob' {nextReplicationRunStartTime} -> nextReplicationRunStartTime) (\s@ReplicationJob' {} a -> s {nextReplicationRunStartTime = a} :: ReplicationJob) Core.. Lens.mapping Core._Time

-- | The description of the current status of the replication job.
replicationJob_statusMessage :: Lens.Lens' ReplicationJob (Core.Maybe Core.Text)
replicationJob_statusMessage = Lens.lens (\ReplicationJob' {statusMessage} -> statusMessage) (\s@ReplicationJob' {} a -> s {statusMessage = a} :: ReplicationJob)

-- | The number of recent AMIs to keep in the customer\'s account for a
-- replication job. By default, the value is set to zero, meaning that all
-- AMIs are kept.
replicationJob_numberOfRecentAmisToKeep :: Lens.Lens' ReplicationJob (Core.Maybe Core.Int)
replicationJob_numberOfRecentAmisToKeep = Lens.lens (\ReplicationJob' {numberOfRecentAmisToKeep} -> numberOfRecentAmisToKeep) (\s@ReplicationJob' {} a -> s {numberOfRecentAmisToKeep = a} :: ReplicationJob)

-- | Indicates whether the replication job should produce encrypted AMIs.
replicationJob_encrypted :: Lens.Lens' ReplicationJob (Core.Maybe Core.Bool)
replicationJob_encrypted = Lens.lens (\ReplicationJob' {encrypted} -> encrypted) (\s@ReplicationJob' {} a -> s {encrypted = a} :: ReplicationJob)

-- | The ID of the latest Amazon Machine Image (AMI).
replicationJob_latestAmiId :: Lens.Lens' ReplicationJob (Core.Maybe Core.Text)
replicationJob_latestAmiId = Lens.lens (\ReplicationJob' {latestAmiId} -> latestAmiId) (\s@ReplicationJob' {} a -> s {latestAmiId = a} :: ReplicationJob)

-- | The name of the IAM role to be used by AWS SMS.
replicationJob_roleName :: Lens.Lens' ReplicationJob (Core.Maybe Core.Text)
replicationJob_roleName = Lens.lens (\ReplicationJob' {roleName} -> roleName) (\s@ReplicationJob' {} a -> s {roleName = a} :: ReplicationJob)

-- | The ID of the server.
replicationJob_serverId :: Lens.Lens' ReplicationJob (Core.Maybe Core.Text)
replicationJob_serverId = Lens.lens (\ReplicationJob' {serverId} -> serverId) (\s@ReplicationJob' {} a -> s {serverId = a} :: ReplicationJob)

-- | The state of the replication job.
replicationJob_state :: Lens.Lens' ReplicationJob (Core.Maybe ReplicationJobState)
replicationJob_state = Lens.lens (\ReplicationJob' {state} -> state) (\s@ReplicationJob' {} a -> s {state = a} :: ReplicationJob)

-- | Information about the replication runs.
replicationJob_replicationRunList :: Lens.Lens' ReplicationJob (Core.Maybe [ReplicationRun])
replicationJob_replicationRunList = Lens.lens (\ReplicationJob' {replicationRunList} -> replicationRunList) (\s@ReplicationJob' {} a -> s {replicationRunList = a} :: ReplicationJob) Core.. Lens.mapping Lens._Coerce

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
-- If encrypted is enabled but a KMS key ID is not specified, the
-- customer\'s default KMS key for Amazon EBS is used.
replicationJob_kmsKeyId :: Lens.Lens' ReplicationJob (Core.Maybe Core.Text)
replicationJob_kmsKeyId = Lens.lens (\ReplicationJob' {kmsKeyId} -> kmsKeyId) (\s@ReplicationJob' {} a -> s {kmsKeyId = a} :: ReplicationJob)

-- | The time between consecutive replication runs, in hours.
replicationJob_frequency :: Lens.Lens' ReplicationJob (Core.Maybe Core.Int)
replicationJob_frequency = Lens.lens (\ReplicationJob' {frequency} -> frequency) (\s@ReplicationJob' {} a -> s {frequency = a} :: ReplicationJob)

-- | Indicates whether to run the replication job one time.
replicationJob_runOnce :: Lens.Lens' ReplicationJob (Core.Maybe Core.Bool)
replicationJob_runOnce = Lens.lens (\ReplicationJob' {runOnce} -> runOnce) (\s@ReplicationJob' {} a -> s {runOnce = a} :: ReplicationJob)

-- | The description of the replication job.
replicationJob_description :: Lens.Lens' ReplicationJob (Core.Maybe Core.Text)
replicationJob_description = Lens.lens (\ReplicationJob' {description} -> description) (\s@ReplicationJob' {} a -> s {description = a} :: ReplicationJob)

-- | The ID of the replication job.
replicationJob_replicationJobId :: Lens.Lens' ReplicationJob (Core.Maybe Core.Text)
replicationJob_replicationJobId = Lens.lens (\ReplicationJob' {replicationJobId} -> replicationJobId) (\s@ReplicationJob' {} a -> s {replicationJobId = a} :: ReplicationJob)

-- | The seed replication time.
replicationJob_seedReplicationTime :: Lens.Lens' ReplicationJob (Core.Maybe Core.UTCTime)
replicationJob_seedReplicationTime = Lens.lens (\ReplicationJob' {seedReplicationTime} -> seedReplicationTime) (\s@ReplicationJob' {} a -> s {seedReplicationTime = a} :: ReplicationJob) Core.. Lens.mapping Core._Time

-- | Information about the VM server.
replicationJob_vmServer :: Lens.Lens' ReplicationJob (Core.Maybe VmServer)
replicationJob_vmServer = Lens.lens (\ReplicationJob' {vmServer} -> vmServer) (\s@ReplicationJob' {} a -> s {vmServer = a} :: ReplicationJob)

-- | The license type to be used for the AMI created by a successful
-- replication run.
replicationJob_licenseType :: Lens.Lens' ReplicationJob (Core.Maybe LicenseType)
replicationJob_licenseType = Lens.lens (\ReplicationJob' {licenseType} -> licenseType) (\s@ReplicationJob' {} a -> s {licenseType = a} :: ReplicationJob)

-- | The type of server.
replicationJob_serverType :: Lens.Lens' ReplicationJob (Core.Maybe ServerType)
replicationJob_serverType = Lens.lens (\ReplicationJob' {serverType} -> serverType) (\s@ReplicationJob' {} a -> s {serverType = a} :: ReplicationJob)

instance Core.FromJSON ReplicationJob where
  parseJSON =
    Core.withObject
      "ReplicationJob"
      ( \x ->
          ReplicationJob'
            Core.<$> (x Core..:? "nextReplicationRunStartTime")
            Core.<*> (x Core..:? "statusMessage")
            Core.<*> (x Core..:? "numberOfRecentAmisToKeep")
            Core.<*> (x Core..:? "encrypted")
            Core.<*> (x Core..:? "latestAmiId")
            Core.<*> (x Core..:? "roleName")
            Core.<*> (x Core..:? "serverId")
            Core.<*> (x Core..:? "state")
            Core.<*> ( x Core..:? "replicationRunList"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "kmsKeyId")
            Core.<*> (x Core..:? "frequency")
            Core.<*> (x Core..:? "runOnce")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "replicationJobId")
            Core.<*> (x Core..:? "seedReplicationTime")
            Core.<*> (x Core..:? "vmServer")
            Core.<*> (x Core..:? "licenseType")
            Core.<*> (x Core..:? "serverType")
      )

instance Core.Hashable ReplicationJob

instance Core.NFData ReplicationJob
