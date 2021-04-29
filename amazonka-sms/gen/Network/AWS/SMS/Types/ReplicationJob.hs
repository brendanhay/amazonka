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
-- Module      : Network.AWS.SMS.Types.ReplicationJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ReplicationJob where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    nextReplicationRunStartTime :: Prelude.Maybe Prelude.POSIX,
    -- | The description of the current status of the replication job.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The number of recent AMIs to keep in the customer\'s account for a
    -- replication job. By default, the value is set to zero, meaning that all
    -- AMIs are kept.
    numberOfRecentAmisToKeep :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the replication job should produce encrypted AMIs.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the latest Amazon Machine Image (AMI).
    latestAmiId :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM role to be used by AWS SMS.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the server.
    serverId :: Prelude.Maybe Prelude.Text,
    -- | The state of the replication job.
    state :: Prelude.Maybe ReplicationJobState,
    -- | Information about the replication runs.
    replicationRunList :: Prelude.Maybe [ReplicationRun],
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
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The time between consecutive replication runs, in hours.
    frequency :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether to run the replication job one time.
    runOnce :: Prelude.Maybe Prelude.Bool,
    -- | The description of the replication job.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the replication job.
    replicationJobId :: Prelude.Maybe Prelude.Text,
    -- | The seed replication time.
    seedReplicationTime :: Prelude.Maybe Prelude.POSIX,
    -- | Information about the VM server.
    vmServer :: Prelude.Maybe VmServer,
    -- | The license type to be used for the AMI created by a successful
    -- replication run.
    licenseType :: Prelude.Maybe LicenseType,
    -- | The type of server.
    serverType :: Prelude.Maybe ServerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      numberOfRecentAmisToKeep = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      latestAmiId = Prelude.Nothing,
      roleName = Prelude.Nothing,
      serverId = Prelude.Nothing,
      state = Prelude.Nothing,
      replicationRunList = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      frequency = Prelude.Nothing,
      runOnce = Prelude.Nothing,
      description = Prelude.Nothing,
      replicationJobId = Prelude.Nothing,
      seedReplicationTime = Prelude.Nothing,
      vmServer = Prelude.Nothing,
      licenseType = Prelude.Nothing,
      serverType = Prelude.Nothing
    }

-- | The start time of the next replication run.
replicationJob_nextReplicationRunStartTime :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.UTCTime)
replicationJob_nextReplicationRunStartTime = Lens.lens (\ReplicationJob' {nextReplicationRunStartTime} -> nextReplicationRunStartTime) (\s@ReplicationJob' {} a -> s {nextReplicationRunStartTime = a} :: ReplicationJob) Prelude.. Lens.mapping Prelude._Time

-- | The description of the current status of the replication job.
replicationJob_statusMessage :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Text)
replicationJob_statusMessage = Lens.lens (\ReplicationJob' {statusMessage} -> statusMessage) (\s@ReplicationJob' {} a -> s {statusMessage = a} :: ReplicationJob)

-- | The number of recent AMIs to keep in the customer\'s account for a
-- replication job. By default, the value is set to zero, meaning that all
-- AMIs are kept.
replicationJob_numberOfRecentAmisToKeep :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Int)
replicationJob_numberOfRecentAmisToKeep = Lens.lens (\ReplicationJob' {numberOfRecentAmisToKeep} -> numberOfRecentAmisToKeep) (\s@ReplicationJob' {} a -> s {numberOfRecentAmisToKeep = a} :: ReplicationJob)

-- | Indicates whether the replication job should produce encrypted AMIs.
replicationJob_encrypted :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Bool)
replicationJob_encrypted = Lens.lens (\ReplicationJob' {encrypted} -> encrypted) (\s@ReplicationJob' {} a -> s {encrypted = a} :: ReplicationJob)

-- | The ID of the latest Amazon Machine Image (AMI).
replicationJob_latestAmiId :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Text)
replicationJob_latestAmiId = Lens.lens (\ReplicationJob' {latestAmiId} -> latestAmiId) (\s@ReplicationJob' {} a -> s {latestAmiId = a} :: ReplicationJob)

-- | The name of the IAM role to be used by AWS SMS.
replicationJob_roleName :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Text)
replicationJob_roleName = Lens.lens (\ReplicationJob' {roleName} -> roleName) (\s@ReplicationJob' {} a -> s {roleName = a} :: ReplicationJob)

-- | The ID of the server.
replicationJob_serverId :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Text)
replicationJob_serverId = Lens.lens (\ReplicationJob' {serverId} -> serverId) (\s@ReplicationJob' {} a -> s {serverId = a} :: ReplicationJob)

-- | The state of the replication job.
replicationJob_state :: Lens.Lens' ReplicationJob (Prelude.Maybe ReplicationJobState)
replicationJob_state = Lens.lens (\ReplicationJob' {state} -> state) (\s@ReplicationJob' {} a -> s {state = a} :: ReplicationJob)

-- | Information about the replication runs.
replicationJob_replicationRunList :: Lens.Lens' ReplicationJob (Prelude.Maybe [ReplicationRun])
replicationJob_replicationRunList = Lens.lens (\ReplicationJob' {replicationRunList} -> replicationRunList) (\s@ReplicationJob' {} a -> s {replicationRunList = a} :: ReplicationJob) Prelude.. Lens.mapping Prelude._Coerce

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
replicationJob_kmsKeyId :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Text)
replicationJob_kmsKeyId = Lens.lens (\ReplicationJob' {kmsKeyId} -> kmsKeyId) (\s@ReplicationJob' {} a -> s {kmsKeyId = a} :: ReplicationJob)

-- | The time between consecutive replication runs, in hours.
replicationJob_frequency :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Int)
replicationJob_frequency = Lens.lens (\ReplicationJob' {frequency} -> frequency) (\s@ReplicationJob' {} a -> s {frequency = a} :: ReplicationJob)

-- | Indicates whether to run the replication job one time.
replicationJob_runOnce :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Bool)
replicationJob_runOnce = Lens.lens (\ReplicationJob' {runOnce} -> runOnce) (\s@ReplicationJob' {} a -> s {runOnce = a} :: ReplicationJob)

-- | The description of the replication job.
replicationJob_description :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Text)
replicationJob_description = Lens.lens (\ReplicationJob' {description} -> description) (\s@ReplicationJob' {} a -> s {description = a} :: ReplicationJob)

-- | The ID of the replication job.
replicationJob_replicationJobId :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Text)
replicationJob_replicationJobId = Lens.lens (\ReplicationJob' {replicationJobId} -> replicationJobId) (\s@ReplicationJob' {} a -> s {replicationJobId = a} :: ReplicationJob)

-- | The seed replication time.
replicationJob_seedReplicationTime :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.UTCTime)
replicationJob_seedReplicationTime = Lens.lens (\ReplicationJob' {seedReplicationTime} -> seedReplicationTime) (\s@ReplicationJob' {} a -> s {seedReplicationTime = a} :: ReplicationJob) Prelude.. Lens.mapping Prelude._Time

-- | Information about the VM server.
replicationJob_vmServer :: Lens.Lens' ReplicationJob (Prelude.Maybe VmServer)
replicationJob_vmServer = Lens.lens (\ReplicationJob' {vmServer} -> vmServer) (\s@ReplicationJob' {} a -> s {vmServer = a} :: ReplicationJob)

-- | The license type to be used for the AMI created by a successful
-- replication run.
replicationJob_licenseType :: Lens.Lens' ReplicationJob (Prelude.Maybe LicenseType)
replicationJob_licenseType = Lens.lens (\ReplicationJob' {licenseType} -> licenseType) (\s@ReplicationJob' {} a -> s {licenseType = a} :: ReplicationJob)

-- | The type of server.
replicationJob_serverType :: Lens.Lens' ReplicationJob (Prelude.Maybe ServerType)
replicationJob_serverType = Lens.lens (\ReplicationJob' {serverType} -> serverType) (\s@ReplicationJob' {} a -> s {serverType = a} :: ReplicationJob)

instance Prelude.FromJSON ReplicationJob where
  parseJSON =
    Prelude.withObject
      "ReplicationJob"
      ( \x ->
          ReplicationJob'
            Prelude.<$> (x Prelude..:? "nextReplicationRunStartTime")
            Prelude.<*> (x Prelude..:? "statusMessage")
            Prelude.<*> (x Prelude..:? "numberOfRecentAmisToKeep")
            Prelude.<*> (x Prelude..:? "encrypted")
            Prelude.<*> (x Prelude..:? "latestAmiId")
            Prelude.<*> (x Prelude..:? "roleName")
            Prelude.<*> (x Prelude..:? "serverId")
            Prelude.<*> (x Prelude..:? "state")
            Prelude.<*> ( x Prelude..:? "replicationRunList"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "kmsKeyId")
            Prelude.<*> (x Prelude..:? "frequency")
            Prelude.<*> (x Prelude..:? "runOnce")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "replicationJobId")
            Prelude.<*> (x Prelude..:? "seedReplicationTime")
            Prelude.<*> (x Prelude..:? "vmServer")
            Prelude.<*> (x Prelude..:? "licenseType")
            Prelude.<*> (x Prelude..:? "serverType")
      )

instance Prelude.Hashable ReplicationJob

instance Prelude.NFData ReplicationJob
