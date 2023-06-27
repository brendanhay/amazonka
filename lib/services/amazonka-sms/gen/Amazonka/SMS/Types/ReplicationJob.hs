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
-- Module      : Amazonka.SMS.Types.ReplicationJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ReplicationJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.LicenseType
import Amazonka.SMS.Types.ReplicationJobState
import Amazonka.SMS.Types.ReplicationRun
import Amazonka.SMS.Types.ServerType
import Amazonka.SMS.Types.VmServer

-- | Represents a replication job.
--
-- /See:/ 'newReplicationJob' smart constructor.
data ReplicationJob = ReplicationJob'
  { -- | The description of the replication job.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the replication job should produce encrypted AMIs.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The time between consecutive replication runs, in hours.
    frequency :: Prelude.Maybe Prelude.Int,
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
    -- | The ID of the latest Amazon Machine Image (AMI).
    latestAmiId :: Prelude.Maybe Prelude.Text,
    -- | The license type to be used for the AMI created by a successful
    -- replication run.
    licenseType :: Prelude.Maybe LicenseType,
    -- | The start time of the next replication run.
    nextReplicationRunStartTime :: Prelude.Maybe Data.POSIX,
    -- | The number of recent AMIs to keep in the customer\'s account for a
    -- replication job. By default, the value is set to zero, meaning that all
    -- AMIs are kept.
    numberOfRecentAmisToKeep :: Prelude.Maybe Prelude.Int,
    -- | The ID of the replication job.
    replicationJobId :: Prelude.Maybe Prelude.Text,
    -- | Information about the replication runs.
    replicationRunList :: Prelude.Maybe [ReplicationRun],
    -- | The name of the IAM role to be used by Server Migration Service.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to run the replication job one time.
    runOnce :: Prelude.Maybe Prelude.Bool,
    -- | The seed replication time.
    seedReplicationTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the server.
    serverId :: Prelude.Maybe Prelude.Text,
    -- | The type of server.
    serverType :: Prelude.Maybe ServerType,
    -- | The state of the replication job.
    state :: Prelude.Maybe ReplicationJobState,
    -- | The description of the current status of the replication job.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Information about the VM server.
    vmServer :: Prelude.Maybe VmServer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'replicationJob_description' - The description of the replication job.
--
-- 'encrypted', 'replicationJob_encrypted' - Indicates whether the replication job should produce encrypted AMIs.
--
-- 'frequency', 'replicationJob_frequency' - The time between consecutive replication runs, in hours.
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
-- 'latestAmiId', 'replicationJob_latestAmiId' - The ID of the latest Amazon Machine Image (AMI).
--
-- 'licenseType', 'replicationJob_licenseType' - The license type to be used for the AMI created by a successful
-- replication run.
--
-- 'nextReplicationRunStartTime', 'replicationJob_nextReplicationRunStartTime' - The start time of the next replication run.
--
-- 'numberOfRecentAmisToKeep', 'replicationJob_numberOfRecentAmisToKeep' - The number of recent AMIs to keep in the customer\'s account for a
-- replication job. By default, the value is set to zero, meaning that all
-- AMIs are kept.
--
-- 'replicationJobId', 'replicationJob_replicationJobId' - The ID of the replication job.
--
-- 'replicationRunList', 'replicationJob_replicationRunList' - Information about the replication runs.
--
-- 'roleName', 'replicationJob_roleName' - The name of the IAM role to be used by Server Migration Service.
--
-- 'runOnce', 'replicationJob_runOnce' - Indicates whether to run the replication job one time.
--
-- 'seedReplicationTime', 'replicationJob_seedReplicationTime' - The seed replication time.
--
-- 'serverId', 'replicationJob_serverId' - The ID of the server.
--
-- 'serverType', 'replicationJob_serverType' - The type of server.
--
-- 'state', 'replicationJob_state' - The state of the replication job.
--
-- 'statusMessage', 'replicationJob_statusMessage' - The description of the current status of the replication job.
--
-- 'vmServer', 'replicationJob_vmServer' - Information about the VM server.
newReplicationJob ::
  ReplicationJob
newReplicationJob =
  ReplicationJob'
    { description = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      frequency = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      latestAmiId = Prelude.Nothing,
      licenseType = Prelude.Nothing,
      nextReplicationRunStartTime = Prelude.Nothing,
      numberOfRecentAmisToKeep = Prelude.Nothing,
      replicationJobId = Prelude.Nothing,
      replicationRunList = Prelude.Nothing,
      roleName = Prelude.Nothing,
      runOnce = Prelude.Nothing,
      seedReplicationTime = Prelude.Nothing,
      serverId = Prelude.Nothing,
      serverType = Prelude.Nothing,
      state = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      vmServer = Prelude.Nothing
    }

-- | The description of the replication job.
replicationJob_description :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Text)
replicationJob_description = Lens.lens (\ReplicationJob' {description} -> description) (\s@ReplicationJob' {} a -> s {description = a} :: ReplicationJob)

-- | Indicates whether the replication job should produce encrypted AMIs.
replicationJob_encrypted :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Bool)
replicationJob_encrypted = Lens.lens (\ReplicationJob' {encrypted} -> encrypted) (\s@ReplicationJob' {} a -> s {encrypted = a} :: ReplicationJob)

-- | The time between consecutive replication runs, in hours.
replicationJob_frequency :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Int)
replicationJob_frequency = Lens.lens (\ReplicationJob' {frequency} -> frequency) (\s@ReplicationJob' {} a -> s {frequency = a} :: ReplicationJob)

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

-- | The ID of the latest Amazon Machine Image (AMI).
replicationJob_latestAmiId :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Text)
replicationJob_latestAmiId = Lens.lens (\ReplicationJob' {latestAmiId} -> latestAmiId) (\s@ReplicationJob' {} a -> s {latestAmiId = a} :: ReplicationJob)

-- | The license type to be used for the AMI created by a successful
-- replication run.
replicationJob_licenseType :: Lens.Lens' ReplicationJob (Prelude.Maybe LicenseType)
replicationJob_licenseType = Lens.lens (\ReplicationJob' {licenseType} -> licenseType) (\s@ReplicationJob' {} a -> s {licenseType = a} :: ReplicationJob)

-- | The start time of the next replication run.
replicationJob_nextReplicationRunStartTime :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.UTCTime)
replicationJob_nextReplicationRunStartTime = Lens.lens (\ReplicationJob' {nextReplicationRunStartTime} -> nextReplicationRunStartTime) (\s@ReplicationJob' {} a -> s {nextReplicationRunStartTime = a} :: ReplicationJob) Prelude.. Lens.mapping Data._Time

-- | The number of recent AMIs to keep in the customer\'s account for a
-- replication job. By default, the value is set to zero, meaning that all
-- AMIs are kept.
replicationJob_numberOfRecentAmisToKeep :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Int)
replicationJob_numberOfRecentAmisToKeep = Lens.lens (\ReplicationJob' {numberOfRecentAmisToKeep} -> numberOfRecentAmisToKeep) (\s@ReplicationJob' {} a -> s {numberOfRecentAmisToKeep = a} :: ReplicationJob)

-- | The ID of the replication job.
replicationJob_replicationJobId :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Text)
replicationJob_replicationJobId = Lens.lens (\ReplicationJob' {replicationJobId} -> replicationJobId) (\s@ReplicationJob' {} a -> s {replicationJobId = a} :: ReplicationJob)

-- | Information about the replication runs.
replicationJob_replicationRunList :: Lens.Lens' ReplicationJob (Prelude.Maybe [ReplicationRun])
replicationJob_replicationRunList = Lens.lens (\ReplicationJob' {replicationRunList} -> replicationRunList) (\s@ReplicationJob' {} a -> s {replicationRunList = a} :: ReplicationJob) Prelude.. Lens.mapping Lens.coerced

-- | The name of the IAM role to be used by Server Migration Service.
replicationJob_roleName :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Text)
replicationJob_roleName = Lens.lens (\ReplicationJob' {roleName} -> roleName) (\s@ReplicationJob' {} a -> s {roleName = a} :: ReplicationJob)

-- | Indicates whether to run the replication job one time.
replicationJob_runOnce :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Bool)
replicationJob_runOnce = Lens.lens (\ReplicationJob' {runOnce} -> runOnce) (\s@ReplicationJob' {} a -> s {runOnce = a} :: ReplicationJob)

-- | The seed replication time.
replicationJob_seedReplicationTime :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.UTCTime)
replicationJob_seedReplicationTime = Lens.lens (\ReplicationJob' {seedReplicationTime} -> seedReplicationTime) (\s@ReplicationJob' {} a -> s {seedReplicationTime = a} :: ReplicationJob) Prelude.. Lens.mapping Data._Time

-- | The ID of the server.
replicationJob_serverId :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Text)
replicationJob_serverId = Lens.lens (\ReplicationJob' {serverId} -> serverId) (\s@ReplicationJob' {} a -> s {serverId = a} :: ReplicationJob)

-- | The type of server.
replicationJob_serverType :: Lens.Lens' ReplicationJob (Prelude.Maybe ServerType)
replicationJob_serverType = Lens.lens (\ReplicationJob' {serverType} -> serverType) (\s@ReplicationJob' {} a -> s {serverType = a} :: ReplicationJob)

-- | The state of the replication job.
replicationJob_state :: Lens.Lens' ReplicationJob (Prelude.Maybe ReplicationJobState)
replicationJob_state = Lens.lens (\ReplicationJob' {state} -> state) (\s@ReplicationJob' {} a -> s {state = a} :: ReplicationJob)

-- | The description of the current status of the replication job.
replicationJob_statusMessage :: Lens.Lens' ReplicationJob (Prelude.Maybe Prelude.Text)
replicationJob_statusMessage = Lens.lens (\ReplicationJob' {statusMessage} -> statusMessage) (\s@ReplicationJob' {} a -> s {statusMessage = a} :: ReplicationJob)

-- | Information about the VM server.
replicationJob_vmServer :: Lens.Lens' ReplicationJob (Prelude.Maybe VmServer)
replicationJob_vmServer = Lens.lens (\ReplicationJob' {vmServer} -> vmServer) (\s@ReplicationJob' {} a -> s {vmServer = a} :: ReplicationJob)

instance Data.FromJSON ReplicationJob where
  parseJSON =
    Data.withObject
      "ReplicationJob"
      ( \x ->
          ReplicationJob'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "encrypted")
            Prelude.<*> (x Data..:? "frequency")
            Prelude.<*> (x Data..:? "kmsKeyId")
            Prelude.<*> (x Data..:? "latestAmiId")
            Prelude.<*> (x Data..:? "licenseType")
            Prelude.<*> (x Data..:? "nextReplicationRunStartTime")
            Prelude.<*> (x Data..:? "numberOfRecentAmisToKeep")
            Prelude.<*> (x Data..:? "replicationJobId")
            Prelude.<*> ( x
                            Data..:? "replicationRunList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "roleName")
            Prelude.<*> (x Data..:? "runOnce")
            Prelude.<*> (x Data..:? "seedReplicationTime")
            Prelude.<*> (x Data..:? "serverId")
            Prelude.<*> (x Data..:? "serverType")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..:? "vmServer")
      )

instance Prelude.Hashable ReplicationJob where
  hashWithSalt _salt ReplicationJob' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` frequency
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` latestAmiId
      `Prelude.hashWithSalt` licenseType
      `Prelude.hashWithSalt` nextReplicationRunStartTime
      `Prelude.hashWithSalt` numberOfRecentAmisToKeep
      `Prelude.hashWithSalt` replicationJobId
      `Prelude.hashWithSalt` replicationRunList
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` runOnce
      `Prelude.hashWithSalt` seedReplicationTime
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` serverType
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` vmServer

instance Prelude.NFData ReplicationJob where
  rnf ReplicationJob' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf frequency
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf latestAmiId
      `Prelude.seq` Prelude.rnf licenseType
      `Prelude.seq` Prelude.rnf nextReplicationRunStartTime
      `Prelude.seq` Prelude.rnf numberOfRecentAmisToKeep
      `Prelude.seq` Prelude.rnf replicationJobId
      `Prelude.seq` Prelude.rnf replicationRunList
      `Prelude.seq` Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf runOnce
      `Prelude.seq` Prelude.rnf seedReplicationTime
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf serverType
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf vmServer
