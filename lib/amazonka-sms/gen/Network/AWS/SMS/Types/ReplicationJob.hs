-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ReplicationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ReplicationJob
  ( ReplicationJob (..),

    -- * Smart constructor
    mkReplicationJob,

    -- * Lenses
    rjFrequency,
    rjNumberOfRecentAMIsToKeep,
    rjState,
    rjServerType,
    rjServerId,
    rjLicenseType,
    rjRoleName,
    rjVmServer,
    rjEncrypted,
    rjReplicationJobId,
    rjReplicationRunList,
    rjNextReplicationRunStartTime,
    rjStatusMessage,
    rjKmsKeyId,
    rjLatestAMIId,
    rjSeedReplicationTime,
    rjRunOnce,
    rjDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.LicenseType
import Network.AWS.SMS.Types.ReplicationJobState
import Network.AWS.SMS.Types.ReplicationRun
import Network.AWS.SMS.Types.ServerType
import Network.AWS.SMS.Types.VMServer

-- | Represents a replication job.
--
-- /See:/ 'mkReplicationJob' smart constructor.
data ReplicationJob = ReplicationJob'
  { frequency ::
      Lude.Maybe Lude.Int,
    numberOfRecentAMIsToKeep :: Lude.Maybe Lude.Int,
    state :: Lude.Maybe ReplicationJobState,
    serverType :: Lude.Maybe ServerType,
    serverId :: Lude.Maybe Lude.Text,
    licenseType :: Lude.Maybe LicenseType,
    roleName :: Lude.Maybe Lude.Text,
    vmServer :: Lude.Maybe VMServer,
    encrypted :: Lude.Maybe Lude.Bool,
    replicationJobId :: Lude.Maybe Lude.Text,
    replicationRunList :: Lude.Maybe [ReplicationRun],
    nextReplicationRunStartTime :: Lude.Maybe Lude.Timestamp,
    statusMessage :: Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    latestAMIId :: Lude.Maybe Lude.Text,
    seedReplicationTime :: Lude.Maybe Lude.Timestamp,
    runOnce :: Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationJob' with the minimum fields required to make a request.
--
-- * 'description' - The description of the replication job.
-- * 'encrypted' - Indicates whether the replication job should produce encrypted AMIs.
-- * 'frequency' - The time between consecutive replication runs, in hours.
-- * 'kmsKeyId' - The ID of the KMS key for replication jobs that produce encrypted AMIs. This value can be any of the following:
--
--
--     * KMS key ID
--
--
--     * KMS key alias
--
--
--     * ARN referring to the KMS key ID
--
--
--     * ARN referring to the KMS key alias
--
--
-- If encrypted is enabled but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
-- * 'latestAMIId' - The ID of the latest Amazon Machine Image (AMI).
-- * 'licenseType' - The license type to be used for the AMI created by a successful replication run.
-- * 'nextReplicationRunStartTime' - The start time of the next replication run.
-- * 'numberOfRecentAMIsToKeep' - The number of recent AMIs to keep in the customer's account for a replication job. By default, the value is set to zero, meaning that all AMIs are kept.
-- * 'replicationJobId' - The ID of the replication job.
-- * 'replicationRunList' - Information about the replication runs.
-- * 'roleName' - The name of the IAM role to be used by AWS SMS.
-- * 'runOnce' - Indicates whether to run the replication job one time.
-- * 'seedReplicationTime' - The seed replication time.
-- * 'serverId' - The ID of the server.
-- * 'serverType' - The type of server.
-- * 'state' - The state of the replication job.
-- * 'statusMessage' - The description of the current status of the replication job.
-- * 'vmServer' - Information about the VM server.
mkReplicationJob ::
  ReplicationJob
mkReplicationJob =
  ReplicationJob'
    { frequency = Lude.Nothing,
      numberOfRecentAMIsToKeep = Lude.Nothing,
      state = Lude.Nothing,
      serverType = Lude.Nothing,
      serverId = Lude.Nothing,
      licenseType = Lude.Nothing,
      roleName = Lude.Nothing,
      vmServer = Lude.Nothing,
      encrypted = Lude.Nothing,
      replicationJobId = Lude.Nothing,
      replicationRunList = Lude.Nothing,
      nextReplicationRunStartTime = Lude.Nothing,
      statusMessage = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      latestAMIId = Lude.Nothing,
      seedReplicationTime = Lude.Nothing,
      runOnce = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The time between consecutive replication runs, in hours.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjFrequency :: Lens.Lens' ReplicationJob (Lude.Maybe Lude.Int)
rjFrequency = Lens.lens (frequency :: ReplicationJob -> Lude.Maybe Lude.Int) (\s a -> s {frequency = a} :: ReplicationJob)
{-# DEPRECATED rjFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | The number of recent AMIs to keep in the customer's account for a replication job. By default, the value is set to zero, meaning that all AMIs are kept.
--
-- /Note:/ Consider using 'numberOfRecentAMIsToKeep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjNumberOfRecentAMIsToKeep :: Lens.Lens' ReplicationJob (Lude.Maybe Lude.Int)
rjNumberOfRecentAMIsToKeep = Lens.lens (numberOfRecentAMIsToKeep :: ReplicationJob -> Lude.Maybe Lude.Int) (\s a -> s {numberOfRecentAMIsToKeep = a} :: ReplicationJob)
{-# DEPRECATED rjNumberOfRecentAMIsToKeep "Use generic-lens or generic-optics with 'numberOfRecentAMIsToKeep' instead." #-}

-- | The state of the replication job.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjState :: Lens.Lens' ReplicationJob (Lude.Maybe ReplicationJobState)
rjState = Lens.lens (state :: ReplicationJob -> Lude.Maybe ReplicationJobState) (\s a -> s {state = a} :: ReplicationJob)
{-# DEPRECATED rjState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The type of server.
--
-- /Note:/ Consider using 'serverType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjServerType :: Lens.Lens' ReplicationJob (Lude.Maybe ServerType)
rjServerType = Lens.lens (serverType :: ReplicationJob -> Lude.Maybe ServerType) (\s a -> s {serverType = a} :: ReplicationJob)
{-# DEPRECATED rjServerType "Use generic-lens or generic-optics with 'serverType' instead." #-}

-- | The ID of the server.
--
-- /Note:/ Consider using 'serverId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjServerId :: Lens.Lens' ReplicationJob (Lude.Maybe Lude.Text)
rjServerId = Lens.lens (serverId :: ReplicationJob -> Lude.Maybe Lude.Text) (\s a -> s {serverId = a} :: ReplicationJob)
{-# DEPRECATED rjServerId "Use generic-lens or generic-optics with 'serverId' instead." #-}

-- | The license type to be used for the AMI created by a successful replication run.
--
-- /Note:/ Consider using 'licenseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjLicenseType :: Lens.Lens' ReplicationJob (Lude.Maybe LicenseType)
rjLicenseType = Lens.lens (licenseType :: ReplicationJob -> Lude.Maybe LicenseType) (\s a -> s {licenseType = a} :: ReplicationJob)
{-# DEPRECATED rjLicenseType "Use generic-lens or generic-optics with 'licenseType' instead." #-}

-- | The name of the IAM role to be used by AWS SMS.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjRoleName :: Lens.Lens' ReplicationJob (Lude.Maybe Lude.Text)
rjRoleName = Lens.lens (roleName :: ReplicationJob -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: ReplicationJob)
{-# DEPRECATED rjRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Information about the VM server.
--
-- /Note:/ Consider using 'vmServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjVmServer :: Lens.Lens' ReplicationJob (Lude.Maybe VMServer)
rjVmServer = Lens.lens (vmServer :: ReplicationJob -> Lude.Maybe VMServer) (\s a -> s {vmServer = a} :: ReplicationJob)
{-# DEPRECATED rjVmServer "Use generic-lens or generic-optics with 'vmServer' instead." #-}

-- | Indicates whether the replication job should produce encrypted AMIs.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjEncrypted :: Lens.Lens' ReplicationJob (Lude.Maybe Lude.Bool)
rjEncrypted = Lens.lens (encrypted :: ReplicationJob -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: ReplicationJob)
{-# DEPRECATED rjEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The ID of the replication job.
--
-- /Note:/ Consider using 'replicationJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjReplicationJobId :: Lens.Lens' ReplicationJob (Lude.Maybe Lude.Text)
rjReplicationJobId = Lens.lens (replicationJobId :: ReplicationJob -> Lude.Maybe Lude.Text) (\s a -> s {replicationJobId = a} :: ReplicationJob)
{-# DEPRECATED rjReplicationJobId "Use generic-lens or generic-optics with 'replicationJobId' instead." #-}

-- | Information about the replication runs.
--
-- /Note:/ Consider using 'replicationRunList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjReplicationRunList :: Lens.Lens' ReplicationJob (Lude.Maybe [ReplicationRun])
rjReplicationRunList = Lens.lens (replicationRunList :: ReplicationJob -> Lude.Maybe [ReplicationRun]) (\s a -> s {replicationRunList = a} :: ReplicationJob)
{-# DEPRECATED rjReplicationRunList "Use generic-lens or generic-optics with 'replicationRunList' instead." #-}

-- | The start time of the next replication run.
--
-- /Note:/ Consider using 'nextReplicationRunStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjNextReplicationRunStartTime :: Lens.Lens' ReplicationJob (Lude.Maybe Lude.Timestamp)
rjNextReplicationRunStartTime = Lens.lens (nextReplicationRunStartTime :: ReplicationJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {nextReplicationRunStartTime = a} :: ReplicationJob)
{-# DEPRECATED rjNextReplicationRunStartTime "Use generic-lens or generic-optics with 'nextReplicationRunStartTime' instead." #-}

-- | The description of the current status of the replication job.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjStatusMessage :: Lens.Lens' ReplicationJob (Lude.Maybe Lude.Text)
rjStatusMessage = Lens.lens (statusMessage :: ReplicationJob -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ReplicationJob)
{-# DEPRECATED rjStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The ID of the KMS key for replication jobs that produce encrypted AMIs. This value can be any of the following:
--
--
--     * KMS key ID
--
--
--     * KMS key alias
--
--
--     * ARN referring to the KMS key ID
--
--
--     * ARN referring to the KMS key alias
--
--
-- If encrypted is enabled but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjKmsKeyId :: Lens.Lens' ReplicationJob (Lude.Maybe Lude.Text)
rjKmsKeyId = Lens.lens (kmsKeyId :: ReplicationJob -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: ReplicationJob)
{-# DEPRECATED rjKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The ID of the latest Amazon Machine Image (AMI).
--
-- /Note:/ Consider using 'latestAMIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjLatestAMIId :: Lens.Lens' ReplicationJob (Lude.Maybe Lude.Text)
rjLatestAMIId = Lens.lens (latestAMIId :: ReplicationJob -> Lude.Maybe Lude.Text) (\s a -> s {latestAMIId = a} :: ReplicationJob)
{-# DEPRECATED rjLatestAMIId "Use generic-lens or generic-optics with 'latestAMIId' instead." #-}

-- | The seed replication time.
--
-- /Note:/ Consider using 'seedReplicationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjSeedReplicationTime :: Lens.Lens' ReplicationJob (Lude.Maybe Lude.Timestamp)
rjSeedReplicationTime = Lens.lens (seedReplicationTime :: ReplicationJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {seedReplicationTime = a} :: ReplicationJob)
{-# DEPRECATED rjSeedReplicationTime "Use generic-lens or generic-optics with 'seedReplicationTime' instead." #-}

-- | Indicates whether to run the replication job one time.
--
-- /Note:/ Consider using 'runOnce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjRunOnce :: Lens.Lens' ReplicationJob (Lude.Maybe Lude.Bool)
rjRunOnce = Lens.lens (runOnce :: ReplicationJob -> Lude.Maybe Lude.Bool) (\s a -> s {runOnce = a} :: ReplicationJob)
{-# DEPRECATED rjRunOnce "Use generic-lens or generic-optics with 'runOnce' instead." #-}

-- | The description of the replication job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjDescription :: Lens.Lens' ReplicationJob (Lude.Maybe Lude.Text)
rjDescription = Lens.lens (description :: ReplicationJob -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ReplicationJob)
{-# DEPRECATED rjDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ReplicationJob where
  parseJSON =
    Lude.withObject
      "ReplicationJob"
      ( \x ->
          ReplicationJob'
            Lude.<$> (x Lude..:? "frequency")
            Lude.<*> (x Lude..:? "numberOfRecentAmisToKeep")
            Lude.<*> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "serverType")
            Lude.<*> (x Lude..:? "serverId")
            Lude.<*> (x Lude..:? "licenseType")
            Lude.<*> (x Lude..:? "roleName")
            Lude.<*> (x Lude..:? "vmServer")
            Lude.<*> (x Lude..:? "encrypted")
            Lude.<*> (x Lude..:? "replicationJobId")
            Lude.<*> (x Lude..:? "replicationRunList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "nextReplicationRunStartTime")
            Lude.<*> (x Lude..:? "statusMessage")
            Lude.<*> (x Lude..:? "kmsKeyId")
            Lude.<*> (x Lude..:? "latestAmiId")
            Lude.<*> (x Lude..:? "seedReplicationTime")
            Lude.<*> (x Lude..:? "runOnce")
            Lude.<*> (x Lude..:? "description")
      )
