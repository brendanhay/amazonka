{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    rjDescription,
    rjEncrypted,
    rjFrequency,
    rjKmsKeyId,
    rjLatestAmiId,
    rjLicenseType,
    rjNextReplicationRunStartTime,
    rjNumberOfRecentAmisToKeep,
    rjReplicationJobId,
    rjReplicationRunList,
    rjRoleName,
    rjRunOnce,
    rjSeedReplicationTime,
    rjServerId,
    rjServerType,
    rjState,
    rjStatusMessage,
    rjVmServer,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.Description as Types
import qualified Network.AWS.SMS.Types.KmsKeyId as Types
import qualified Network.AWS.SMS.Types.LatestAmiId as Types
import qualified Network.AWS.SMS.Types.LicenseType as Types
import qualified Network.AWS.SMS.Types.ReplicationJobId as Types
import qualified Network.AWS.SMS.Types.ReplicationJobState as Types
import qualified Network.AWS.SMS.Types.ReplicationJobStatusMessage as Types
import qualified Network.AWS.SMS.Types.ReplicationRun as Types
import qualified Network.AWS.SMS.Types.RoleName as Types
import qualified Network.AWS.SMS.Types.ServerId as Types
import qualified Network.AWS.SMS.Types.ServerType as Types
import qualified Network.AWS.SMS.Types.VmServer as Types

-- | Represents a replication job.
--
-- /See:/ 'mkReplicationJob' smart constructor.
data ReplicationJob = ReplicationJob'
  { -- | The description of the replication job.
    description :: Core.Maybe Types.Description,
    -- | Indicates whether the replication job should produce encrypted AMIs.
    encrypted :: Core.Maybe Core.Bool,
    -- | The time between consecutive replication runs, in hours.
    frequency :: Core.Maybe Core.Int,
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
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | The ID of the latest Amazon Machine Image (AMI).
    latestAmiId :: Core.Maybe Types.LatestAmiId,
    -- | The license type to be used for the AMI created by a successful replication run.
    licenseType :: Core.Maybe Types.LicenseType,
    -- | The start time of the next replication run.
    nextReplicationRunStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | The number of recent AMIs to keep in the customer's account for a replication job. By default, the value is set to zero, meaning that all AMIs are kept.
    numberOfRecentAmisToKeep :: Core.Maybe Core.Int,
    -- | The ID of the replication job.
    replicationJobId :: Core.Maybe Types.ReplicationJobId,
    -- | Information about the replication runs.
    replicationRunList :: Core.Maybe [Types.ReplicationRun],
    -- | The name of the IAM role to be used by AWS SMS.
    roleName :: Core.Maybe Types.RoleName,
    -- | Indicates whether to run the replication job one time.
    runOnce :: Core.Maybe Core.Bool,
    -- | The seed replication time.
    seedReplicationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the server.
    serverId :: Core.Maybe Types.ServerId,
    -- | The type of server.
    serverType :: Core.Maybe Types.ServerType,
    -- | The state of the replication job.
    state :: Core.Maybe Types.ReplicationJobState,
    -- | The description of the current status of the replication job.
    statusMessage :: Core.Maybe Types.ReplicationJobStatusMessage,
    -- | Information about the VM server.
    vmServer :: Core.Maybe Types.VmServer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReplicationJob' value with any optional fields omitted.
mkReplicationJob ::
  ReplicationJob
mkReplicationJob =
  ReplicationJob'
    { description = Core.Nothing,
      encrypted = Core.Nothing,
      frequency = Core.Nothing,
      kmsKeyId = Core.Nothing,
      latestAmiId = Core.Nothing,
      licenseType = Core.Nothing,
      nextReplicationRunStartTime = Core.Nothing,
      numberOfRecentAmisToKeep = Core.Nothing,
      replicationJobId = Core.Nothing,
      replicationRunList = Core.Nothing,
      roleName = Core.Nothing,
      runOnce = Core.Nothing,
      seedReplicationTime = Core.Nothing,
      serverId = Core.Nothing,
      serverType = Core.Nothing,
      state = Core.Nothing,
      statusMessage = Core.Nothing,
      vmServer = Core.Nothing
    }

-- | The description of the replication job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjDescription :: Lens.Lens' ReplicationJob (Core.Maybe Types.Description)
rjDescription = Lens.field @"description"
{-# DEPRECATED rjDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Indicates whether the replication job should produce encrypted AMIs.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjEncrypted :: Lens.Lens' ReplicationJob (Core.Maybe Core.Bool)
rjEncrypted = Lens.field @"encrypted"
{-# DEPRECATED rjEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The time between consecutive replication runs, in hours.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjFrequency :: Lens.Lens' ReplicationJob (Core.Maybe Core.Int)
rjFrequency = Lens.field @"frequency"
{-# DEPRECATED rjFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

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
rjKmsKeyId :: Lens.Lens' ReplicationJob (Core.Maybe Types.KmsKeyId)
rjKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED rjKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The ID of the latest Amazon Machine Image (AMI).
--
-- /Note:/ Consider using 'latestAmiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjLatestAmiId :: Lens.Lens' ReplicationJob (Core.Maybe Types.LatestAmiId)
rjLatestAmiId = Lens.field @"latestAmiId"
{-# DEPRECATED rjLatestAmiId "Use generic-lens or generic-optics with 'latestAmiId' instead." #-}

-- | The license type to be used for the AMI created by a successful replication run.
--
-- /Note:/ Consider using 'licenseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjLicenseType :: Lens.Lens' ReplicationJob (Core.Maybe Types.LicenseType)
rjLicenseType = Lens.field @"licenseType"
{-# DEPRECATED rjLicenseType "Use generic-lens or generic-optics with 'licenseType' instead." #-}

-- | The start time of the next replication run.
--
-- /Note:/ Consider using 'nextReplicationRunStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjNextReplicationRunStartTime :: Lens.Lens' ReplicationJob (Core.Maybe Core.NominalDiffTime)
rjNextReplicationRunStartTime = Lens.field @"nextReplicationRunStartTime"
{-# DEPRECATED rjNextReplicationRunStartTime "Use generic-lens or generic-optics with 'nextReplicationRunStartTime' instead." #-}

-- | The number of recent AMIs to keep in the customer's account for a replication job. By default, the value is set to zero, meaning that all AMIs are kept.
--
-- /Note:/ Consider using 'numberOfRecentAmisToKeep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjNumberOfRecentAmisToKeep :: Lens.Lens' ReplicationJob (Core.Maybe Core.Int)
rjNumberOfRecentAmisToKeep = Lens.field @"numberOfRecentAmisToKeep"
{-# DEPRECATED rjNumberOfRecentAmisToKeep "Use generic-lens or generic-optics with 'numberOfRecentAmisToKeep' instead." #-}

-- | The ID of the replication job.
--
-- /Note:/ Consider using 'replicationJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjReplicationJobId :: Lens.Lens' ReplicationJob (Core.Maybe Types.ReplicationJobId)
rjReplicationJobId = Lens.field @"replicationJobId"
{-# DEPRECATED rjReplicationJobId "Use generic-lens or generic-optics with 'replicationJobId' instead." #-}

-- | Information about the replication runs.
--
-- /Note:/ Consider using 'replicationRunList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjReplicationRunList :: Lens.Lens' ReplicationJob (Core.Maybe [Types.ReplicationRun])
rjReplicationRunList = Lens.field @"replicationRunList"
{-# DEPRECATED rjReplicationRunList "Use generic-lens or generic-optics with 'replicationRunList' instead." #-}

-- | The name of the IAM role to be used by AWS SMS.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjRoleName :: Lens.Lens' ReplicationJob (Core.Maybe Types.RoleName)
rjRoleName = Lens.field @"roleName"
{-# DEPRECATED rjRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Indicates whether to run the replication job one time.
--
-- /Note:/ Consider using 'runOnce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjRunOnce :: Lens.Lens' ReplicationJob (Core.Maybe Core.Bool)
rjRunOnce = Lens.field @"runOnce"
{-# DEPRECATED rjRunOnce "Use generic-lens or generic-optics with 'runOnce' instead." #-}

-- | The seed replication time.
--
-- /Note:/ Consider using 'seedReplicationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjSeedReplicationTime :: Lens.Lens' ReplicationJob (Core.Maybe Core.NominalDiffTime)
rjSeedReplicationTime = Lens.field @"seedReplicationTime"
{-# DEPRECATED rjSeedReplicationTime "Use generic-lens or generic-optics with 'seedReplicationTime' instead." #-}

-- | The ID of the server.
--
-- /Note:/ Consider using 'serverId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjServerId :: Lens.Lens' ReplicationJob (Core.Maybe Types.ServerId)
rjServerId = Lens.field @"serverId"
{-# DEPRECATED rjServerId "Use generic-lens or generic-optics with 'serverId' instead." #-}

-- | The type of server.
--
-- /Note:/ Consider using 'serverType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjServerType :: Lens.Lens' ReplicationJob (Core.Maybe Types.ServerType)
rjServerType = Lens.field @"serverType"
{-# DEPRECATED rjServerType "Use generic-lens or generic-optics with 'serverType' instead." #-}

-- | The state of the replication job.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjState :: Lens.Lens' ReplicationJob (Core.Maybe Types.ReplicationJobState)
rjState = Lens.field @"state"
{-# DEPRECATED rjState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The description of the current status of the replication job.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjStatusMessage :: Lens.Lens' ReplicationJob (Core.Maybe Types.ReplicationJobStatusMessage)
rjStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED rjStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | Information about the VM server.
--
-- /Note:/ Consider using 'vmServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjVmServer :: Lens.Lens' ReplicationJob (Core.Maybe Types.VmServer)
rjVmServer = Lens.field @"vmServer"
{-# DEPRECATED rjVmServer "Use generic-lens or generic-optics with 'vmServer' instead." #-}

instance Core.FromJSON ReplicationJob where
  parseJSON =
    Core.withObject "ReplicationJob" Core.$
      \x ->
        ReplicationJob'
          Core.<$> (x Core..:? "description")
          Core.<*> (x Core..:? "encrypted")
          Core.<*> (x Core..:? "frequency")
          Core.<*> (x Core..:? "kmsKeyId")
          Core.<*> (x Core..:? "latestAmiId")
          Core.<*> (x Core..:? "licenseType")
          Core.<*> (x Core..:? "nextReplicationRunStartTime")
          Core.<*> (x Core..:? "numberOfRecentAmisToKeep")
          Core.<*> (x Core..:? "replicationJobId")
          Core.<*> (x Core..:? "replicationRunList")
          Core.<*> (x Core..:? "roleName")
          Core.<*> (x Core..:? "runOnce")
          Core.<*> (x Core..:? "seedReplicationTime")
          Core.<*> (x Core..:? "serverId")
          Core.<*> (x Core..:? "serverType")
          Core.<*> (x Core..:? "state")
          Core.<*> (x Core..:? "statusMessage")
          Core.<*> (x Core..:? "vmServer")
