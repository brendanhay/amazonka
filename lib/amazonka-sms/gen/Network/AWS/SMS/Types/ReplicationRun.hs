{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ReplicationRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ReplicationRun
  ( ReplicationRun (..),

    -- * Smart constructor
    mkReplicationRun,

    -- * Lenses
    rrAmiId,
    rrCompletedTime,
    rrDescription,
    rrEncrypted,
    rrKmsKeyId,
    rrReplicationRunId,
    rrScheduledStartTime,
    rrStageDetails,
    rrState,
    rrStatusMessage,
    rrType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.AmiId as Types
import qualified Network.AWS.SMS.Types.Description as Types
import qualified Network.AWS.SMS.Types.KmsKeyId as Types
import qualified Network.AWS.SMS.Types.ReplicationRunId as Types
import qualified Network.AWS.SMS.Types.ReplicationRunStageDetails as Types
import qualified Network.AWS.SMS.Types.ReplicationRunState as Types
import qualified Network.AWS.SMS.Types.ReplicationRunStatusMessage as Types
import qualified Network.AWS.SMS.Types.ReplicationRunType as Types

-- | Represents a replication run.
--
-- /See:/ 'mkReplicationRun' smart constructor.
data ReplicationRun = ReplicationRun'
  { -- | The ID of the Amazon Machine Image (AMI) from the replication run.
    amiId :: Core.Maybe Types.AmiId,
    -- | The completion time of the last replication run.
    completedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The description of the replication run.
    description :: Core.Maybe Types.Description,
    -- | Indicates whether the replication run should produce an encrypted AMI.
    encrypted :: Core.Maybe Core.Bool,
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
    -- If encrypted is /true/ but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | The ID of the replication run.
    replicationRunId :: Core.Maybe Types.ReplicationRunId,
    -- | The start time of the next replication run.
    scheduledStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | Details about the current stage of the replication run.
    stageDetails :: Core.Maybe Types.ReplicationRunStageDetails,
    -- | The state of the replication run.
    state :: Core.Maybe Types.ReplicationRunState,
    -- | The description of the current status of the replication job.
    statusMessage :: Core.Maybe Types.ReplicationRunStatusMessage,
    -- | The type of replication run.
    type' :: Core.Maybe Types.ReplicationRunType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReplicationRun' value with any optional fields omitted.
mkReplicationRun ::
  ReplicationRun
mkReplicationRun =
  ReplicationRun'
    { amiId = Core.Nothing,
      completedTime = Core.Nothing,
      description = Core.Nothing,
      encrypted = Core.Nothing,
      kmsKeyId = Core.Nothing,
      replicationRunId = Core.Nothing,
      scheduledStartTime = Core.Nothing,
      stageDetails = Core.Nothing,
      state = Core.Nothing,
      statusMessage = Core.Nothing,
      type' = Core.Nothing
    }

-- | The ID of the Amazon Machine Image (AMI) from the replication run.
--
-- /Note:/ Consider using 'amiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrAmiId :: Lens.Lens' ReplicationRun (Core.Maybe Types.AmiId)
rrAmiId = Lens.field @"amiId"
{-# DEPRECATED rrAmiId "Use generic-lens or generic-optics with 'amiId' instead." #-}

-- | The completion time of the last replication run.
--
-- /Note:/ Consider using 'completedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrCompletedTime :: Lens.Lens' ReplicationRun (Core.Maybe Core.NominalDiffTime)
rrCompletedTime = Lens.field @"completedTime"
{-# DEPRECATED rrCompletedTime "Use generic-lens or generic-optics with 'completedTime' instead." #-}

-- | The description of the replication run.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDescription :: Lens.Lens' ReplicationRun (Core.Maybe Types.Description)
rrDescription = Lens.field @"description"
{-# DEPRECATED rrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Indicates whether the replication run should produce an encrypted AMI.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrEncrypted :: Lens.Lens' ReplicationRun (Core.Maybe Core.Bool)
rrEncrypted = Lens.field @"encrypted"
{-# DEPRECATED rrEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

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
-- If encrypted is /true/ but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrKmsKeyId :: Lens.Lens' ReplicationRun (Core.Maybe Types.KmsKeyId)
rrKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED rrKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The ID of the replication run.
--
-- /Note:/ Consider using 'replicationRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrReplicationRunId :: Lens.Lens' ReplicationRun (Core.Maybe Types.ReplicationRunId)
rrReplicationRunId = Lens.field @"replicationRunId"
{-# DEPRECATED rrReplicationRunId "Use generic-lens or generic-optics with 'replicationRunId' instead." #-}

-- | The start time of the next replication run.
--
-- /Note:/ Consider using 'scheduledStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrScheduledStartTime :: Lens.Lens' ReplicationRun (Core.Maybe Core.NominalDiffTime)
rrScheduledStartTime = Lens.field @"scheduledStartTime"
{-# DEPRECATED rrScheduledStartTime "Use generic-lens or generic-optics with 'scheduledStartTime' instead." #-}

-- | Details about the current stage of the replication run.
--
-- /Note:/ Consider using 'stageDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrStageDetails :: Lens.Lens' ReplicationRun (Core.Maybe Types.ReplicationRunStageDetails)
rrStageDetails = Lens.field @"stageDetails"
{-# DEPRECATED rrStageDetails "Use generic-lens or generic-optics with 'stageDetails' instead." #-}

-- | The state of the replication run.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrState :: Lens.Lens' ReplicationRun (Core.Maybe Types.ReplicationRunState)
rrState = Lens.field @"state"
{-# DEPRECATED rrState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The description of the current status of the replication job.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrStatusMessage :: Lens.Lens' ReplicationRun (Core.Maybe Types.ReplicationRunStatusMessage)
rrStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED rrStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The type of replication run.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrType :: Lens.Lens' ReplicationRun (Core.Maybe Types.ReplicationRunType)
rrType = Lens.field @"type'"
{-# DEPRECATED rrType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON ReplicationRun where
  parseJSON =
    Core.withObject "ReplicationRun" Core.$
      \x ->
        ReplicationRun'
          Core.<$> (x Core..:? "amiId")
          Core.<*> (x Core..:? "completedTime")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "encrypted")
          Core.<*> (x Core..:? "kmsKeyId")
          Core.<*> (x Core..:? "replicationRunId")
          Core.<*> (x Core..:? "scheduledStartTime")
          Core.<*> (x Core..:? "stageDetails")
          Core.<*> (x Core..:? "state")
          Core.<*> (x Core..:? "statusMessage")
          Core.<*> (x Core..:? "type")
