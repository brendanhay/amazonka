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
    rrState,
    rrReplicationRunId,
    rrEncrypted,
    rrStageDetails,
    rrScheduledStartTime,
    rrStatusMessage,
    rrKmsKeyId,
    rrCompletedTime,
    rrAmiId,
    rrType,
    rrDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.ReplicationRunStageDetails
import Network.AWS.SMS.Types.ReplicationRunState
import Network.AWS.SMS.Types.ReplicationRunType

-- | Represents a replication run.
--
-- /See:/ 'mkReplicationRun' smart constructor.
data ReplicationRun = ReplicationRun'
  { state ::
      Lude.Maybe ReplicationRunState,
    replicationRunId :: Lude.Maybe Lude.Text,
    encrypted :: Lude.Maybe Lude.Bool,
    stageDetails :: Lude.Maybe ReplicationRunStageDetails,
    scheduledStartTime :: Lude.Maybe Lude.Timestamp,
    statusMessage :: Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    completedTime :: Lude.Maybe Lude.Timestamp,
    amiId :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe ReplicationRunType,
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

-- | Creates a value of 'ReplicationRun' with the minimum fields required to make a request.
--
-- * 'amiId' - The ID of the Amazon Machine Image (AMI) from the replication run.
-- * 'completedTime' - The completion time of the last replication run.
-- * 'description' - The description of the replication run.
-- * 'encrypted' - Indicates whether the replication run should produce an encrypted AMI.
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
-- If encrypted is /true/ but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
-- * 'replicationRunId' - The ID of the replication run.
-- * 'scheduledStartTime' - The start time of the next replication run.
-- * 'stageDetails' - Details about the current stage of the replication run.
-- * 'state' - The state of the replication run.
-- * 'statusMessage' - The description of the current status of the replication job.
-- * 'type'' - The type of replication run.
mkReplicationRun ::
  ReplicationRun
mkReplicationRun =
  ReplicationRun'
    { state = Lude.Nothing,
      replicationRunId = Lude.Nothing,
      encrypted = Lude.Nothing,
      stageDetails = Lude.Nothing,
      scheduledStartTime = Lude.Nothing,
      statusMessage = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      completedTime = Lude.Nothing,
      amiId = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The state of the replication run.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrState :: Lens.Lens' ReplicationRun (Lude.Maybe ReplicationRunState)
rrState = Lens.lens (state :: ReplicationRun -> Lude.Maybe ReplicationRunState) (\s a -> s {state = a} :: ReplicationRun)
{-# DEPRECATED rrState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the replication run.
--
-- /Note:/ Consider using 'replicationRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrReplicationRunId :: Lens.Lens' ReplicationRun (Lude.Maybe Lude.Text)
rrReplicationRunId = Lens.lens (replicationRunId :: ReplicationRun -> Lude.Maybe Lude.Text) (\s a -> s {replicationRunId = a} :: ReplicationRun)
{-# DEPRECATED rrReplicationRunId "Use generic-lens or generic-optics with 'replicationRunId' instead." #-}

-- | Indicates whether the replication run should produce an encrypted AMI.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrEncrypted :: Lens.Lens' ReplicationRun (Lude.Maybe Lude.Bool)
rrEncrypted = Lens.lens (encrypted :: ReplicationRun -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: ReplicationRun)
{-# DEPRECATED rrEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | Details about the current stage of the replication run.
--
-- /Note:/ Consider using 'stageDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrStageDetails :: Lens.Lens' ReplicationRun (Lude.Maybe ReplicationRunStageDetails)
rrStageDetails = Lens.lens (stageDetails :: ReplicationRun -> Lude.Maybe ReplicationRunStageDetails) (\s a -> s {stageDetails = a} :: ReplicationRun)
{-# DEPRECATED rrStageDetails "Use generic-lens or generic-optics with 'stageDetails' instead." #-}

-- | The start time of the next replication run.
--
-- /Note:/ Consider using 'scheduledStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrScheduledStartTime :: Lens.Lens' ReplicationRun (Lude.Maybe Lude.Timestamp)
rrScheduledStartTime = Lens.lens (scheduledStartTime :: ReplicationRun -> Lude.Maybe Lude.Timestamp) (\s a -> s {scheduledStartTime = a} :: ReplicationRun)
{-# DEPRECATED rrScheduledStartTime "Use generic-lens or generic-optics with 'scheduledStartTime' instead." #-}

-- | The description of the current status of the replication job.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrStatusMessage :: Lens.Lens' ReplicationRun (Lude.Maybe Lude.Text)
rrStatusMessage = Lens.lens (statusMessage :: ReplicationRun -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ReplicationRun)
{-# DEPRECATED rrStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

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
rrKmsKeyId :: Lens.Lens' ReplicationRun (Lude.Maybe Lude.Text)
rrKmsKeyId = Lens.lens (kmsKeyId :: ReplicationRun -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: ReplicationRun)
{-# DEPRECATED rrKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The completion time of the last replication run.
--
-- /Note:/ Consider using 'completedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrCompletedTime :: Lens.Lens' ReplicationRun (Lude.Maybe Lude.Timestamp)
rrCompletedTime = Lens.lens (completedTime :: ReplicationRun -> Lude.Maybe Lude.Timestamp) (\s a -> s {completedTime = a} :: ReplicationRun)
{-# DEPRECATED rrCompletedTime "Use generic-lens or generic-optics with 'completedTime' instead." #-}

-- | The ID of the Amazon Machine Image (AMI) from the replication run.
--
-- /Note:/ Consider using 'amiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrAmiId :: Lens.Lens' ReplicationRun (Lude.Maybe Lude.Text)
rrAmiId = Lens.lens (amiId :: ReplicationRun -> Lude.Maybe Lude.Text) (\s a -> s {amiId = a} :: ReplicationRun)
{-# DEPRECATED rrAmiId "Use generic-lens or generic-optics with 'amiId' instead." #-}

-- | The type of replication run.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrType :: Lens.Lens' ReplicationRun (Lude.Maybe ReplicationRunType)
rrType = Lens.lens (type' :: ReplicationRun -> Lude.Maybe ReplicationRunType) (\s a -> s {type' = a} :: ReplicationRun)
{-# DEPRECATED rrType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The description of the replication run.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDescription :: Lens.Lens' ReplicationRun (Lude.Maybe Lude.Text)
rrDescription = Lens.lens (description :: ReplicationRun -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ReplicationRun)
{-# DEPRECATED rrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ReplicationRun where
  parseJSON =
    Lude.withObject
      "ReplicationRun"
      ( \x ->
          ReplicationRun'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "replicationRunId")
            Lude.<*> (x Lude..:? "encrypted")
            Lude.<*> (x Lude..:? "stageDetails")
            Lude.<*> (x Lude..:? "scheduledStartTime")
            Lude.<*> (x Lude..:? "statusMessage")
            Lude.<*> (x Lude..:? "kmsKeyId")
            Lude.<*> (x Lude..:? "completedTime")
            Lude.<*> (x Lude..:? "amiId")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "description")
      )
