{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Snapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Snapshot
  ( Snapshot (..),

    -- * Smart constructor
    mkSnapshot,

    -- * Lenses
    sDataEncryptionKeyId,
    sDescription,
    sEncrypted,
    sKmsKeyId,
    sOwnerAlias,
    sOwnerId,
    sProgress,
    sSnapshotId,
    sStartTime,
    sState,
    sStateMessage,
    sTags,
    sVolumeId,
    sVolumeSize,
  )
where

import qualified Network.AWS.EC2.Types.DataEncryptionKeyId as Types
import qualified Network.AWS.EC2.Types.Description as Types
import qualified Network.AWS.EC2.Types.KmsKeyId as Types
import qualified Network.AWS.EC2.Types.OwnerAlias as Types
import qualified Network.AWS.EC2.Types.OwnerId as Types
import qualified Network.AWS.EC2.Types.Progress as Types
import qualified Network.AWS.EC2.Types.SnapshotId as Types
import qualified Network.AWS.EC2.Types.SnapshotState as Types
import qualified Network.AWS.EC2.Types.StateMessage as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.VolumeId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a snapshot.
--
-- /See:/ 'mkSnapshot' smart constructor.
data Snapshot = Snapshot'
  { -- | The data encryption key identifier for the snapshot. This value is a unique identifier that corresponds to the data encryption key that was used to encrypt the original volume or snapshot copy. Because data encryption keys are inherited by volumes created from snapshots, and vice versa, if snapshots share the same data encryption key identifier, then they belong to the same volume/snapshot lineage. This parameter is only returned by 'DescribeSnapshots' .
    dataEncryptionKeyId :: Core.Maybe Types.DataEncryptionKeyId,
    -- | The description for the snapshot.
    description :: Types.Description,
    -- | Indicates whether the snapshot is encrypted.
    encrypted :: Core.Bool,
    -- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the parent volume.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | The AWS owner alias, from an Amazon-maintained list (@amazon@ ). This is not the user-configured AWS account alias set using the IAM console.
    ownerAlias :: Core.Maybe Types.OwnerAlias,
    -- | The AWS account ID of the EBS snapshot owner.
    ownerId :: Types.OwnerId,
    -- | The progress of the snapshot, as a percentage.
    progress :: Types.Progress,
    -- | The ID of the snapshot. Each snapshot receives a unique identifier when it is created.
    snapshotId :: Types.SnapshotId,
    -- | The time stamp when the snapshot was initiated.
    startTime :: Core.UTCTime,
    -- | The snapshot state.
    state :: Types.SnapshotState,
    -- | Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot copy operation fails (for example, if the proper AWS Key Management Service (AWS KMS) permissions are not obtained) this field displays error state details to help you diagnose why the error occurred. This parameter is only returned by 'DescribeSnapshots' .
    stateMessage :: Core.Maybe Types.StateMessage,
    -- | Any tags assigned to the snapshot.
    tags :: Core.Maybe [Types.Tag],
    -- | The ID of the volume that was used to create the snapshot. Snapshots created by the 'CopySnapshot' action have an arbitrary volume ID that should not be used for any purpose.
    volumeId :: Types.VolumeId,
    -- | The size of the volume, in GiB.
    volumeSize :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Snapshot' value with any optional fields omitted.
mkSnapshot ::
  -- | 'description'
  Types.Description ->
  -- | 'encrypted'
  Core.Bool ->
  -- | 'ownerId'
  Types.OwnerId ->
  -- | 'progress'
  Types.Progress ->
  -- | 'snapshotId'
  Types.SnapshotId ->
  -- | 'startTime'
  Core.UTCTime ->
  -- | 'state'
  Types.SnapshotState ->
  -- | 'volumeId'
  Types.VolumeId ->
  -- | 'volumeSize'
  Core.Int ->
  Snapshot
mkSnapshot
  description
  encrypted
  ownerId
  progress
  snapshotId
  startTime
  state
  volumeId
  volumeSize =
    Snapshot'
      { dataEncryptionKeyId = Core.Nothing,
        description,
        encrypted,
        kmsKeyId = Core.Nothing,
        ownerAlias = Core.Nothing,
        ownerId,
        progress,
        snapshotId,
        startTime,
        state,
        stateMessage = Core.Nothing,
        tags = Core.Nothing,
        volumeId,
        volumeSize
      }

-- | The data encryption key identifier for the snapshot. This value is a unique identifier that corresponds to the data encryption key that was used to encrypt the original volume or snapshot copy. Because data encryption keys are inherited by volumes created from snapshots, and vice versa, if snapshots share the same data encryption key identifier, then they belong to the same volume/snapshot lineage. This parameter is only returned by 'DescribeSnapshots' .
--
-- /Note:/ Consider using 'dataEncryptionKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDataEncryptionKeyId :: Lens.Lens' Snapshot (Core.Maybe Types.DataEncryptionKeyId)
sDataEncryptionKeyId = Lens.field @"dataEncryptionKeyId"
{-# DEPRECATED sDataEncryptionKeyId "Use generic-lens or generic-optics with 'dataEncryptionKeyId' instead." #-}

-- | The description for the snapshot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDescription :: Lens.Lens' Snapshot Types.Description
sDescription = Lens.field @"description"
{-# DEPRECATED sDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Indicates whether the snapshot is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEncrypted :: Lens.Lens' Snapshot Core.Bool
sEncrypted = Lens.field @"encrypted"
{-# DEPRECATED sEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the parent volume.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKmsKeyId :: Lens.Lens' Snapshot (Core.Maybe Types.KmsKeyId)
sKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED sKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The AWS owner alias, from an Amazon-maintained list (@amazon@ ). This is not the user-configured AWS account alias set using the IAM console.
--
-- /Note:/ Consider using 'ownerAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwnerAlias :: Lens.Lens' Snapshot (Core.Maybe Types.OwnerAlias)
sOwnerAlias = Lens.field @"ownerAlias"
{-# DEPRECATED sOwnerAlias "Use generic-lens or generic-optics with 'ownerAlias' instead." #-}

-- | The AWS account ID of the EBS snapshot owner.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwnerId :: Lens.Lens' Snapshot Types.OwnerId
sOwnerId = Lens.field @"ownerId"
{-# DEPRECATED sOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The progress of the snapshot, as a percentage.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sProgress :: Lens.Lens' Snapshot Types.Progress
sProgress = Lens.field @"progress"
{-# DEPRECATED sProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The ID of the snapshot. Each snapshot receives a unique identifier when it is created.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotId :: Lens.Lens' Snapshot Types.SnapshotId
sSnapshotId = Lens.field @"snapshotId"
{-# DEPRECATED sSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The time stamp when the snapshot was initiated.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartTime :: Lens.Lens' Snapshot Core.UTCTime
sStartTime = Lens.field @"startTime"
{-# DEPRECATED sStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The snapshot state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sState :: Lens.Lens' Snapshot Types.SnapshotState
sState = Lens.field @"state"
{-# DEPRECATED sState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot copy operation fails (for example, if the proper AWS Key Management Service (AWS KMS) permissions are not obtained) this field displays error state details to help you diagnose why the error occurred. This parameter is only returned by 'DescribeSnapshots' .
--
-- /Note:/ Consider using 'stateMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStateMessage :: Lens.Lens' Snapshot (Core.Maybe Types.StateMessage)
sStateMessage = Lens.field @"stateMessage"
{-# DEPRECATED sStateMessage "Use generic-lens or generic-optics with 'stateMessage' instead." #-}

-- | Any tags assigned to the snapshot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTags :: Lens.Lens' Snapshot (Core.Maybe [Types.Tag])
sTags = Lens.field @"tags"
{-# DEPRECATED sTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the volume that was used to create the snapshot. Snapshots created by the 'CopySnapshot' action have an arbitrary volume ID that should not be used for any purpose.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVolumeId :: Lens.Lens' Snapshot Types.VolumeId
sVolumeId = Lens.field @"volumeId"
{-# DEPRECATED sVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The size of the volume, in GiB.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVolumeSize :: Lens.Lens' Snapshot Core.Int
sVolumeSize = Lens.field @"volumeSize"
{-# DEPRECATED sVolumeSize "Use generic-lens or generic-optics with 'volumeSize' instead." #-}

instance Core.FromXML Snapshot where
  parseXML x =
    Snapshot'
      Core.<$> (x Core..@? "dataEncryptionKeyId")
      Core.<*> (x Core..@ "description")
      Core.<*> (x Core..@ "encrypted")
      Core.<*> (x Core..@? "kmsKeyId")
      Core.<*> (x Core..@? "ownerAlias")
      Core.<*> (x Core..@ "ownerId")
      Core.<*> (x Core..@ "progress")
      Core.<*> (x Core..@ "snapshotId")
      Core.<*> (x Core..@ "startTime")
      Core.<*> (x Core..@ "status")
      Core.<*> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@ "volumeId")
      Core.<*> (x Core..@ "volumeSize")
