{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Snapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Snapshot
  ( Snapshot (..)
  -- * Smart constructor
  , mkSnapshot
  -- * Lenses
  , sDataEncryptionKeyId
  , sDescription
  , sEncrypted
  , sKmsKeyId
  , sOwnerAlias
  , sOwnerId
  , sProgress
  , sSnapshotId
  , sStartTime
  , sState
  , sStateMessage
  , sTags
  , sVolumeId
  , sVolumeSize
  ) where

import qualified Network.AWS.EC2.Types.SnapshotState as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a snapshot.
--
-- /See:/ 'mkSnapshot' smart constructor.
data Snapshot = Snapshot'
  { dataEncryptionKeyId :: Core.Maybe Core.Text
    -- ^ The data encryption key identifier for the snapshot. This value is a unique identifier that corresponds to the data encryption key that was used to encrypt the original volume or snapshot copy. Because data encryption keys are inherited by volumes created from snapshots, and vice versa, if snapshots share the same data encryption key identifier, then they belong to the same volume/snapshot lineage. This parameter is only returned by 'DescribeSnapshots' .
  , description :: Core.Text
    -- ^ The description for the snapshot.
  , encrypted :: Core.Bool
    -- ^ Indicates whether the snapshot is encrypted.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the parent volume.
  , ownerAlias :: Core.Maybe Core.Text
    -- ^ The AWS owner alias, from an Amazon-maintained list (@amazon@ ). This is not the user-configured AWS account alias set using the IAM console.
  , ownerId :: Core.Text
    -- ^ The AWS account ID of the EBS snapshot owner.
  , progress :: Core.Text
    -- ^ The progress of the snapshot, as a percentage.
  , snapshotId :: Core.Text
    -- ^ The ID of the snapshot. Each snapshot receives a unique identifier when it is created.
  , startTime :: Core.UTCTime
    -- ^ The time stamp when the snapshot was initiated.
  , state :: Types.SnapshotState
    -- ^ The snapshot state.
  , stateMessage :: Core.Maybe Core.Text
    -- ^ Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot copy operation fails (for example, if the proper AWS Key Management Service (AWS KMS) permissions are not obtained) this field displays error state details to help you diagnose why the error occurred. This parameter is only returned by 'DescribeSnapshots' .
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the snapshot.
  , volumeId :: Core.Text
    -- ^ The ID of the volume that was used to create the snapshot. Snapshots created by the 'CopySnapshot' action have an arbitrary volume ID that should not be used for any purpose.
  , volumeSize :: Core.Int
    -- ^ The size of the volume, in GiB.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Snapshot' value with any optional fields omitted.
mkSnapshot
    :: Core.Text -- ^ 'description'
    -> Core.Bool -- ^ 'encrypted'
    -> Core.Text -- ^ 'ownerId'
    -> Core.Text -- ^ 'progress'
    -> Core.Text -- ^ 'snapshotId'
    -> Core.UTCTime -- ^ 'startTime'
    -> Types.SnapshotState -- ^ 'state'
    -> Core.Text -- ^ 'volumeId'
    -> Core.Int -- ^ 'volumeSize'
    -> Snapshot
mkSnapshot description encrypted ownerId progress snapshotId
  startTime state volumeId volumeSize
  = Snapshot'{dataEncryptionKeyId = Core.Nothing, description,
              encrypted, kmsKeyId = Core.Nothing, ownerAlias = Core.Nothing,
              ownerId, progress, snapshotId, startTime, state,
              stateMessage = Core.Nothing, tags = Core.Nothing, volumeId,
              volumeSize}

-- | The data encryption key identifier for the snapshot. This value is a unique identifier that corresponds to the data encryption key that was used to encrypt the original volume or snapshot copy. Because data encryption keys are inherited by volumes created from snapshots, and vice versa, if snapshots share the same data encryption key identifier, then they belong to the same volume/snapshot lineage. This parameter is only returned by 'DescribeSnapshots' .
--
-- /Note:/ Consider using 'dataEncryptionKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDataEncryptionKeyId :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sDataEncryptionKeyId = Lens.field @"dataEncryptionKeyId"
{-# INLINEABLE sDataEncryptionKeyId #-}
{-# DEPRECATED dataEncryptionKeyId "Use generic-lens or generic-optics with 'dataEncryptionKeyId' instead"  #-}

-- | The description for the snapshot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDescription :: Lens.Lens' Snapshot Core.Text
sDescription = Lens.field @"description"
{-# INLINEABLE sDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Indicates whether the snapshot is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEncrypted :: Lens.Lens' Snapshot Core.Bool
sEncrypted = Lens.field @"encrypted"
{-# INLINEABLE sEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the parent volume.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKmsKeyId :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE sKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The AWS owner alias, from an Amazon-maintained list (@amazon@ ). This is not the user-configured AWS account alias set using the IAM console.
--
-- /Note:/ Consider using 'ownerAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwnerAlias :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sOwnerAlias = Lens.field @"ownerAlias"
{-# INLINEABLE sOwnerAlias #-}
{-# DEPRECATED ownerAlias "Use generic-lens or generic-optics with 'ownerAlias' instead"  #-}

-- | The AWS account ID of the EBS snapshot owner.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwnerId :: Lens.Lens' Snapshot Core.Text
sOwnerId = Lens.field @"ownerId"
{-# INLINEABLE sOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The progress of the snapshot, as a percentage.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sProgress :: Lens.Lens' Snapshot Core.Text
sProgress = Lens.field @"progress"
{-# INLINEABLE sProgress #-}
{-# DEPRECATED progress "Use generic-lens or generic-optics with 'progress' instead"  #-}

-- | The ID of the snapshot. Each snapshot receives a unique identifier when it is created.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotId :: Lens.Lens' Snapshot Core.Text
sSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE sSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The time stamp when the snapshot was initiated.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartTime :: Lens.Lens' Snapshot Core.UTCTime
sStartTime = Lens.field @"startTime"
{-# INLINEABLE sStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The snapshot state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sState :: Lens.Lens' Snapshot Types.SnapshotState
sState = Lens.field @"state"
{-# INLINEABLE sState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot copy operation fails (for example, if the proper AWS Key Management Service (AWS KMS) permissions are not obtained) this field displays error state details to help you diagnose why the error occurred. This parameter is only returned by 'DescribeSnapshots' .
--
-- /Note:/ Consider using 'stateMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStateMessage :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
sStateMessage = Lens.field @"stateMessage"
{-# INLINEABLE sStateMessage #-}
{-# DEPRECATED stateMessage "Use generic-lens or generic-optics with 'stateMessage' instead"  #-}

-- | Any tags assigned to the snapshot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTags :: Lens.Lens' Snapshot (Core.Maybe [Types.Tag])
sTags = Lens.field @"tags"
{-# INLINEABLE sTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the volume that was used to create the snapshot. Snapshots created by the 'CopySnapshot' action have an arbitrary volume ID that should not be used for any purpose.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVolumeId :: Lens.Lens' Snapshot Core.Text
sVolumeId = Lens.field @"volumeId"
{-# INLINEABLE sVolumeId #-}
{-# DEPRECATED volumeId "Use generic-lens or generic-optics with 'volumeId' instead"  #-}

-- | The size of the volume, in GiB.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVolumeSize :: Lens.Lens' Snapshot Core.Int
sVolumeSize = Lens.field @"volumeSize"
{-# INLINEABLE sVolumeSize #-}
{-# DEPRECATED volumeSize "Use generic-lens or generic-optics with 'volumeSize' instead"  #-}

instance Core.FromXML Snapshot where
        parseXML x
          = Snapshot' Core.<$>
              (x Core..@? "dataEncryptionKeyId") Core.<*> x Core..@ "description"
                Core.<*> x Core..@ "encrypted"
                Core.<*> x Core..@? "kmsKeyId"
                Core.<*> x Core..@? "ownerAlias"
                Core.<*> x Core..@ "ownerId"
                Core.<*> x Core..@ "progress"
                Core.<*> x Core..@ "snapshotId"
                Core.<*> x Core..@ "startTime"
                Core.<*> x Core..@ "status"
                Core.<*> x Core..@? "statusMessage"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@ "volumeId"
                Core.<*> x Core..@ "volumeSize"
