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
    sStateMessage,
    sOwnerAlias,
    sDataEncryptionKeyId,
    sKMSKeyId,
    sTags,
    sSnapshotId,
    sOwnerId,
    sVolumeId,
    sVolumeSize,
    sDescription,
    sStartTime,
    sProgress,
    sState,
    sEncrypted,
  )
where

import Network.AWS.EC2.Types.SnapshotState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a snapshot.
--
-- /See:/ 'mkSnapshot' smart constructor.
data Snapshot = Snapshot'
  { stateMessage :: Lude.Maybe Lude.Text,
    ownerAlias :: Lude.Maybe Lude.Text,
    dataEncryptionKeyId :: Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    snapshotId :: Lude.Text,
    ownerId :: Lude.Text,
    volumeId :: Lude.Text,
    volumeSize :: Lude.Int,
    description :: Lude.Text,
    startTime :: Lude.ISO8601,
    progress :: Lude.Text,
    state :: SnapshotState,
    encrypted :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- * 'dataEncryptionKeyId' - The data encryption key identifier for the snapshot. This value is a unique identifier that corresponds to the data encryption key that was used to encrypt the original volume or snapshot copy. Because data encryption keys are inherited by volumes created from snapshots, and vice versa, if snapshots share the same data encryption key identifier, then they belong to the same volume/snapshot lineage. This parameter is only returned by 'DescribeSnapshots' .
-- * 'description' - The description for the snapshot.
-- * 'encrypted' - Indicates whether the snapshot is encrypted.
-- * 'kmsKeyId' - The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the parent volume.
-- * 'ownerAlias' - The AWS owner alias, from an Amazon-maintained list (@amazon@ ). This is not the user-configured AWS account alias set using the IAM console.
-- * 'ownerId' - The AWS account ID of the EBS snapshot owner.
-- * 'progress' - The progress of the snapshot, as a percentage.
-- * 'snapshotId' - The ID of the snapshot. Each snapshot receives a unique identifier when it is created.
-- * 'startTime' - The time stamp when the snapshot was initiated.
-- * 'state' - The snapshot state.
-- * 'stateMessage' - Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot copy operation fails (for example, if the proper AWS Key Management Service (AWS KMS) permissions are not obtained) this field displays error state details to help you diagnose why the error occurred. This parameter is only returned by 'DescribeSnapshots' .
-- * 'tags' - Any tags assigned to the snapshot.
-- * 'volumeId' - The ID of the volume that was used to create the snapshot. Snapshots created by the 'CopySnapshot' action have an arbitrary volume ID that should not be used for any purpose.
-- * 'volumeSize' - The size of the volume, in GiB.
mkSnapshot ::
  -- | 'snapshotId'
  Lude.Text ->
  -- | 'ownerId'
  Lude.Text ->
  -- | 'volumeId'
  Lude.Text ->
  -- | 'volumeSize'
  Lude.Int ->
  -- | 'description'
  Lude.Text ->
  -- | 'startTime'
  Lude.ISO8601 ->
  -- | 'progress'
  Lude.Text ->
  -- | 'state'
  SnapshotState ->
  -- | 'encrypted'
  Lude.Bool ->
  Snapshot
mkSnapshot
  pSnapshotId_
  pOwnerId_
  pVolumeId_
  pVolumeSize_
  pDescription_
  pStartTime_
  pProgress_
  pState_
  pEncrypted_ =
    Snapshot'
      { stateMessage = Lude.Nothing,
        ownerAlias = Lude.Nothing,
        dataEncryptionKeyId = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        tags = Lude.Nothing,
        snapshotId = pSnapshotId_,
        ownerId = pOwnerId_,
        volumeId = pVolumeId_,
        volumeSize = pVolumeSize_,
        description = pDescription_,
        startTime = pStartTime_,
        progress = pProgress_,
        state = pState_,
        encrypted = pEncrypted_
      }

-- | Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot copy operation fails (for example, if the proper AWS Key Management Service (AWS KMS) permissions are not obtained) this field displays error state details to help you diagnose why the error occurred. This parameter is only returned by 'DescribeSnapshots' .
--
-- /Note:/ Consider using 'stateMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStateMessage :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sStateMessage = Lens.lens (stateMessage :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {stateMessage = a} :: Snapshot)
{-# DEPRECATED sStateMessage "Use generic-lens or generic-optics with 'stateMessage' instead." #-}

-- | The AWS owner alias, from an Amazon-maintained list (@amazon@ ). This is not the user-configured AWS account alias set using the IAM console.
--
-- /Note:/ Consider using 'ownerAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwnerAlias :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sOwnerAlias = Lens.lens (ownerAlias :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {ownerAlias = a} :: Snapshot)
{-# DEPRECATED sOwnerAlias "Use generic-lens or generic-optics with 'ownerAlias' instead." #-}

-- | The data encryption key identifier for the snapshot. This value is a unique identifier that corresponds to the data encryption key that was used to encrypt the original volume or snapshot copy. Because data encryption keys are inherited by volumes created from snapshots, and vice versa, if snapshots share the same data encryption key identifier, then they belong to the same volume/snapshot lineage. This parameter is only returned by 'DescribeSnapshots' .
--
-- /Note:/ Consider using 'dataEncryptionKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDataEncryptionKeyId :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sDataEncryptionKeyId = Lens.lens (dataEncryptionKeyId :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {dataEncryptionKeyId = a} :: Snapshot)
{-# DEPRECATED sDataEncryptionKeyId "Use generic-lens or generic-optics with 'dataEncryptionKeyId' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the parent volume.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKMSKeyId :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sKMSKeyId = Lens.lens (kmsKeyId :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: Snapshot)
{-# DEPRECATED sKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Any tags assigned to the snapshot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTags :: Lens.Lens' Snapshot (Lude.Maybe [Tag])
sTags = Lens.lens (tags :: Snapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Snapshot)
{-# DEPRECATED sTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the snapshot. Each snapshot receives a unique identifier when it is created.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotId :: Lens.Lens' Snapshot Lude.Text
sSnapshotId = Lens.lens (snapshotId :: Snapshot -> Lude.Text) (\s a -> s {snapshotId = a} :: Snapshot)
{-# DEPRECATED sSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The AWS account ID of the EBS snapshot owner.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwnerId :: Lens.Lens' Snapshot Lude.Text
sOwnerId = Lens.lens (ownerId :: Snapshot -> Lude.Text) (\s a -> s {ownerId = a} :: Snapshot)
{-# DEPRECATED sOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The ID of the volume that was used to create the snapshot. Snapshots created by the 'CopySnapshot' action have an arbitrary volume ID that should not be used for any purpose.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVolumeId :: Lens.Lens' Snapshot Lude.Text
sVolumeId = Lens.lens (volumeId :: Snapshot -> Lude.Text) (\s a -> s {volumeId = a} :: Snapshot)
{-# DEPRECATED sVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The size of the volume, in GiB.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVolumeSize :: Lens.Lens' Snapshot Lude.Int
sVolumeSize = Lens.lens (volumeSize :: Snapshot -> Lude.Int) (\s a -> s {volumeSize = a} :: Snapshot)
{-# DEPRECATED sVolumeSize "Use generic-lens or generic-optics with 'volumeSize' instead." #-}

-- | The description for the snapshot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDescription :: Lens.Lens' Snapshot Lude.Text
sDescription = Lens.lens (description :: Snapshot -> Lude.Text) (\s a -> s {description = a} :: Snapshot)
{-# DEPRECATED sDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The time stamp when the snapshot was initiated.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartTime :: Lens.Lens' Snapshot Lude.ISO8601
sStartTime = Lens.lens (startTime :: Snapshot -> Lude.ISO8601) (\s a -> s {startTime = a} :: Snapshot)
{-# DEPRECATED sStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The progress of the snapshot, as a percentage.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sProgress :: Lens.Lens' Snapshot Lude.Text
sProgress = Lens.lens (progress :: Snapshot -> Lude.Text) (\s a -> s {progress = a} :: Snapshot)
{-# DEPRECATED sProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The snapshot state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sState :: Lens.Lens' Snapshot SnapshotState
sState = Lens.lens (state :: Snapshot -> SnapshotState) (\s a -> s {state = a} :: Snapshot)
{-# DEPRECATED sState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Indicates whether the snapshot is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEncrypted :: Lens.Lens' Snapshot Lude.Bool
sEncrypted = Lens.lens (encrypted :: Snapshot -> Lude.Bool) (\s a -> s {encrypted = a} :: Snapshot)
{-# DEPRECATED sEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

instance Lude.FromXML Snapshot where
  parseXML x =
    Snapshot'
      Lude.<$> (x Lude..@? "statusMessage")
      Lude.<*> (x Lude..@? "ownerAlias")
      Lude.<*> (x Lude..@? "dataEncryptionKeyId")
      Lude.<*> (x Lude..@? "kmsKeyId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@ "snapshotId")
      Lude.<*> (x Lude..@ "ownerId")
      Lude.<*> (x Lude..@ "volumeId")
      Lude.<*> (x Lude..@ "volumeSize")
      Lude.<*> (x Lude..@ "description")
      Lude.<*> (x Lude..@ "startTime")
      Lude.<*> (x Lude..@ "progress")
      Lude.<*> (x Lude..@ "status")
      Lude.<*> (x Lude..@ "encrypted")
