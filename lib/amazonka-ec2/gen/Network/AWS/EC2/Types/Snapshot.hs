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
    sfStateMessage,
    sfState,
    sfOwnerAlias,
    sfProgress,
    sfStartTime,
    sfVolumeSize,
    sfDataEncryptionKeyId,
    sfEncrypted,
    sfOwnerId,
    sfKMSKeyId,
    sfVolumeId,
    sfDescription,
    sfTags,
    sfSnapshotId,
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
  { -- | Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot copy operation fails (for example, if the proper AWS Key Management Service (AWS KMS) permissions are not obtained) this field displays error state details to help you diagnose why the error occurred. This parameter is only returned by 'DescribeSnapshots' .
    stateMessage :: Lude.Maybe Lude.Text,
    -- | The snapshot state.
    state :: SnapshotState,
    -- | The AWS owner alias, from an Amazon-maintained list (@amazon@ ). This is not the user-configured AWS account alias set using the IAM console.
    ownerAlias :: Lude.Maybe Lude.Text,
    -- | The progress of the snapshot, as a percentage.
    progress :: Lude.Text,
    -- | The time stamp when the snapshot was initiated.
    startTime :: Lude.DateTime,
    -- | The size of the volume, in GiB.
    volumeSize :: Lude.Int,
    -- | The data encryption key identifier for the snapshot. This value is a unique identifier that corresponds to the data encryption key that was used to encrypt the original volume or snapshot copy. Because data encryption keys are inherited by volumes created from snapshots, and vice versa, if snapshots share the same data encryption key identifier, then they belong to the same volume/snapshot lineage. This parameter is only returned by 'DescribeSnapshots' .
    dataEncryptionKeyId :: Lude.Maybe Lude.Text,
    -- | Indicates whether the snapshot is encrypted.
    encrypted :: Lude.Bool,
    -- | The AWS account ID of the EBS snapshot owner.
    ownerId :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the parent volume.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The ID of the volume that was used to create the snapshot. Snapshots created by the 'CopySnapshot' action have an arbitrary volume ID that should not be used for any purpose.
    volumeId :: Lude.Text,
    -- | The description for the snapshot.
    description :: Lude.Text,
    -- | Any tags assigned to the snapshot.
    tags :: Lude.Maybe [Tag],
    -- | The ID of the snapshot. Each snapshot receives a unique identifier when it is created.
    snapshotId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- * 'stateMessage' - Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot copy operation fails (for example, if the proper AWS Key Management Service (AWS KMS) permissions are not obtained) this field displays error state details to help you diagnose why the error occurred. This parameter is only returned by 'DescribeSnapshots' .
-- * 'state' - The snapshot state.
-- * 'ownerAlias' - The AWS owner alias, from an Amazon-maintained list (@amazon@ ). This is not the user-configured AWS account alias set using the IAM console.
-- * 'progress' - The progress of the snapshot, as a percentage.
-- * 'startTime' - The time stamp when the snapshot was initiated.
-- * 'volumeSize' - The size of the volume, in GiB.
-- * 'dataEncryptionKeyId' - The data encryption key identifier for the snapshot. This value is a unique identifier that corresponds to the data encryption key that was used to encrypt the original volume or snapshot copy. Because data encryption keys are inherited by volumes created from snapshots, and vice versa, if snapshots share the same data encryption key identifier, then they belong to the same volume/snapshot lineage. This parameter is only returned by 'DescribeSnapshots' .
-- * 'encrypted' - Indicates whether the snapshot is encrypted.
-- * 'ownerId' - The AWS account ID of the EBS snapshot owner.
-- * 'kmsKeyId' - The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the parent volume.
-- * 'volumeId' - The ID of the volume that was used to create the snapshot. Snapshots created by the 'CopySnapshot' action have an arbitrary volume ID that should not be used for any purpose.
-- * 'description' - The description for the snapshot.
-- * 'tags' - Any tags assigned to the snapshot.
-- * 'snapshotId' - The ID of the snapshot. Each snapshot receives a unique identifier when it is created.
mkSnapshot ::
  -- | 'state'
  SnapshotState ->
  -- | 'progress'
  Lude.Text ->
  -- | 'startTime'
  Lude.DateTime ->
  -- | 'volumeSize'
  Lude.Int ->
  -- | 'encrypted'
  Lude.Bool ->
  -- | 'ownerId'
  Lude.Text ->
  -- | 'volumeId'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  -- | 'snapshotId'
  Lude.Text ->
  Snapshot
mkSnapshot
  pState_
  pProgress_
  pStartTime_
  pVolumeSize_
  pEncrypted_
  pOwnerId_
  pVolumeId_
  pDescription_
  pSnapshotId_ =
    Snapshot'
      { stateMessage = Lude.Nothing,
        state = pState_,
        ownerAlias = Lude.Nothing,
        progress = pProgress_,
        startTime = pStartTime_,
        volumeSize = pVolumeSize_,
        dataEncryptionKeyId = Lude.Nothing,
        encrypted = pEncrypted_,
        ownerId = pOwnerId_,
        kmsKeyId = Lude.Nothing,
        volumeId = pVolumeId_,
        description = pDescription_,
        tags = Lude.Nothing,
        snapshotId = pSnapshotId_
      }

-- | Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot copy operation fails (for example, if the proper AWS Key Management Service (AWS KMS) permissions are not obtained) this field displays error state details to help you diagnose why the error occurred. This parameter is only returned by 'DescribeSnapshots' .
--
-- /Note:/ Consider using 'stateMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStateMessage :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sfStateMessage = Lens.lens (stateMessage :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {stateMessage = a} :: Snapshot)
{-# DEPRECATED sfStateMessage "Use generic-lens or generic-optics with 'stateMessage' instead." #-}

-- | The snapshot state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfState :: Lens.Lens' Snapshot SnapshotState
sfState = Lens.lens (state :: Snapshot -> SnapshotState) (\s a -> s {state = a} :: Snapshot)
{-# DEPRECATED sfState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The AWS owner alias, from an Amazon-maintained list (@amazon@ ). This is not the user-configured AWS account alias set using the IAM console.
--
-- /Note:/ Consider using 'ownerAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfOwnerAlias :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sfOwnerAlias = Lens.lens (ownerAlias :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {ownerAlias = a} :: Snapshot)
{-# DEPRECATED sfOwnerAlias "Use generic-lens or generic-optics with 'ownerAlias' instead." #-}

-- | The progress of the snapshot, as a percentage.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfProgress :: Lens.Lens' Snapshot Lude.Text
sfProgress = Lens.lens (progress :: Snapshot -> Lude.Text) (\s a -> s {progress = a} :: Snapshot)
{-# DEPRECATED sfProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The time stamp when the snapshot was initiated.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStartTime :: Lens.Lens' Snapshot Lude.DateTime
sfStartTime = Lens.lens (startTime :: Snapshot -> Lude.DateTime) (\s a -> s {startTime = a} :: Snapshot)
{-# DEPRECATED sfStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The size of the volume, in GiB.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfVolumeSize :: Lens.Lens' Snapshot Lude.Int
sfVolumeSize = Lens.lens (volumeSize :: Snapshot -> Lude.Int) (\s a -> s {volumeSize = a} :: Snapshot)
{-# DEPRECATED sfVolumeSize "Use generic-lens or generic-optics with 'volumeSize' instead." #-}

-- | The data encryption key identifier for the snapshot. This value is a unique identifier that corresponds to the data encryption key that was used to encrypt the original volume or snapshot copy. Because data encryption keys are inherited by volumes created from snapshots, and vice versa, if snapshots share the same data encryption key identifier, then they belong to the same volume/snapshot lineage. This parameter is only returned by 'DescribeSnapshots' .
--
-- /Note:/ Consider using 'dataEncryptionKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDataEncryptionKeyId :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sfDataEncryptionKeyId = Lens.lens (dataEncryptionKeyId :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {dataEncryptionKeyId = a} :: Snapshot)
{-# DEPRECATED sfDataEncryptionKeyId "Use generic-lens or generic-optics with 'dataEncryptionKeyId' instead." #-}

-- | Indicates whether the snapshot is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfEncrypted :: Lens.Lens' Snapshot Lude.Bool
sfEncrypted = Lens.lens (encrypted :: Snapshot -> Lude.Bool) (\s a -> s {encrypted = a} :: Snapshot)
{-# DEPRECATED sfEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The AWS account ID of the EBS snapshot owner.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfOwnerId :: Lens.Lens' Snapshot Lude.Text
sfOwnerId = Lens.lens (ownerId :: Snapshot -> Lude.Text) (\s a -> s {ownerId = a} :: Snapshot)
{-# DEPRECATED sfOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the parent volume.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfKMSKeyId :: Lens.Lens' Snapshot (Lude.Maybe Lude.Text)
sfKMSKeyId = Lens.lens (kmsKeyId :: Snapshot -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: Snapshot)
{-# DEPRECATED sfKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The ID of the volume that was used to create the snapshot. Snapshots created by the 'CopySnapshot' action have an arbitrary volume ID that should not be used for any purpose.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfVolumeId :: Lens.Lens' Snapshot Lude.Text
sfVolumeId = Lens.lens (volumeId :: Snapshot -> Lude.Text) (\s a -> s {volumeId = a} :: Snapshot)
{-# DEPRECATED sfVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The description for the snapshot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDescription :: Lens.Lens' Snapshot Lude.Text
sfDescription = Lens.lens (description :: Snapshot -> Lude.Text) (\s a -> s {description = a} :: Snapshot)
{-# DEPRECATED sfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Any tags assigned to the snapshot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfTags :: Lens.Lens' Snapshot (Lude.Maybe [Tag])
sfTags = Lens.lens (tags :: Snapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Snapshot)
{-# DEPRECATED sfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the snapshot. Each snapshot receives a unique identifier when it is created.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfSnapshotId :: Lens.Lens' Snapshot Lude.Text
sfSnapshotId = Lens.lens (snapshotId :: Snapshot -> Lude.Text) (\s a -> s {snapshotId = a} :: Snapshot)
{-# DEPRECATED sfSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.FromXML Snapshot where
  parseXML x =
    Snapshot'
      Lude.<$> (x Lude..@? "statusMessage")
      Lude.<*> (x Lude..@ "status")
      Lude.<*> (x Lude..@? "ownerAlias")
      Lude.<*> (x Lude..@ "progress")
      Lude.<*> (x Lude..@ "startTime")
      Lude.<*> (x Lude..@ "volumeSize")
      Lude.<*> (x Lude..@? "dataEncryptionKeyId")
      Lude.<*> (x Lude..@ "encrypted")
      Lude.<*> (x Lude..@ "ownerId")
      Lude.<*> (x Lude..@? "kmsKeyId")
      Lude.<*> (x Lude..@ "volumeId")
      Lude.<*> (x Lude..@ "description")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@ "snapshotId")
