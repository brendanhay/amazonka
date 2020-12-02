{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Snapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Snapshot where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SnapshotState
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a snapshot.
--
--
--
-- /See:/ 'snapshot' smart constructor.
data Snapshot = Snapshot'
  { _sStateMessage :: !(Maybe Text),
    _sOwnerAlias :: !(Maybe Text),
    _sDataEncryptionKeyId :: !(Maybe Text),
    _sKMSKeyId :: !(Maybe Text),
    _sTags :: !(Maybe [Tag]),
    _sSnapshotId :: !Text,
    _sOwnerId :: !Text,
    _sVolumeId :: !Text,
    _sVolumeSize :: !Int,
    _sDescription :: !Text,
    _sStartTime :: !ISO8601,
    _sProgress :: !Text,
    _sState :: !SnapshotState,
    _sEncrypted :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStateMessage' - Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot copy operation fails (for example, if the proper AWS Key Management Service (AWS KMS) permissions are not obtained) this field displays error state details to help you diagnose why the error occurred. This parameter is only returned by 'DescribeSnapshots' .
--
-- * 'sOwnerAlias' - The AWS owner alias, from an Amazon-maintained list (@amazon@ ). This is not the user-configured AWS account alias set using the IAM console.
--
-- * 'sDataEncryptionKeyId' - The data encryption key identifier for the snapshot. This value is a unique identifier that corresponds to the data encryption key that was used to encrypt the original volume or snapshot copy. Because data encryption keys are inherited by volumes created from snapshots, and vice versa, if snapshots share the same data encryption key identifier, then they belong to the same volume/snapshot lineage. This parameter is only returned by 'DescribeSnapshots' .
--
-- * 'sKMSKeyId' - The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the parent volume.
--
-- * 'sTags' - Any tags assigned to the snapshot.
--
-- * 'sSnapshotId' - The ID of the snapshot. Each snapshot receives a unique identifier when it is created.
--
-- * 'sOwnerId' - The AWS account ID of the EBS snapshot owner.
--
-- * 'sVolumeId' - The ID of the volume that was used to create the snapshot. Snapshots created by the 'CopySnapshot' action have an arbitrary volume ID that should not be used for any purpose.
--
-- * 'sVolumeSize' - The size of the volume, in GiB.
--
-- * 'sDescription' - The description for the snapshot.
--
-- * 'sStartTime' - The time stamp when the snapshot was initiated.
--
-- * 'sProgress' - The progress of the snapshot, as a percentage.
--
-- * 'sState' - The snapshot state.
--
-- * 'sEncrypted' - Indicates whether the snapshot is encrypted.
snapshot ::
  -- | 'sSnapshotId'
  Text ->
  -- | 'sOwnerId'
  Text ->
  -- | 'sVolumeId'
  Text ->
  -- | 'sVolumeSize'
  Int ->
  -- | 'sDescription'
  Text ->
  -- | 'sStartTime'
  UTCTime ->
  -- | 'sProgress'
  Text ->
  -- | 'sState'
  SnapshotState ->
  -- | 'sEncrypted'
  Bool ->
  Snapshot
snapshot
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
      { _sStateMessage = Nothing,
        _sOwnerAlias = Nothing,
        _sDataEncryptionKeyId = Nothing,
        _sKMSKeyId = Nothing,
        _sTags = Nothing,
        _sSnapshotId = pSnapshotId_,
        _sOwnerId = pOwnerId_,
        _sVolumeId = pVolumeId_,
        _sVolumeSize = pVolumeSize_,
        _sDescription = pDescription_,
        _sStartTime = _Time # pStartTime_,
        _sProgress = pProgress_,
        _sState = pState_,
        _sEncrypted = pEncrypted_
      }

-- | Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot copy operation fails (for example, if the proper AWS Key Management Service (AWS KMS) permissions are not obtained) this field displays error state details to help you diagnose why the error occurred. This parameter is only returned by 'DescribeSnapshots' .
sStateMessage :: Lens' Snapshot (Maybe Text)
sStateMessage = lens _sStateMessage (\s a -> s {_sStateMessage = a})

-- | The AWS owner alias, from an Amazon-maintained list (@amazon@ ). This is not the user-configured AWS account alias set using the IAM console.
sOwnerAlias :: Lens' Snapshot (Maybe Text)
sOwnerAlias = lens _sOwnerAlias (\s a -> s {_sOwnerAlias = a})

-- | The data encryption key identifier for the snapshot. This value is a unique identifier that corresponds to the data encryption key that was used to encrypt the original volume or snapshot copy. Because data encryption keys are inherited by volumes created from snapshots, and vice versa, if snapshots share the same data encryption key identifier, then they belong to the same volume/snapshot lineage. This parameter is only returned by 'DescribeSnapshots' .
sDataEncryptionKeyId :: Lens' Snapshot (Maybe Text)
sDataEncryptionKeyId = lens _sDataEncryptionKeyId (\s a -> s {_sDataEncryptionKeyId = a})

-- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the parent volume.
sKMSKeyId :: Lens' Snapshot (Maybe Text)
sKMSKeyId = lens _sKMSKeyId (\s a -> s {_sKMSKeyId = a})

-- | Any tags assigned to the snapshot.
sTags :: Lens' Snapshot [Tag]
sTags = lens _sTags (\s a -> s {_sTags = a}) . _Default . _Coerce

-- | The ID of the snapshot. Each snapshot receives a unique identifier when it is created.
sSnapshotId :: Lens' Snapshot Text
sSnapshotId = lens _sSnapshotId (\s a -> s {_sSnapshotId = a})

-- | The AWS account ID of the EBS snapshot owner.
sOwnerId :: Lens' Snapshot Text
sOwnerId = lens _sOwnerId (\s a -> s {_sOwnerId = a})

-- | The ID of the volume that was used to create the snapshot. Snapshots created by the 'CopySnapshot' action have an arbitrary volume ID that should not be used for any purpose.
sVolumeId :: Lens' Snapshot Text
sVolumeId = lens _sVolumeId (\s a -> s {_sVolumeId = a})

-- | The size of the volume, in GiB.
sVolumeSize :: Lens' Snapshot Int
sVolumeSize = lens _sVolumeSize (\s a -> s {_sVolumeSize = a})

-- | The description for the snapshot.
sDescription :: Lens' Snapshot Text
sDescription = lens _sDescription (\s a -> s {_sDescription = a})

-- | The time stamp when the snapshot was initiated.
sStartTime :: Lens' Snapshot UTCTime
sStartTime = lens _sStartTime (\s a -> s {_sStartTime = a}) . _Time

-- | The progress of the snapshot, as a percentage.
sProgress :: Lens' Snapshot Text
sProgress = lens _sProgress (\s a -> s {_sProgress = a})

-- | The snapshot state.
sState :: Lens' Snapshot SnapshotState
sState = lens _sState (\s a -> s {_sState = a})

-- | Indicates whether the snapshot is encrypted.
sEncrypted :: Lens' Snapshot Bool
sEncrypted = lens _sEncrypted (\s a -> s {_sEncrypted = a})

instance FromXML Snapshot where
  parseXML x =
    Snapshot'
      <$> (x .@? "statusMessage")
      <*> (x .@? "ownerAlias")
      <*> (x .@? "dataEncryptionKeyId")
      <*> (x .@? "kmsKeyId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@ "snapshotId")
      <*> (x .@ "ownerId")
      <*> (x .@ "volumeId")
      <*> (x .@ "volumeSize")
      <*> (x .@ "description")
      <*> (x .@ "startTime")
      <*> (x .@ "progress")
      <*> (x .@ "status")
      <*> (x .@ "encrypted")

instance Hashable Snapshot

instance NFData Snapshot
