{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Volume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Volume where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VolumeAttachment
import Network.AWS.EC2.Types.VolumeState
import Network.AWS.EC2.Types.VolumeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a volume.
--
--
--
-- /See:/ 'volume' smart constructor.
data Volume = Volume'
  { _vFastRestored :: !(Maybe Bool),
    _vMultiAttachEnabled :: !(Maybe Bool),
    _vAttachments :: !(Maybe [VolumeAttachment]),
    _vIOPS :: !(Maybe Int),
    _vOutpostARN :: !(Maybe Text),
    _vKMSKeyId :: !(Maybe Text),
    _vTags :: !(Maybe [Tag]),
    _vAvailabilityZone :: !Text,
    _vCreateTime :: !ISO8601,
    _vEncrypted :: !Bool,
    _vSize :: !Int,
    _vSnapshotId :: !Text,
    _vState :: !VolumeState,
    _vVolumeId :: !Text,
    _vVolumeType :: !VolumeType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Volume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vFastRestored' - Indicates whether the volume was created using fast snapshot restore.
--
-- * 'vMultiAttachEnabled' - Indicates whether Amazon EBS Multi-Attach is enabled.
--
-- * 'vAttachments' - Information about the volume attachments.
--
-- * 'vIOPS' - The number of I/O operations per second (IOPS) that the volume supports. For Provisioned IOPS SSD volumes, this represents the number of IOPS that are provisioned for the volume. For General Purpose SSD volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ . Constraints: Range is 100-16,000 IOPS for @gp2@ volumes and 100 to 64,000 IOPS for @io1@ and @io2@ volumes, in most Regions. The maximum IOPS for @io1@ and @io2@ of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. Condition: This parameter is required for requests to create @io1@ and @io2@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
--
-- * 'vOutpostARN' - The Amazon Resource Name (ARN) of the Outpost.
--
-- * 'vKMSKeyId' - The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the volume.
--
-- * 'vTags' - Any tags assigned to the volume.
--
-- * 'vAvailabilityZone' - The Availability Zone for the volume.
--
-- * 'vCreateTime' - The time stamp when volume creation was initiated.
--
-- * 'vEncrypted' - Indicates whether the volume is encrypted.
--
-- * 'vSize' - The size of the volume, in GiBs.
--
-- * 'vSnapshotId' - The snapshot from which the volume was created, if applicable.
--
-- * 'vState' - The volume state.
--
-- * 'vVolumeId' - The ID of the volume.
--
-- * 'vVolumeType' - The volume type. This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes.
volume ::
  -- | 'vAvailabilityZone'
  Text ->
  -- | 'vCreateTime'
  UTCTime ->
  -- | 'vEncrypted'
  Bool ->
  -- | 'vSize'
  Int ->
  -- | 'vSnapshotId'
  Text ->
  -- | 'vState'
  VolumeState ->
  -- | 'vVolumeId'
  Text ->
  -- | 'vVolumeType'
  VolumeType ->
  Volume
volume
  pAvailabilityZone_
  pCreateTime_
  pEncrypted_
  pSize_
  pSnapshotId_
  pState_
  pVolumeId_
  pVolumeType_ =
    Volume'
      { _vFastRestored = Nothing,
        _vMultiAttachEnabled = Nothing,
        _vAttachments = Nothing,
        _vIOPS = Nothing,
        _vOutpostARN = Nothing,
        _vKMSKeyId = Nothing,
        _vTags = Nothing,
        _vAvailabilityZone = pAvailabilityZone_,
        _vCreateTime = _Time # pCreateTime_,
        _vEncrypted = pEncrypted_,
        _vSize = pSize_,
        _vSnapshotId = pSnapshotId_,
        _vState = pState_,
        _vVolumeId = pVolumeId_,
        _vVolumeType = pVolumeType_
      }

-- | Indicates whether the volume was created using fast snapshot restore.
vFastRestored :: Lens' Volume (Maybe Bool)
vFastRestored = lens _vFastRestored (\s a -> s {_vFastRestored = a})

-- | Indicates whether Amazon EBS Multi-Attach is enabled.
vMultiAttachEnabled :: Lens' Volume (Maybe Bool)
vMultiAttachEnabled = lens _vMultiAttachEnabled (\s a -> s {_vMultiAttachEnabled = a})

-- | Information about the volume attachments.
vAttachments :: Lens' Volume [VolumeAttachment]
vAttachments = lens _vAttachments (\s a -> s {_vAttachments = a}) . _Default . _Coerce

-- | The number of I/O operations per second (IOPS) that the volume supports. For Provisioned IOPS SSD volumes, this represents the number of IOPS that are provisioned for the volume. For General Purpose SSD volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ . Constraints: Range is 100-16,000 IOPS for @gp2@ volumes and 100 to 64,000 IOPS for @io1@ and @io2@ volumes, in most Regions. The maximum IOPS for @io1@ and @io2@ of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. Condition: This parameter is required for requests to create @io1@ and @io2@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
vIOPS :: Lens' Volume (Maybe Int)
vIOPS = lens _vIOPS (\s a -> s {_vIOPS = a})

-- | The Amazon Resource Name (ARN) of the Outpost.
vOutpostARN :: Lens' Volume (Maybe Text)
vOutpostARN = lens _vOutpostARN (\s a -> s {_vOutpostARN = a})

-- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the volume.
vKMSKeyId :: Lens' Volume (Maybe Text)
vKMSKeyId = lens _vKMSKeyId (\s a -> s {_vKMSKeyId = a})

-- | Any tags assigned to the volume.
vTags :: Lens' Volume [Tag]
vTags = lens _vTags (\s a -> s {_vTags = a}) . _Default . _Coerce

-- | The Availability Zone for the volume.
vAvailabilityZone :: Lens' Volume Text
vAvailabilityZone = lens _vAvailabilityZone (\s a -> s {_vAvailabilityZone = a})

-- | The time stamp when volume creation was initiated.
vCreateTime :: Lens' Volume UTCTime
vCreateTime = lens _vCreateTime (\s a -> s {_vCreateTime = a}) . _Time

-- | Indicates whether the volume is encrypted.
vEncrypted :: Lens' Volume Bool
vEncrypted = lens _vEncrypted (\s a -> s {_vEncrypted = a})

-- | The size of the volume, in GiBs.
vSize :: Lens' Volume Int
vSize = lens _vSize (\s a -> s {_vSize = a})

-- | The snapshot from which the volume was created, if applicable.
vSnapshotId :: Lens' Volume Text
vSnapshotId = lens _vSnapshotId (\s a -> s {_vSnapshotId = a})

-- | The volume state.
vState :: Lens' Volume VolumeState
vState = lens _vState (\s a -> s {_vState = a})

-- | The ID of the volume.
vVolumeId :: Lens' Volume Text
vVolumeId = lens _vVolumeId (\s a -> s {_vVolumeId = a})

-- | The volume type. This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes.
vVolumeType :: Lens' Volume VolumeType
vVolumeType = lens _vVolumeType (\s a -> s {_vVolumeType = a})

instance FromXML Volume where
  parseXML x =
    Volume'
      <$> (x .@? "fastRestored")
      <*> (x .@? "multiAttachEnabled")
      <*> (x .@? "attachmentSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "iops")
      <*> (x .@? "outpostArn")
      <*> (x .@? "kmsKeyId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@ "availabilityZone")
      <*> (x .@ "createTime")
      <*> (x .@ "encrypted")
      <*> (x .@ "size")
      <*> (x .@ "snapshotId")
      <*> (x .@ "status")
      <*> (x .@ "volumeId")
      <*> (x .@ "volumeType")

instance Hashable Volume

instance NFData Volume
