{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EBSBlockDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EBSBlockDevice where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VolumeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a block device for an EBS volume.
--
--
--
-- /See:/ 'ebsBlockDevice' smart constructor.
data EBSBlockDevice = EBSBlockDevice'
  { _ebdDeleteOnTermination ::
      !(Maybe Bool),
    _ebdVolumeSize :: !(Maybe Int),
    _ebdIOPS :: !(Maybe Int),
    _ebdEncrypted :: !(Maybe Bool),
    _ebdKMSKeyId :: !(Maybe Text),
    _ebdVolumeType :: !(Maybe VolumeType),
    _ebdSnapshotId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EBSBlockDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebdDeleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/terminating-instances.html#preserving-volumes-on-termination Preserving Amazon EBS volumes on instance termination> in the Amazon Elastic Compute Cloud User Guide.
--
-- * 'ebdVolumeSize' - The size of the volume, in GiB. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size. Constraints: 1-16384 for General Purpose SSD (@gp2@ ), 4-16384 for Provisioned IOPS SSD (@io1@ and @io2@ ), 500-16384 for Throughput Optimized HDD (@st1@ ), 500-16384 for Cold HDD (@sc1@ ), and 1-1024 for Magnetic (@standard@ ) volumes. If you specify a snapshot, the volume size must be equal to or larger than the snapshot size.
--
-- * 'ebdIOPS' - The number of I/O operations per second (IOPS) that the volume supports. For @io1@ and @io2@ volumes, this represents the number of IOPS that are provisioned for the volume. For @gp2@ volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ . Constraints: Range is 100-16,000 IOPS for @gp2@ volumes and 100 to 64,000 IOPS for @io1@ and @io2@ volumes in most Regions. Maximum @io1@ and @io2@ IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . Condition: This parameter is required for requests to create @io1@ and @io2@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
--
-- * 'ebdEncrypted' - Indicates whether the encryption state of an EBS volume is changed while being restored from a backing snapshot. The effect of setting the encryption state to @true@ depends on the volume origin (new or from a snapshot), starting encryption state, ownership, and whether encryption by default is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-parameters Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ . In no case can you remove encryption from an encrypted volume. Encrypted volumes can only be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types> . This parameter is not returned by .
--
-- * 'ebdKMSKeyId' - Identifier (key ID, key alias, ID ARN, or alias ARN) for a customer managed CMK under which the EBS volume is encrypted. This parameter is only supported on @BlockDeviceMapping@ objects called by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> , <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotFleet.html RequestSpotFleet> , and <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotInstances.html RequestSpotInstances> .
--
-- * 'ebdVolumeType' - The volume type. If you set the type to @io1@ or @io2@ , you must also specify the __Iops__ parameter. If you set the type to @gp2@ , @st1@ , @sc1@ , or @standard@ , you must omit the __Iops__ parameter. Default: @gp2@
--
-- * 'ebdSnapshotId' - The ID of the snapshot.
ebsBlockDevice ::
  EBSBlockDevice
ebsBlockDevice =
  EBSBlockDevice'
    { _ebdDeleteOnTermination = Nothing,
      _ebdVolumeSize = Nothing,
      _ebdIOPS = Nothing,
      _ebdEncrypted = Nothing,
      _ebdKMSKeyId = Nothing,
      _ebdVolumeType = Nothing,
      _ebdSnapshotId = Nothing
    }

-- | Indicates whether the EBS volume is deleted on instance termination. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/terminating-instances.html#preserving-volumes-on-termination Preserving Amazon EBS volumes on instance termination> in the Amazon Elastic Compute Cloud User Guide.
ebdDeleteOnTermination :: Lens' EBSBlockDevice (Maybe Bool)
ebdDeleteOnTermination = lens _ebdDeleteOnTermination (\s a -> s {_ebdDeleteOnTermination = a})

-- | The size of the volume, in GiB. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size. Constraints: 1-16384 for General Purpose SSD (@gp2@ ), 4-16384 for Provisioned IOPS SSD (@io1@ and @io2@ ), 500-16384 for Throughput Optimized HDD (@st1@ ), 500-16384 for Cold HDD (@sc1@ ), and 1-1024 for Magnetic (@standard@ ) volumes. If you specify a snapshot, the volume size must be equal to or larger than the snapshot size.
ebdVolumeSize :: Lens' EBSBlockDevice (Maybe Int)
ebdVolumeSize = lens _ebdVolumeSize (\s a -> s {_ebdVolumeSize = a})

-- | The number of I/O operations per second (IOPS) that the volume supports. For @io1@ and @io2@ volumes, this represents the number of IOPS that are provisioned for the volume. For @gp2@ volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ . Constraints: Range is 100-16,000 IOPS for @gp2@ volumes and 100 to 64,000 IOPS for @io1@ and @io2@ volumes in most Regions. Maximum @io1@ and @io2@ IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . Condition: This parameter is required for requests to create @io1@ and @io2@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
ebdIOPS :: Lens' EBSBlockDevice (Maybe Int)
ebdIOPS = lens _ebdIOPS (\s a -> s {_ebdIOPS = a})

-- | Indicates whether the encryption state of an EBS volume is changed while being restored from a backing snapshot. The effect of setting the encryption state to @true@ depends on the volume origin (new or from a snapshot), starting encryption state, ownership, and whether encryption by default is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-parameters Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ . In no case can you remove encryption from an encrypted volume. Encrypted volumes can only be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types> . This parameter is not returned by .
ebdEncrypted :: Lens' EBSBlockDevice (Maybe Bool)
ebdEncrypted = lens _ebdEncrypted (\s a -> s {_ebdEncrypted = a})

-- | Identifier (key ID, key alias, ID ARN, or alias ARN) for a customer managed CMK under which the EBS volume is encrypted. This parameter is only supported on @BlockDeviceMapping@ objects called by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> , <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotFleet.html RequestSpotFleet> , and <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotInstances.html RequestSpotInstances> .
ebdKMSKeyId :: Lens' EBSBlockDevice (Maybe Text)
ebdKMSKeyId = lens _ebdKMSKeyId (\s a -> s {_ebdKMSKeyId = a})

-- | The volume type. If you set the type to @io1@ or @io2@ , you must also specify the __Iops__ parameter. If you set the type to @gp2@ , @st1@ , @sc1@ , or @standard@ , you must omit the __Iops__ parameter. Default: @gp2@
ebdVolumeType :: Lens' EBSBlockDevice (Maybe VolumeType)
ebdVolumeType = lens _ebdVolumeType (\s a -> s {_ebdVolumeType = a})

-- | The ID of the snapshot.
ebdSnapshotId :: Lens' EBSBlockDevice (Maybe Text)
ebdSnapshotId = lens _ebdSnapshotId (\s a -> s {_ebdSnapshotId = a})

instance FromXML EBSBlockDevice where
  parseXML x =
    EBSBlockDevice'
      <$> (x .@? "deleteOnTermination")
      <*> (x .@? "volumeSize")
      <*> (x .@? "iops")
      <*> (x .@? "encrypted")
      <*> (x .@? "KmsKeyId")
      <*> (x .@? "volumeType")
      <*> (x .@? "snapshotId")

instance Hashable EBSBlockDevice

instance NFData EBSBlockDevice

instance ToQuery EBSBlockDevice where
  toQuery EBSBlockDevice' {..} =
    mconcat
      [ "DeleteOnTermination" =: _ebdDeleteOnTermination,
        "VolumeSize" =: _ebdVolumeSize,
        "Iops" =: _ebdIOPS,
        "Encrypted" =: _ebdEncrypted,
        "KmsKeyId" =: _ebdKMSKeyId,
        "VolumeType" =: _ebdVolumeType,
        "SnapshotId" =: _ebdSnapshotId
      ]
