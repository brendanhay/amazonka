{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.EBS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.EBS where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes information used to set up an Amazon EBS volume specified in a block device mapping.
--
--
--
-- /See:/ 'ebs' smart constructor.
data EBS = EBS'
  { _ebsDeleteOnTermination :: !(Maybe Bool),
    _ebsVolumeSize :: !(Maybe Nat),
    _ebsIOPS :: !(Maybe Nat),
    _ebsEncrypted :: !(Maybe Bool),
    _ebsVolumeType :: !(Maybe Text),
    _ebsSnapshotId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EBS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebsDeleteOnTermination' - Indicates whether the volume is deleted on instance termination. For Amazon EC2 Auto Scaling, the default value is @true@ .
--
-- * 'ebsVolumeSize' - The volume size, in Gibibytes (GiB). This can be a number from 1-1,024 for @standard@ , 4-16,384 for @io1@ , 1-16,384 for @gp2@ , and 500-16,384 for @st1@ and @sc1@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size. Default: If you create a volume from a snapshot and you don't specify a volume size, the default is the snapshot size. You must specify either a @VolumeSize@ or a @SnapshotId@ . If you specify both @SnapshotId@ and @VolumeSize@ , the volume size must be equal or greater than the size of the snapshot.
--
-- * 'ebsIOPS' - The number of I/O operations per second (IOPS) to provision for the volume. The maximum ratio of IOPS to volume size (in GiB) is 50:1. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon EC2 User Guide for Linux Instances/ . Required when the volume type is @io1@ . (Not used with @standard@ , @gp2@ , @st1@ , or @sc1@ volumes.)
--
-- * 'ebsEncrypted' - Specifies whether the volume should be encrypted. Encrypted EBS volumes can only be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported Instance Types> . If your AMI uses encrypted volumes, you can also only launch it on supported instance types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AMIEncryption.html Using Encryption with EBS-Backed AMIs> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/key-policy-requirements-EBS-encryption.html Required CMK key policy for use with encrypted volumes> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'ebsVolumeType' - The volume type, which can be @standard@ for Magnetic, @io1@ for Provisioned IOPS SSD, @gp2@ for General Purpose SSD, @st1@ for Throughput Optimized HDD, or @sc1@ for Cold HDD. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon EC2 User Guide for Linux Instances/ . Valid Values: @standard@ | @io1@ | @gp2@ | @st1@ | @sc1@
--
-- * 'ebsSnapshotId' - The snapshot ID of the volume to use. You must specify either a @VolumeSize@ or a @SnapshotId@ .
ebs ::
  EBS
ebs =
  EBS'
    { _ebsDeleteOnTermination = Nothing,
      _ebsVolumeSize = Nothing,
      _ebsIOPS = Nothing,
      _ebsEncrypted = Nothing,
      _ebsVolumeType = Nothing,
      _ebsSnapshotId = Nothing
    }

-- | Indicates whether the volume is deleted on instance termination. For Amazon EC2 Auto Scaling, the default value is @true@ .
ebsDeleteOnTermination :: Lens' EBS (Maybe Bool)
ebsDeleteOnTermination = lens _ebsDeleteOnTermination (\s a -> s {_ebsDeleteOnTermination = a})

-- | The volume size, in Gibibytes (GiB). This can be a number from 1-1,024 for @standard@ , 4-16,384 for @io1@ , 1-16,384 for @gp2@ , and 500-16,384 for @st1@ and @sc1@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size. Default: If you create a volume from a snapshot and you don't specify a volume size, the default is the snapshot size. You must specify either a @VolumeSize@ or a @SnapshotId@ . If you specify both @SnapshotId@ and @VolumeSize@ , the volume size must be equal or greater than the size of the snapshot.
ebsVolumeSize :: Lens' EBS (Maybe Natural)
ebsVolumeSize = lens _ebsVolumeSize (\s a -> s {_ebsVolumeSize = a}) . mapping _Nat

-- | The number of I/O operations per second (IOPS) to provision for the volume. The maximum ratio of IOPS to volume size (in GiB) is 50:1. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon EC2 User Guide for Linux Instances/ . Required when the volume type is @io1@ . (Not used with @standard@ , @gp2@ , @st1@ , or @sc1@ volumes.)
ebsIOPS :: Lens' EBS (Maybe Natural)
ebsIOPS = lens _ebsIOPS (\s a -> s {_ebsIOPS = a}) . mapping _Nat

-- | Specifies whether the volume should be encrypted. Encrypted EBS volumes can only be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported Instance Types> . If your AMI uses encrypted volumes, you can also only launch it on supported instance types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AMIEncryption.html Using Encryption with EBS-Backed AMIs> in the /Amazon EC2 User Guide for Linux Instances/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/key-policy-requirements-EBS-encryption.html Required CMK key policy for use with encrypted volumes> in the /Amazon EC2 Auto Scaling User Guide/ .
ebsEncrypted :: Lens' EBS (Maybe Bool)
ebsEncrypted = lens _ebsEncrypted (\s a -> s {_ebsEncrypted = a})

-- | The volume type, which can be @standard@ for Magnetic, @io1@ for Provisioned IOPS SSD, @gp2@ for General Purpose SSD, @st1@ for Throughput Optimized HDD, or @sc1@ for Cold HDD. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon EC2 User Guide for Linux Instances/ . Valid Values: @standard@ | @io1@ | @gp2@ | @st1@ | @sc1@
ebsVolumeType :: Lens' EBS (Maybe Text)
ebsVolumeType = lens _ebsVolumeType (\s a -> s {_ebsVolumeType = a})

-- | The snapshot ID of the volume to use. You must specify either a @VolumeSize@ or a @SnapshotId@ .
ebsSnapshotId :: Lens' EBS (Maybe Text)
ebsSnapshotId = lens _ebsSnapshotId (\s a -> s {_ebsSnapshotId = a})

instance FromXML EBS where
  parseXML x =
    EBS'
      <$> (x .@? "DeleteOnTermination")
      <*> (x .@? "VolumeSize")
      <*> (x .@? "Iops")
      <*> (x .@? "Encrypted")
      <*> (x .@? "VolumeType")
      <*> (x .@? "SnapshotId")

instance Hashable EBS

instance NFData EBS

instance ToQuery EBS where
  toQuery EBS' {..} =
    mconcat
      [ "DeleteOnTermination" =: _ebsDeleteOnTermination,
        "VolumeSize" =: _ebsVolumeSize,
        "Iops" =: _ebsIOPS,
        "Encrypted" =: _ebsEncrypted,
        "VolumeType" =: _ebsVolumeType,
        "SnapshotId" =: _ebsSnapshotId
      ]
