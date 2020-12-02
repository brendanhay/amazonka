{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesEBS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesEBS where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an EBS volume for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstancesEBS' smart constructor.
data ScheduledInstancesEBS = ScheduledInstancesEBS'
  { _sieDeleteOnTermination ::
      !(Maybe Bool),
    _sieVolumeSize :: !(Maybe Int),
    _sieIOPS :: !(Maybe Int),
    _sieEncrypted :: !(Maybe Bool),
    _sieVolumeType :: !(Maybe Text),
    _sieSnapshotId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledInstancesEBS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sieDeleteOnTermination' - Indicates whether the volume is deleted on instance termination.
--
-- * 'sieVolumeSize' - The size of the volume, in GiB. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
--
-- * 'sieIOPS' - The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
--
-- * 'sieEncrypted' - Indicates whether the volume is encrypted. You can attached encrypted volumes only to instances that support them.
--
-- * 'sieVolumeType' - The volume type. @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, Throughput Optimized HDD for @st1@ , Cold HDD for @sc1@ , or @standard@ for Magnetic. Default: @gp2@
--
-- * 'sieSnapshotId' - The ID of the snapshot.
scheduledInstancesEBS ::
  ScheduledInstancesEBS
scheduledInstancesEBS =
  ScheduledInstancesEBS'
    { _sieDeleteOnTermination = Nothing,
      _sieVolumeSize = Nothing,
      _sieIOPS = Nothing,
      _sieEncrypted = Nothing,
      _sieVolumeType = Nothing,
      _sieSnapshotId = Nothing
    }

-- | Indicates whether the volume is deleted on instance termination.
sieDeleteOnTermination :: Lens' ScheduledInstancesEBS (Maybe Bool)
sieDeleteOnTermination = lens _sieDeleteOnTermination (\s a -> s {_sieDeleteOnTermination = a})

-- | The size of the volume, in GiB. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
sieVolumeSize :: Lens' ScheduledInstancesEBS (Maybe Int)
sieVolumeSize = lens _sieVolumeSize (\s a -> s {_sieVolumeSize = a})

-- | The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
sieIOPS :: Lens' ScheduledInstancesEBS (Maybe Int)
sieIOPS = lens _sieIOPS (\s a -> s {_sieIOPS = a})

-- | Indicates whether the volume is encrypted. You can attached encrypted volumes only to instances that support them.
sieEncrypted :: Lens' ScheduledInstancesEBS (Maybe Bool)
sieEncrypted = lens _sieEncrypted (\s a -> s {_sieEncrypted = a})

-- | The volume type. @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, Throughput Optimized HDD for @st1@ , Cold HDD for @sc1@ , or @standard@ for Magnetic. Default: @gp2@
sieVolumeType :: Lens' ScheduledInstancesEBS (Maybe Text)
sieVolumeType = lens _sieVolumeType (\s a -> s {_sieVolumeType = a})

-- | The ID of the snapshot.
sieSnapshotId :: Lens' ScheduledInstancesEBS (Maybe Text)
sieSnapshotId = lens _sieSnapshotId (\s a -> s {_sieSnapshotId = a})

instance Hashable ScheduledInstancesEBS

instance NFData ScheduledInstancesEBS

instance ToQuery ScheduledInstancesEBS where
  toQuery ScheduledInstancesEBS' {..} =
    mconcat
      [ "DeleteOnTermination" =: _sieDeleteOnTermination,
        "VolumeSize" =: _sieVolumeSize,
        "Iops" =: _sieIOPS,
        "Encrypted" =: _sieEncrypted,
        "VolumeType" =: _sieVolumeType,
        "SnapshotId" =: _sieSnapshotId
      ]
