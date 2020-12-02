{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.RAIdArray
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.RAIdArray where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an instance's RAID array.
--
--
--
-- /See:/ 'rAIdArray' smart constructor.
data RAIdArray = RAIdArray'
  { _raiaInstanceId :: !(Maybe Text),
    _raiaSize :: !(Maybe Int),
    _raiaIOPS :: !(Maybe Int),
    _raiaCreatedAt :: !(Maybe Text),
    _raiaRAIdLevel :: !(Maybe Int),
    _raiaDevice :: !(Maybe Text),
    _raiaNumberOfDisks :: !(Maybe Int),
    _raiaAvailabilityZone :: !(Maybe Text),
    _raiaName :: !(Maybe Text),
    _raiaRAIdArrayId :: !(Maybe Text),
    _raiaVolumeType :: !(Maybe Text),
    _raiaStackId :: !(Maybe Text),
    _raiaMountPoint :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RAIdArray' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raiaInstanceId' - The instance ID.
--
-- * 'raiaSize' - The array's size.
--
-- * 'raiaIOPS' - For PIOPS volumes, the IOPS per disk.
--
-- * 'raiaCreatedAt' - When the RAID array was created.
--
-- * 'raiaRAIdLevel' - The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
--
-- * 'raiaDevice' - The array's Linux device. For example /dev/mdadm0.
--
-- * 'raiaNumberOfDisks' - The number of disks in the array.
--
-- * 'raiaAvailabilityZone' - The array's Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- * 'raiaName' - The array name.
--
-- * 'raiaRAIdArrayId' - The array ID.
--
-- * 'raiaVolumeType' - The volume type, standard or PIOPS.
--
-- * 'raiaStackId' - The stack ID.
--
-- * 'raiaMountPoint' - The array's mount point.
rAIdArray ::
  RAIdArray
rAIdArray =
  RAIdArray'
    { _raiaInstanceId = Nothing,
      _raiaSize = Nothing,
      _raiaIOPS = Nothing,
      _raiaCreatedAt = Nothing,
      _raiaRAIdLevel = Nothing,
      _raiaDevice = Nothing,
      _raiaNumberOfDisks = Nothing,
      _raiaAvailabilityZone = Nothing,
      _raiaName = Nothing,
      _raiaRAIdArrayId = Nothing,
      _raiaVolumeType = Nothing,
      _raiaStackId = Nothing,
      _raiaMountPoint = Nothing
    }

-- | The instance ID.
raiaInstanceId :: Lens' RAIdArray (Maybe Text)
raiaInstanceId = lens _raiaInstanceId (\s a -> s {_raiaInstanceId = a})

-- | The array's size.
raiaSize :: Lens' RAIdArray (Maybe Int)
raiaSize = lens _raiaSize (\s a -> s {_raiaSize = a})

-- | For PIOPS volumes, the IOPS per disk.
raiaIOPS :: Lens' RAIdArray (Maybe Int)
raiaIOPS = lens _raiaIOPS (\s a -> s {_raiaIOPS = a})

-- | When the RAID array was created.
raiaCreatedAt :: Lens' RAIdArray (Maybe Text)
raiaCreatedAt = lens _raiaCreatedAt (\s a -> s {_raiaCreatedAt = a})

-- | The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
raiaRAIdLevel :: Lens' RAIdArray (Maybe Int)
raiaRAIdLevel = lens _raiaRAIdLevel (\s a -> s {_raiaRAIdLevel = a})

-- | The array's Linux device. For example /dev/mdadm0.
raiaDevice :: Lens' RAIdArray (Maybe Text)
raiaDevice = lens _raiaDevice (\s a -> s {_raiaDevice = a})

-- | The number of disks in the array.
raiaNumberOfDisks :: Lens' RAIdArray (Maybe Int)
raiaNumberOfDisks = lens _raiaNumberOfDisks (\s a -> s {_raiaNumberOfDisks = a})

-- | The array's Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
raiaAvailabilityZone :: Lens' RAIdArray (Maybe Text)
raiaAvailabilityZone = lens _raiaAvailabilityZone (\s a -> s {_raiaAvailabilityZone = a})

-- | The array name.
raiaName :: Lens' RAIdArray (Maybe Text)
raiaName = lens _raiaName (\s a -> s {_raiaName = a})

-- | The array ID.
raiaRAIdArrayId :: Lens' RAIdArray (Maybe Text)
raiaRAIdArrayId = lens _raiaRAIdArrayId (\s a -> s {_raiaRAIdArrayId = a})

-- | The volume type, standard or PIOPS.
raiaVolumeType :: Lens' RAIdArray (Maybe Text)
raiaVolumeType = lens _raiaVolumeType (\s a -> s {_raiaVolumeType = a})

-- | The stack ID.
raiaStackId :: Lens' RAIdArray (Maybe Text)
raiaStackId = lens _raiaStackId (\s a -> s {_raiaStackId = a})

-- | The array's mount point.
raiaMountPoint :: Lens' RAIdArray (Maybe Text)
raiaMountPoint = lens _raiaMountPoint (\s a -> s {_raiaMountPoint = a})

instance FromJSON RAIdArray where
  parseJSON =
    withObject
      "RAIdArray"
      ( \x ->
          RAIdArray'
            <$> (x .:? "InstanceId")
            <*> (x .:? "Size")
            <*> (x .:? "Iops")
            <*> (x .:? "CreatedAt")
            <*> (x .:? "RaidLevel")
            <*> (x .:? "Device")
            <*> (x .:? "NumberOfDisks")
            <*> (x .:? "AvailabilityZone")
            <*> (x .:? "Name")
            <*> (x .:? "RaidArrayId")
            <*> (x .:? "VolumeType")
            <*> (x .:? "StackId")
            <*> (x .:? "MountPoint")
      )

instance Hashable RAIdArray

instance NFData RAIdArray
