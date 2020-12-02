{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportVolumeTaskDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportVolumeTaskDetails where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DiskImageDescription
import Network.AWS.EC2.Types.DiskImageVolumeDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an import volume task.
--
--
--
-- /See:/ 'importVolumeTaskDetails' smart constructor.
data ImportVolumeTaskDetails = ImportVolumeTaskDetails'
  { _ivtdBytesConverted ::
      !(Maybe Integer),
    _ivtdImage :: !(Maybe DiskImageDescription),
    _ivtdVolume ::
      !(Maybe DiskImageVolumeDescription),
    _ivtdAvailabilityZone :: !(Maybe Text),
    _ivtdDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportVolumeTaskDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ivtdBytesConverted' - The number of bytes converted so far.
--
-- * 'ivtdImage' - The image.
--
-- * 'ivtdVolume' - The volume.
--
-- * 'ivtdAvailabilityZone' - The Availability Zone where the resulting volume will reside.
--
-- * 'ivtdDescription' - The description you provided when starting the import volume task.
importVolumeTaskDetails ::
  ImportVolumeTaskDetails
importVolumeTaskDetails =
  ImportVolumeTaskDetails'
    { _ivtdBytesConverted = Nothing,
      _ivtdImage = Nothing,
      _ivtdVolume = Nothing,
      _ivtdAvailabilityZone = Nothing,
      _ivtdDescription = Nothing
    }

-- | The number of bytes converted so far.
ivtdBytesConverted :: Lens' ImportVolumeTaskDetails (Maybe Integer)
ivtdBytesConverted = lens _ivtdBytesConverted (\s a -> s {_ivtdBytesConverted = a})

-- | The image.
ivtdImage :: Lens' ImportVolumeTaskDetails (Maybe DiskImageDescription)
ivtdImage = lens _ivtdImage (\s a -> s {_ivtdImage = a})

-- | The volume.
ivtdVolume :: Lens' ImportVolumeTaskDetails (Maybe DiskImageVolumeDescription)
ivtdVolume = lens _ivtdVolume (\s a -> s {_ivtdVolume = a})

-- | The Availability Zone where the resulting volume will reside.
ivtdAvailabilityZone :: Lens' ImportVolumeTaskDetails (Maybe Text)
ivtdAvailabilityZone = lens _ivtdAvailabilityZone (\s a -> s {_ivtdAvailabilityZone = a})

-- | The description you provided when starting the import volume task.
ivtdDescription :: Lens' ImportVolumeTaskDetails (Maybe Text)
ivtdDescription = lens _ivtdDescription (\s a -> s {_ivtdDescription = a})

instance FromXML ImportVolumeTaskDetails where
  parseXML x =
    ImportVolumeTaskDetails'
      <$> (x .@? "bytesConverted")
      <*> (x .@? "image")
      <*> (x .@? "volume")
      <*> (x .@? "availabilityZone")
      <*> (x .@? "description")

instance Hashable ImportVolumeTaskDetails

instance NFData ImportVolumeTaskDetails
