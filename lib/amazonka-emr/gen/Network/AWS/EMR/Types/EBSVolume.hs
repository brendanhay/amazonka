{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.EBSVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.EBSVolume where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | EBS block device that's attached to an EC2 instance.
--
--
--
-- /See:/ 'ebsVolume' smart constructor.
data EBSVolume = EBSVolume'
  { _evDevice :: !(Maybe Text),
    _evVolumeId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EBSVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evDevice' - The device name that is exposed to the instance, such as /dev/sdh.
--
-- * 'evVolumeId' - The volume identifier of the EBS volume.
ebsVolume ::
  EBSVolume
ebsVolume = EBSVolume' {_evDevice = Nothing, _evVolumeId = Nothing}

-- | The device name that is exposed to the instance, such as /dev/sdh.
evDevice :: Lens' EBSVolume (Maybe Text)
evDevice = lens _evDevice (\s a -> s {_evDevice = a})

-- | The volume identifier of the EBS volume.
evVolumeId :: Lens' EBSVolume (Maybe Text)
evVolumeId = lens _evVolumeId (\s a -> s {_evVolumeId = a})

instance FromJSON EBSVolume where
  parseJSON =
    withObject
      "EBSVolume"
      (\x -> EBSVolume' <$> (x .:? "Device") <*> (x .:? "VolumeId"))

instance Hashable EBSVolume

instance NFData EBSVolume
