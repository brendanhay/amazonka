{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.EBSBlockDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.EBSBlockDevice where

import Network.AWS.EMR.Types.VolumeSpecification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration of requested EBS block device associated with the instance group.
--
--
--
-- /See:/ 'ebsBlockDevice' smart constructor.
data EBSBlockDevice = EBSBlockDevice'
  { _ebdDevice :: !(Maybe Text),
    _ebdVolumeSpecification :: !(Maybe VolumeSpecification)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EBSBlockDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebdDevice' - The device name that is exposed to the instance, such as /dev/sdh.
--
-- * 'ebdVolumeSpecification' - EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
ebsBlockDevice ::
  EBSBlockDevice
ebsBlockDevice =
  EBSBlockDevice'
    { _ebdDevice = Nothing,
      _ebdVolumeSpecification = Nothing
    }

-- | The device name that is exposed to the instance, such as /dev/sdh.
ebdDevice :: Lens' EBSBlockDevice (Maybe Text)
ebdDevice = lens _ebdDevice (\s a -> s {_ebdDevice = a})

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
ebdVolumeSpecification :: Lens' EBSBlockDevice (Maybe VolumeSpecification)
ebdVolumeSpecification = lens _ebdVolumeSpecification (\s a -> s {_ebdVolumeSpecification = a})

instance FromJSON EBSBlockDevice where
  parseJSON =
    withObject
      "EBSBlockDevice"
      ( \x ->
          EBSBlockDevice'
            <$> (x .:? "Device") <*> (x .:? "VolumeSpecification")
      )

instance Hashable EBSBlockDevice

instance NFData EBSBlockDevice
