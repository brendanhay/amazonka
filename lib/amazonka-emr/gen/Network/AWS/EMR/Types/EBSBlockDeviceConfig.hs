{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.EBSBlockDeviceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.EBSBlockDeviceConfig where

import Network.AWS.EMR.Types.VolumeSpecification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration of requested EBS block device associated with the instance group with count of volumes that will be associated to every instance.
--
--
--
-- /See:/ 'ebsBlockDeviceConfig' smart constructor.
data EBSBlockDeviceConfig = EBSBlockDeviceConfig'
  { _ebdcVolumesPerInstance ::
      !(Maybe Int),
    _ebdcVolumeSpecification :: !VolumeSpecification
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EBSBlockDeviceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebdcVolumesPerInstance' - Number of EBS volumes with a specific volume configuration that will be associated with every instance in the instance group
--
-- * 'ebdcVolumeSpecification' - EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
ebsBlockDeviceConfig ::
  -- | 'ebdcVolumeSpecification'
  VolumeSpecification ->
  EBSBlockDeviceConfig
ebsBlockDeviceConfig pVolumeSpecification_ =
  EBSBlockDeviceConfig'
    { _ebdcVolumesPerInstance = Nothing,
      _ebdcVolumeSpecification = pVolumeSpecification_
    }

-- | Number of EBS volumes with a specific volume configuration that will be associated with every instance in the instance group
ebdcVolumesPerInstance :: Lens' EBSBlockDeviceConfig (Maybe Int)
ebdcVolumesPerInstance = lens _ebdcVolumesPerInstance (\s a -> s {_ebdcVolumesPerInstance = a})

-- | EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.
ebdcVolumeSpecification :: Lens' EBSBlockDeviceConfig VolumeSpecification
ebdcVolumeSpecification = lens _ebdcVolumeSpecification (\s a -> s {_ebdcVolumeSpecification = a})

instance Hashable EBSBlockDeviceConfig

instance NFData EBSBlockDeviceConfig

instance ToJSON EBSBlockDeviceConfig where
  toJSON EBSBlockDeviceConfig' {..} =
    object
      ( catMaybes
          [ ("VolumesPerInstance" .=) <$> _ebdcVolumesPerInstance,
            Just ("VolumeSpecification" .= _ebdcVolumeSpecification)
          ]
      )
