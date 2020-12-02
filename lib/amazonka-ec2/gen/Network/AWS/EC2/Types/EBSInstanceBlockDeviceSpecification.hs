{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EBSInstanceBlockDeviceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EBSInstanceBlockDeviceSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes information used to set up an EBS volume specified in a block device mapping.
--
--
--
-- /See:/ 'ebsInstanceBlockDeviceSpecification' smart constructor.
data EBSInstanceBlockDeviceSpecification = EBSInstanceBlockDeviceSpecification'
  { _eibdsDeleteOnTermination ::
      !(Maybe Bool),
    _eibdsVolumeId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EBSInstanceBlockDeviceSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eibdsDeleteOnTermination' - Indicates whether the volume is deleted on instance termination.
--
-- * 'eibdsVolumeId' - The ID of the EBS volume.
ebsInstanceBlockDeviceSpecification ::
  EBSInstanceBlockDeviceSpecification
ebsInstanceBlockDeviceSpecification =
  EBSInstanceBlockDeviceSpecification'
    { _eibdsDeleteOnTermination =
        Nothing,
      _eibdsVolumeId = Nothing
    }

-- | Indicates whether the volume is deleted on instance termination.
eibdsDeleteOnTermination :: Lens' EBSInstanceBlockDeviceSpecification (Maybe Bool)
eibdsDeleteOnTermination = lens _eibdsDeleteOnTermination (\s a -> s {_eibdsDeleteOnTermination = a})

-- | The ID of the EBS volume.
eibdsVolumeId :: Lens' EBSInstanceBlockDeviceSpecification (Maybe Text)
eibdsVolumeId = lens _eibdsVolumeId (\s a -> s {_eibdsVolumeId = a})

instance Hashable EBSInstanceBlockDeviceSpecification

instance NFData EBSInstanceBlockDeviceSpecification

instance ToQuery EBSInstanceBlockDeviceSpecification where
  toQuery EBSInstanceBlockDeviceSpecification' {..} =
    mconcat
      [ "DeleteOnTermination" =: _eibdsDeleteOnTermination,
        "VolumeId" =: _eibdsVolumeId
      ]
