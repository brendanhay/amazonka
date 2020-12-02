{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceBlockDeviceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceBlockDeviceMapping where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.EBSInstanceBlockDevice
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a block device mapping.
--
--
--
-- /See:/ 'instanceBlockDeviceMapping' smart constructor.
data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping'
  { _ibdmEBS ::
      !(Maybe EBSInstanceBlockDevice),
    _ibdmDeviceName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceBlockDeviceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ibdmEBS' - Parameters used to automatically set up EBS volumes when the instance is launched.
--
-- * 'ibdmDeviceName' - The device name (for example, @/dev/sdh@ or @xvdh@ ).
instanceBlockDeviceMapping ::
  InstanceBlockDeviceMapping
instanceBlockDeviceMapping =
  InstanceBlockDeviceMapping'
    { _ibdmEBS = Nothing,
      _ibdmDeviceName = Nothing
    }

-- | Parameters used to automatically set up EBS volumes when the instance is launched.
ibdmEBS :: Lens' InstanceBlockDeviceMapping (Maybe EBSInstanceBlockDevice)
ibdmEBS = lens _ibdmEBS (\s a -> s {_ibdmEBS = a})

-- | The device name (for example, @/dev/sdh@ or @xvdh@ ).
ibdmDeviceName :: Lens' InstanceBlockDeviceMapping (Maybe Text)
ibdmDeviceName = lens _ibdmDeviceName (\s a -> s {_ibdmDeviceName = a})

instance FromXML InstanceBlockDeviceMapping where
  parseXML x =
    InstanceBlockDeviceMapping'
      <$> (x .@? "ebs") <*> (x .@? "deviceName")

instance Hashable InstanceBlockDeviceMapping

instance NFData InstanceBlockDeviceMapping
