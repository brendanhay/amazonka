{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PciId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PciId where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the data that identifies an Amazon FPGA image (AFI) on the PCI bus.
--
--
--
-- /See:/ 'pciId' smart constructor.
data PciId = PciId'
  { _piSubsystemId :: !(Maybe Text),
    _piDeviceId :: !(Maybe Text),
    _piSubsystemVendorId :: !(Maybe Text),
    _piVendorId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PciId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piSubsystemId' - The ID of the subsystem.
--
-- * 'piDeviceId' - The ID of the device.
--
-- * 'piSubsystemVendorId' - The ID of the vendor for the subsystem.
--
-- * 'piVendorId' - The ID of the vendor.
pciId ::
  PciId
pciId =
  PciId'
    { _piSubsystemId = Nothing,
      _piDeviceId = Nothing,
      _piSubsystemVendorId = Nothing,
      _piVendorId = Nothing
    }

-- | The ID of the subsystem.
piSubsystemId :: Lens' PciId (Maybe Text)
piSubsystemId = lens _piSubsystemId (\s a -> s {_piSubsystemId = a})

-- | The ID of the device.
piDeviceId :: Lens' PciId (Maybe Text)
piDeviceId = lens _piDeviceId (\s a -> s {_piDeviceId = a})

-- | The ID of the vendor for the subsystem.
piSubsystemVendorId :: Lens' PciId (Maybe Text)
piSubsystemVendorId = lens _piSubsystemVendorId (\s a -> s {_piSubsystemVendorId = a})

-- | The ID of the vendor.
piVendorId :: Lens' PciId (Maybe Text)
piVendorId = lens _piVendorId (\s a -> s {_piVendorId = a})

instance FromXML PciId where
  parseXML x =
    PciId'
      <$> (x .@? "SubsystemId")
      <*> (x .@? "DeviceId")
      <*> (x .@? "SubsystemVendorId")
      <*> (x .@? "VendorId")

instance Hashable PciId

instance NFData PciId
