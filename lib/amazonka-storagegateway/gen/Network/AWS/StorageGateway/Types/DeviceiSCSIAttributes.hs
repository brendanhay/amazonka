{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.DeviceiSCSIAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.DeviceiSCSIAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Lists iSCSI information about a VTL device.
--
--
--
-- /See:/ 'deviceiSCSIAttributes' smart constructor.
data DeviceiSCSIAttributes = DeviceiSCSIAttributes'
  { _dscsiaTargetARN ::
      !(Maybe Text),
    _dscsiaChapEnabled :: !(Maybe Bool),
    _dscsiaNetworkInterfaceId :: !(Maybe Text),
    _dscsiaNetworkInterfacePort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeviceiSCSIAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscsiaTargetARN' - Specifies the unique Amazon Resource Name (ARN) that encodes the iSCSI qualified name(iqn) of a tape drive or media changer target.
--
-- * 'dscsiaChapEnabled' - Indicates whether mutual CHAP is enabled for the iSCSI target.
--
-- * 'dscsiaNetworkInterfaceId' - The network interface identifier of the VTL device.
--
-- * 'dscsiaNetworkInterfacePort' - The port used to communicate with iSCSI VTL device targets.
deviceiSCSIAttributes ::
  DeviceiSCSIAttributes
deviceiSCSIAttributes =
  DeviceiSCSIAttributes'
    { _dscsiaTargetARN = Nothing,
      _dscsiaChapEnabled = Nothing,
      _dscsiaNetworkInterfaceId = Nothing,
      _dscsiaNetworkInterfacePort = Nothing
    }

-- | Specifies the unique Amazon Resource Name (ARN) that encodes the iSCSI qualified name(iqn) of a tape drive or media changer target.
dscsiaTargetARN :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaTargetARN = lens _dscsiaTargetARN (\s a -> s {_dscsiaTargetARN = a})

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
dscsiaChapEnabled :: Lens' DeviceiSCSIAttributes (Maybe Bool)
dscsiaChapEnabled = lens _dscsiaChapEnabled (\s a -> s {_dscsiaChapEnabled = a})

-- | The network interface identifier of the VTL device.
dscsiaNetworkInterfaceId :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaNetworkInterfaceId = lens _dscsiaNetworkInterfaceId (\s a -> s {_dscsiaNetworkInterfaceId = a})

-- | The port used to communicate with iSCSI VTL device targets.
dscsiaNetworkInterfacePort :: Lens' DeviceiSCSIAttributes (Maybe Int)
dscsiaNetworkInterfacePort = lens _dscsiaNetworkInterfacePort (\s a -> s {_dscsiaNetworkInterfacePort = a})

instance FromJSON DeviceiSCSIAttributes where
  parseJSON =
    withObject
      "DeviceiSCSIAttributes"
      ( \x ->
          DeviceiSCSIAttributes'
            <$> (x .:? "TargetARN")
            <*> (x .:? "ChapEnabled")
            <*> (x .:? "NetworkInterfaceId")
            <*> (x .:? "NetworkInterfacePort")
      )

instance Hashable DeviceiSCSIAttributes

instance NFData DeviceiSCSIAttributes
