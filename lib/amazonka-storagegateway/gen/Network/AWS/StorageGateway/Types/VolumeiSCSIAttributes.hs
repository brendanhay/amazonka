{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Lists iSCSI information about a volume.
--
--
--
-- /See:/ 'volumeiSCSIAttributes' smart constructor.
data VolumeiSCSIAttributes = VolumeiSCSIAttributes'
  { _vscsiaLunNumber ::
      !(Maybe Nat),
    _vscsiaTargetARN :: !(Maybe Text),
    _vscsiaChapEnabled :: !(Maybe Bool),
    _vscsiaNetworkInterfaceId :: !(Maybe Text),
    _vscsiaNetworkInterfacePort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VolumeiSCSIAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vscsiaLunNumber' - The logical disk number.
--
-- * 'vscsiaTargetARN' - The Amazon Resource Name (ARN) of the volume target.
--
-- * 'vscsiaChapEnabled' - Indicates whether mutual CHAP is enabled for the iSCSI target.
--
-- * 'vscsiaNetworkInterfaceId' - The network interface identifier.
--
-- * 'vscsiaNetworkInterfacePort' - The port used to communicate with iSCSI targets.
volumeiSCSIAttributes ::
  VolumeiSCSIAttributes
volumeiSCSIAttributes =
  VolumeiSCSIAttributes'
    { _vscsiaLunNumber = Nothing,
      _vscsiaTargetARN = Nothing,
      _vscsiaChapEnabled = Nothing,
      _vscsiaNetworkInterfaceId = Nothing,
      _vscsiaNetworkInterfacePort = Nothing
    }

-- | The logical disk number.
vscsiaLunNumber :: Lens' VolumeiSCSIAttributes (Maybe Natural)
vscsiaLunNumber = lens _vscsiaLunNumber (\s a -> s {_vscsiaLunNumber = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the volume target.
vscsiaTargetARN :: Lens' VolumeiSCSIAttributes (Maybe Text)
vscsiaTargetARN = lens _vscsiaTargetARN (\s a -> s {_vscsiaTargetARN = a})

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
vscsiaChapEnabled :: Lens' VolumeiSCSIAttributes (Maybe Bool)
vscsiaChapEnabled = lens _vscsiaChapEnabled (\s a -> s {_vscsiaChapEnabled = a})

-- | The network interface identifier.
vscsiaNetworkInterfaceId :: Lens' VolumeiSCSIAttributes (Maybe Text)
vscsiaNetworkInterfaceId = lens _vscsiaNetworkInterfaceId (\s a -> s {_vscsiaNetworkInterfaceId = a})

-- | The port used to communicate with iSCSI targets.
vscsiaNetworkInterfacePort :: Lens' VolumeiSCSIAttributes (Maybe Int)
vscsiaNetworkInterfacePort = lens _vscsiaNetworkInterfacePort (\s a -> s {_vscsiaNetworkInterfacePort = a})

instance FromJSON VolumeiSCSIAttributes where
  parseJSON =
    withObject
      "VolumeiSCSIAttributes"
      ( \x ->
          VolumeiSCSIAttributes'
            <$> (x .:? "LunNumber")
            <*> (x .:? "TargetARN")
            <*> (x .:? "ChapEnabled")
            <*> (x .:? "NetworkInterfaceId")
            <*> (x .:? "NetworkInterfacePort")
      )

instance Hashable VolumeiSCSIAttributes

instance NFData VolumeiSCSIAttributes
