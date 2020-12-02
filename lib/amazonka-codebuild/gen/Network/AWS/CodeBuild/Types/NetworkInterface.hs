{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.NetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.NetworkInterface where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a network interface.
--
--
--
-- /See:/ 'networkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { _niSubnetId ::
      !(Maybe Text),
    _niNetworkInterfaceId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niSubnetId' - The ID of the subnet.
--
-- * 'niNetworkInterfaceId' - The ID of the network interface.
networkInterface ::
  NetworkInterface
networkInterface =
  NetworkInterface'
    { _niSubnetId = Nothing,
      _niNetworkInterfaceId = Nothing
    }

-- | The ID of the subnet.
niSubnetId :: Lens' NetworkInterface (Maybe Text)
niSubnetId = lens _niSubnetId (\s a -> s {_niSubnetId = a})

-- | The ID of the network interface.
niNetworkInterfaceId :: Lens' NetworkInterface (Maybe Text)
niNetworkInterfaceId = lens _niNetworkInterfaceId (\s a -> s {_niNetworkInterfaceId = a})

instance FromJSON NetworkInterface where
  parseJSON =
    withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            <$> (x .:? "subnetId") <*> (x .:? "networkInterfaceId")
      )

instance Hashable NetworkInterface

instance NFData NetworkInterface
