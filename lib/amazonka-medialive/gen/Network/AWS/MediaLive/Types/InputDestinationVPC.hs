{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDestinationVPC
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDestinationVPC where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The properties for a VPC type input destination.
--
-- /See:/ 'inputDestinationVPC' smart constructor.
data InputDestinationVPC = InputDestinationVPC'
  { _idvNetworkInterfaceId ::
      !(Maybe Text),
    _idvAvailabilityZone :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDestinationVPC' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idvNetworkInterfaceId' - The network interface ID of the Input destination in the VPC.
--
-- * 'idvAvailabilityZone' - The availability zone of the Input destination.
inputDestinationVPC ::
  InputDestinationVPC
inputDestinationVPC =
  InputDestinationVPC'
    { _idvNetworkInterfaceId = Nothing,
      _idvAvailabilityZone = Nothing
    }

-- | The network interface ID of the Input destination in the VPC.
idvNetworkInterfaceId :: Lens' InputDestinationVPC (Maybe Text)
idvNetworkInterfaceId = lens _idvNetworkInterfaceId (\s a -> s {_idvNetworkInterfaceId = a})

-- | The availability zone of the Input destination.
idvAvailabilityZone :: Lens' InputDestinationVPC (Maybe Text)
idvAvailabilityZone = lens _idvAvailabilityZone (\s a -> s {_idvAvailabilityZone = a})

instance FromJSON InputDestinationVPC where
  parseJSON =
    withObject
      "InputDestinationVPC"
      ( \x ->
          InputDestinationVPC'
            <$> (x .:? "networkInterfaceId") <*> (x .:? "availabilityZone")
      )

instance Hashable InputDestinationVPC

instance NFData InputDestinationVPC
