{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.NetworkAccessConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.NetworkAccessConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the network details of the fleet or image builder instance.
--
--
--
-- /See:/ 'networkAccessConfiguration' smart constructor.
data NetworkAccessConfiguration = NetworkAccessConfiguration'
  { _nacEniId ::
      !(Maybe Text),
    _nacEniPrivateIPAddress ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkAccessConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nacEniId' - The resource identifier of the elastic network interface that is attached to instances in your VPC. All network interfaces have the eni-xxxxxxxx resource identifier.
--
-- * 'nacEniPrivateIPAddress' - The private IP address of the elastic network interface that is attached to instances in your VPC.
networkAccessConfiguration ::
  NetworkAccessConfiguration
networkAccessConfiguration =
  NetworkAccessConfiguration'
    { _nacEniId = Nothing,
      _nacEniPrivateIPAddress = Nothing
    }

-- | The resource identifier of the elastic network interface that is attached to instances in your VPC. All network interfaces have the eni-xxxxxxxx resource identifier.
nacEniId :: Lens' NetworkAccessConfiguration (Maybe Text)
nacEniId = lens _nacEniId (\s a -> s {_nacEniId = a})

-- | The private IP address of the elastic network interface that is attached to instances in your VPC.
nacEniPrivateIPAddress :: Lens' NetworkAccessConfiguration (Maybe Text)
nacEniPrivateIPAddress = lens _nacEniPrivateIPAddress (\s a -> s {_nacEniPrivateIPAddress = a})

instance FromJSON NetworkAccessConfiguration where
  parseJSON =
    withObject
      "NetworkAccessConfiguration"
      ( \x ->
          NetworkAccessConfiguration'
            <$> (x .:? "EniId") <*> (x .:? "EniPrivateIpAddress")
      )

instance Hashable NetworkAccessConfiguration

instance NFData NetworkAccessConfiguration
