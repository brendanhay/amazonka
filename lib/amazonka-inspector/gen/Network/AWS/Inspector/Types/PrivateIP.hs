{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.PrivateIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.PrivateIP where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a private IP address associated with a network interface. This data type is used as a response element in the 'DescribeFindings' action.
--
--
--
-- /See:/ 'privateIP' smart constructor.
data PrivateIP = PrivateIP'
  { _piPrivateIPAddress :: !(Maybe Text),
    _piPrivateDNSName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PrivateIP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piPrivateIPAddress' - The full IP address of the network inteface.
--
-- * 'piPrivateDNSName' - The DNS name of the private IP address.
privateIP ::
  PrivateIP
privateIP =
  PrivateIP'
    { _piPrivateIPAddress = Nothing,
      _piPrivateDNSName = Nothing
    }

-- | The full IP address of the network inteface.
piPrivateIPAddress :: Lens' PrivateIP (Maybe Text)
piPrivateIPAddress = lens _piPrivateIPAddress (\s a -> s {_piPrivateIPAddress = a})

-- | The DNS name of the private IP address.
piPrivateDNSName :: Lens' PrivateIP (Maybe Text)
piPrivateDNSName = lens _piPrivateDNSName (\s a -> s {_piPrivateDNSName = a})

instance FromJSON PrivateIP where
  parseJSON =
    withObject
      "PrivateIP"
      ( \x ->
          PrivateIP'
            <$> (x .:? "privateIpAddress") <*> (x .:? "privateDnsName")
      )

instance Hashable PrivateIP

instance NFData PrivateIP
