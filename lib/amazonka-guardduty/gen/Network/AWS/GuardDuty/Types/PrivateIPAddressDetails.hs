{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PrivateIPAddressDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PrivateIPAddressDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains other private IP address information of the EC2 instance.
--
--
--
-- /See:/ 'privateIPAddressDetails' smart constructor.
data PrivateIPAddressDetails = PrivateIPAddressDetails'
  { _piadPrivateIPAddress ::
      !(Maybe Text),
    _piadPrivateDNSName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PrivateIPAddressDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piadPrivateIPAddress' - The private IP address of the EC2 instance.
--
-- * 'piadPrivateDNSName' - The private DNS name of the EC2 instance.
privateIPAddressDetails ::
  PrivateIPAddressDetails
privateIPAddressDetails =
  PrivateIPAddressDetails'
    { _piadPrivateIPAddress = Nothing,
      _piadPrivateDNSName = Nothing
    }

-- | The private IP address of the EC2 instance.
piadPrivateIPAddress :: Lens' PrivateIPAddressDetails (Maybe Text)
piadPrivateIPAddress = lens _piadPrivateIPAddress (\s a -> s {_piadPrivateIPAddress = a})

-- | The private DNS name of the EC2 instance.
piadPrivateDNSName :: Lens' PrivateIPAddressDetails (Maybe Text)
piadPrivateDNSName = lens _piadPrivateDNSName (\s a -> s {_piadPrivateDNSName = a})

instance FromJSON PrivateIPAddressDetails where
  parseJSON =
    withObject
      "PrivateIPAddressDetails"
      ( \x ->
          PrivateIPAddressDetails'
            <$> (x .:? "privateIpAddress") <*> (x .:? "privateDnsName")
      )

instance Hashable PrivateIPAddressDetails

instance NFData PrivateIPAddressDetails
