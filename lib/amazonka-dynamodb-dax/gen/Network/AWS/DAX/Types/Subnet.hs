{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.Subnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.Subnet where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the subnet associated with a DAX cluster. This parameter refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used with DAX.
--
--
--
-- /See:/ 'subnet' smart constructor.
data Subnet = Subnet'
  { _sSubnetIdentifier :: !(Maybe Text),
    _sSubnetAvailabilityZone :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubnetIdentifier' - The system-assigned identifier for the subnet.
--
-- * 'sSubnetAvailabilityZone' - The Availability Zone (AZ) for the subnet.
subnet ::
  Subnet
subnet =
  Subnet'
    { _sSubnetIdentifier = Nothing,
      _sSubnetAvailabilityZone = Nothing
    }

-- | The system-assigned identifier for the subnet.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier = lens _sSubnetIdentifier (\s a -> s {_sSubnetIdentifier = a})

-- | The Availability Zone (AZ) for the subnet.
sSubnetAvailabilityZone :: Lens' Subnet (Maybe Text)
sSubnetAvailabilityZone = lens _sSubnetAvailabilityZone (\s a -> s {_sSubnetAvailabilityZone = a})

instance FromJSON Subnet where
  parseJSON =
    withObject
      "Subnet"
      ( \x ->
          Subnet'
            <$> (x .:? "SubnetIdentifier") <*> (x .:? "SubnetAvailabilityZone")
      )

instance Hashable Subnet

instance NFData Subnet
