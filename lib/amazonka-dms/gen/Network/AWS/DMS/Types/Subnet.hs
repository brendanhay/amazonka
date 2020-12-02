{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Subnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.Subnet where

import Network.AWS.DMS.Types.AvailabilityZone
import Network.AWS.Lens
import Network.AWS.Prelude

-- | In response to a request by the @DescribeReplicationSubnetGroups@ operation, this object identifies a subnet by its given Availability Zone, subnet identifier, and status.
--
--
--
-- /See:/ 'subnet' smart constructor.
data Subnet = Subnet'
  { _sSubnetStatus :: !(Maybe Text),
    _sSubnetIdentifier :: !(Maybe Text),
    _sSubnetAvailabilityZone :: !(Maybe AvailabilityZone)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubnetStatus' - The status of the subnet.
--
-- * 'sSubnetIdentifier' - The subnet identifier.
--
-- * 'sSubnetAvailabilityZone' - The Availability Zone of the subnet.
subnet ::
  Subnet
subnet =
  Subnet'
    { _sSubnetStatus = Nothing,
      _sSubnetIdentifier = Nothing,
      _sSubnetAvailabilityZone = Nothing
    }

-- | The status of the subnet.
sSubnetStatus :: Lens' Subnet (Maybe Text)
sSubnetStatus = lens _sSubnetStatus (\s a -> s {_sSubnetStatus = a})

-- | The subnet identifier.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier = lens _sSubnetIdentifier (\s a -> s {_sSubnetIdentifier = a})

-- | The Availability Zone of the subnet.
sSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sSubnetAvailabilityZone = lens _sSubnetAvailabilityZone (\s a -> s {_sSubnetAvailabilityZone = a})

instance FromJSON Subnet where
  parseJSON =
    withObject
      "Subnet"
      ( \x ->
          Subnet'
            <$> (x .:? "SubnetStatus")
            <*> (x .:? "SubnetIdentifier")
            <*> (x .:? "SubnetAvailabilityZone")
      )

instance Hashable Subnet

instance NFData Subnet
