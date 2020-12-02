{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Subnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Subnet where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.AvailabilityZone
import Network.AWS.RDS.Types.Outpost

-- | This data type is used as a response element for the @DescribeDBSubnetGroups@ operation.
--
--
--
-- /See:/ 'subnet' smart constructor.
data Subnet = Subnet'
  { _sSubnetStatus :: !(Maybe Text),
    _sSubnetIdentifier :: !(Maybe Text),
    _sSubnetAvailabilityZone :: !(Maybe AvailabilityZone),
    _sSubnetOutpost :: !(Maybe Outpost)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubnetStatus' - The status of the subnet.
--
-- * 'sSubnetIdentifier' - The identifier of the subnet.
--
-- * 'sSubnetAvailabilityZone' - Undocumented member.
--
-- * 'sSubnetOutpost' - If the subnet is associated with an Outpost, this value specifies the Outpost. For more information about RDS on Outposts, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts> in the /Amazon RDS User Guide./
subnet ::
  Subnet
subnet =
  Subnet'
    { _sSubnetStatus = Nothing,
      _sSubnetIdentifier = Nothing,
      _sSubnetAvailabilityZone = Nothing,
      _sSubnetOutpost = Nothing
    }

-- | The status of the subnet.
sSubnetStatus :: Lens' Subnet (Maybe Text)
sSubnetStatus = lens _sSubnetStatus (\s a -> s {_sSubnetStatus = a})

-- | The identifier of the subnet.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier = lens _sSubnetIdentifier (\s a -> s {_sSubnetIdentifier = a})

-- | Undocumented member.
sSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sSubnetAvailabilityZone = lens _sSubnetAvailabilityZone (\s a -> s {_sSubnetAvailabilityZone = a})

-- | If the subnet is associated with an Outpost, this value specifies the Outpost. For more information about RDS on Outposts, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts> in the /Amazon RDS User Guide./
sSubnetOutpost :: Lens' Subnet (Maybe Outpost)
sSubnetOutpost = lens _sSubnetOutpost (\s a -> s {_sSubnetOutpost = a})

instance FromXML Subnet where
  parseXML x =
    Subnet'
      <$> (x .@? "SubnetStatus")
      <*> (x .@? "SubnetIdentifier")
      <*> (x .@? "SubnetAvailabilityZone")
      <*> (x .@? "SubnetOutpost")

instance Hashable Subnet

instance NFData Subnet
