{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotPlacement where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tenancy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes Spot Instance placement.
--
--
--
-- /See:/ 'spotPlacement' smart constructor.
data SpotPlacement = SpotPlacement'
  { _spAvailabilityZone ::
      !(Maybe Text),
    _spTenancy :: !(Maybe Tenancy),
    _spGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SpotPlacement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spAvailabilityZone' - The Availability Zone. [Spot Fleet only] To specify multiple Availability Zones, separate them using commas; for example, "us-west-2a, us-west-2b".
--
-- * 'spTenancy' - The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for Spot Instances.
--
-- * 'spGroupName' - The name of the placement group.
spotPlacement ::
  SpotPlacement
spotPlacement =
  SpotPlacement'
    { _spAvailabilityZone = Nothing,
      _spTenancy = Nothing,
      _spGroupName = Nothing
    }

-- | The Availability Zone. [Spot Fleet only] To specify multiple Availability Zones, separate them using commas; for example, "us-west-2a, us-west-2b".
spAvailabilityZone :: Lens' SpotPlacement (Maybe Text)
spAvailabilityZone = lens _spAvailabilityZone (\s a -> s {_spAvailabilityZone = a})

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for Spot Instances.
spTenancy :: Lens' SpotPlacement (Maybe Tenancy)
spTenancy = lens _spTenancy (\s a -> s {_spTenancy = a})

-- | The name of the placement group.
spGroupName :: Lens' SpotPlacement (Maybe Text)
spGroupName = lens _spGroupName (\s a -> s {_spGroupName = a})

instance FromXML SpotPlacement where
  parseXML x =
    SpotPlacement'
      <$> (x .@? "availabilityZone")
      <*> (x .@? "tenancy")
      <*> (x .@? "groupName")

instance Hashable SpotPlacement

instance NFData SpotPlacement

instance ToQuery SpotPlacement where
  toQuery SpotPlacement' {..} =
    mconcat
      [ "AvailabilityZone" =: _spAvailabilityZone,
        "Tenancy" =: _spTenancy,
        "GroupName" =: _spGroupName
      ]
