{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplatePlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplatePlacement where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tenancy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the placement of an instance.
--
--
--
-- /See:/ 'launchTemplatePlacement' smart constructor.
data LaunchTemplatePlacement = LaunchTemplatePlacement'
  { _ltpAffinity ::
      !(Maybe Text),
    _ltpHostId :: !(Maybe Text),
    _ltpPartitionNumber :: !(Maybe Int),
    _ltpSpreadDomain :: !(Maybe Text),
    _ltpAvailabilityZone :: !(Maybe Text),
    _ltpTenancy :: !(Maybe Tenancy),
    _ltpGroupName :: !(Maybe Text),
    _ltpHostResourceGroupARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplatePlacement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltpAffinity' - The affinity setting for the instance on the Dedicated Host.
--
-- * 'ltpHostId' - The ID of the Dedicated Host for the instance.
--
-- * 'ltpPartitionNumber' - The number of the partition the instance should launch in. Valid only if the placement group strategy is set to @partition@ .
--
-- * 'ltpSpreadDomain' - Reserved for future use.
--
-- * 'ltpAvailabilityZone' - The Availability Zone of the instance.
--
-- * 'ltpTenancy' - The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware.
--
-- * 'ltpGroupName' - The name of the placement group for the instance.
--
-- * 'ltpHostResourceGroupARN' - The ARN of the host resource group in which to launch the instances.
launchTemplatePlacement ::
  LaunchTemplatePlacement
launchTemplatePlacement =
  LaunchTemplatePlacement'
    { _ltpAffinity = Nothing,
      _ltpHostId = Nothing,
      _ltpPartitionNumber = Nothing,
      _ltpSpreadDomain = Nothing,
      _ltpAvailabilityZone = Nothing,
      _ltpTenancy = Nothing,
      _ltpGroupName = Nothing,
      _ltpHostResourceGroupARN = Nothing
    }

-- | The affinity setting for the instance on the Dedicated Host.
ltpAffinity :: Lens' LaunchTemplatePlacement (Maybe Text)
ltpAffinity = lens _ltpAffinity (\s a -> s {_ltpAffinity = a})

-- | The ID of the Dedicated Host for the instance.
ltpHostId :: Lens' LaunchTemplatePlacement (Maybe Text)
ltpHostId = lens _ltpHostId (\s a -> s {_ltpHostId = a})

-- | The number of the partition the instance should launch in. Valid only if the placement group strategy is set to @partition@ .
ltpPartitionNumber :: Lens' LaunchTemplatePlacement (Maybe Int)
ltpPartitionNumber = lens _ltpPartitionNumber (\s a -> s {_ltpPartitionNumber = a})

-- | Reserved for future use.
ltpSpreadDomain :: Lens' LaunchTemplatePlacement (Maybe Text)
ltpSpreadDomain = lens _ltpSpreadDomain (\s a -> s {_ltpSpreadDomain = a})

-- | The Availability Zone of the instance.
ltpAvailabilityZone :: Lens' LaunchTemplatePlacement (Maybe Text)
ltpAvailabilityZone = lens _ltpAvailabilityZone (\s a -> s {_ltpAvailabilityZone = a})

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware.
ltpTenancy :: Lens' LaunchTemplatePlacement (Maybe Tenancy)
ltpTenancy = lens _ltpTenancy (\s a -> s {_ltpTenancy = a})

-- | The name of the placement group for the instance.
ltpGroupName :: Lens' LaunchTemplatePlacement (Maybe Text)
ltpGroupName = lens _ltpGroupName (\s a -> s {_ltpGroupName = a})

-- | The ARN of the host resource group in which to launch the instances.
ltpHostResourceGroupARN :: Lens' LaunchTemplatePlacement (Maybe Text)
ltpHostResourceGroupARN = lens _ltpHostResourceGroupARN (\s a -> s {_ltpHostResourceGroupARN = a})

instance FromXML LaunchTemplatePlacement where
  parseXML x =
    LaunchTemplatePlacement'
      <$> (x .@? "affinity")
      <*> (x .@? "hostId")
      <*> (x .@? "partitionNumber")
      <*> (x .@? "spreadDomain")
      <*> (x .@? "availabilityZone")
      <*> (x .@? "tenancy")
      <*> (x .@? "groupName")
      <*> (x .@? "hostResourceGroupArn")

instance Hashable LaunchTemplatePlacement

instance NFData LaunchTemplatePlacement
