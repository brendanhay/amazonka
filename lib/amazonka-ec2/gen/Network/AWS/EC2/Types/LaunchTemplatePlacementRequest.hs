{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplatePlacementRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplatePlacementRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tenancy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the placement of an instance.
--
--
--
-- /See:/ 'launchTemplatePlacementRequest' smart constructor.
data LaunchTemplatePlacementRequest = LaunchTemplatePlacementRequest'
  { _ltprAffinity ::
      !(Maybe Text),
    _ltprHostId :: !(Maybe Text),
    _ltprPartitionNumber ::
      !(Maybe Int),
    _ltprSpreadDomain ::
      !(Maybe Text),
    _ltprAvailabilityZone ::
      !(Maybe Text),
    _ltprTenancy ::
      !(Maybe Tenancy),
    _ltprGroupName ::
      !(Maybe Text),
    _ltprHostResourceGroupARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplatePlacementRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltprAffinity' - The affinity setting for an instance on a Dedicated Host.
--
-- * 'ltprHostId' - The ID of the Dedicated Host for the instance.
--
-- * 'ltprPartitionNumber' - The number of the partition the instance should launch in. Valid only if the placement group strategy is set to @partition@ .
--
-- * 'ltprSpreadDomain' - Reserved for future use.
--
-- * 'ltprAvailabilityZone' - The Availability Zone for the instance.
--
-- * 'ltprTenancy' - The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of dedicated runs on single-tenant hardware.
--
-- * 'ltprGroupName' - The name of the placement group for the instance.
--
-- * 'ltprHostResourceGroupARN' - The ARN of the host resource group in which to launch the instances. If you specify a host resource group ARN, omit the __Tenancy__ parameter or set it to @host@ .
launchTemplatePlacementRequest ::
  LaunchTemplatePlacementRequest
launchTemplatePlacementRequest =
  LaunchTemplatePlacementRequest'
    { _ltprAffinity = Nothing,
      _ltprHostId = Nothing,
      _ltprPartitionNumber = Nothing,
      _ltprSpreadDomain = Nothing,
      _ltprAvailabilityZone = Nothing,
      _ltprTenancy = Nothing,
      _ltprGroupName = Nothing,
      _ltprHostResourceGroupARN = Nothing
    }

-- | The affinity setting for an instance on a Dedicated Host.
ltprAffinity :: Lens' LaunchTemplatePlacementRequest (Maybe Text)
ltprAffinity = lens _ltprAffinity (\s a -> s {_ltprAffinity = a})

-- | The ID of the Dedicated Host for the instance.
ltprHostId :: Lens' LaunchTemplatePlacementRequest (Maybe Text)
ltprHostId = lens _ltprHostId (\s a -> s {_ltprHostId = a})

-- | The number of the partition the instance should launch in. Valid only if the placement group strategy is set to @partition@ .
ltprPartitionNumber :: Lens' LaunchTemplatePlacementRequest (Maybe Int)
ltprPartitionNumber = lens _ltprPartitionNumber (\s a -> s {_ltprPartitionNumber = a})

-- | Reserved for future use.
ltprSpreadDomain :: Lens' LaunchTemplatePlacementRequest (Maybe Text)
ltprSpreadDomain = lens _ltprSpreadDomain (\s a -> s {_ltprSpreadDomain = a})

-- | The Availability Zone for the instance.
ltprAvailabilityZone :: Lens' LaunchTemplatePlacementRequest (Maybe Text)
ltprAvailabilityZone = lens _ltprAvailabilityZone (\s a -> s {_ltprAvailabilityZone = a})

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of dedicated runs on single-tenant hardware.
ltprTenancy :: Lens' LaunchTemplatePlacementRequest (Maybe Tenancy)
ltprTenancy = lens _ltprTenancy (\s a -> s {_ltprTenancy = a})

-- | The name of the placement group for the instance.
ltprGroupName :: Lens' LaunchTemplatePlacementRequest (Maybe Text)
ltprGroupName = lens _ltprGroupName (\s a -> s {_ltprGroupName = a})

-- | The ARN of the host resource group in which to launch the instances. If you specify a host resource group ARN, omit the __Tenancy__ parameter or set it to @host@ .
ltprHostResourceGroupARN :: Lens' LaunchTemplatePlacementRequest (Maybe Text)
ltprHostResourceGroupARN = lens _ltprHostResourceGroupARN (\s a -> s {_ltprHostResourceGroupARN = a})

instance Hashable LaunchTemplatePlacementRequest

instance NFData LaunchTemplatePlacementRequest

instance ToQuery LaunchTemplatePlacementRequest where
  toQuery LaunchTemplatePlacementRequest' {..} =
    mconcat
      [ "Affinity" =: _ltprAffinity,
        "HostId" =: _ltprHostId,
        "PartitionNumber" =: _ltprPartitionNumber,
        "SpreadDomain" =: _ltprSpreadDomain,
        "AvailabilityZone" =: _ltprAvailabilityZone,
        "Tenancy" =: _ltprTenancy,
        "GroupName" =: _ltprGroupName,
        "HostResourceGroupArn" =: _ltprHostResourceGroupARN
      ]
