{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Placement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Placement where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tenancy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the placement of an instance.
--
--
--
-- /See:/ 'placement' smart constructor.
data Placement = Placement'
  { _plaAffinity :: !(Maybe Text),
    _plaHostId :: !(Maybe Text),
    _plaPartitionNumber :: !(Maybe Int),
    _plaSpreadDomain :: !(Maybe Text),
    _plaAvailabilityZone :: !(Maybe Text),
    _plaTenancy :: !(Maybe Tenancy),
    _plaGroupName :: !(Maybe Text),
    _plaHostResourceGroupARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Placement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plaAffinity' - The affinity setting for the instance on the Dedicated Host. This parameter is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command. This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- * 'plaHostId' - The ID of the Dedicated Host on which the instance resides. This parameter is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command. This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- * 'plaPartitionNumber' - The number of the partition the instance is in. Valid only if the placement group strategy is set to @partition@ . This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- * 'plaSpreadDomain' - Reserved for future use. This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- * 'plaAvailabilityZone' - The Availability Zone of the instance. If not specified, an Availability Zone will be automatically chosen for you based on the load balancing criteria for the Region. This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- * 'plaTenancy' - The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command. This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- * 'plaGroupName' - The name of the placement group the instance is in.
--
-- * 'plaHostResourceGroupARN' - The ARN of the host resource group in which to launch the instances. If you specify a host resource group ARN, omit the __Tenancy__ parameter or set it to @host@ . This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
placement ::
  Placement
placement =
  Placement'
    { _plaAffinity = Nothing,
      _plaHostId = Nothing,
      _plaPartitionNumber = Nothing,
      _plaSpreadDomain = Nothing,
      _plaAvailabilityZone = Nothing,
      _plaTenancy = Nothing,
      _plaGroupName = Nothing,
      _plaHostResourceGroupARN = Nothing
    }

-- | The affinity setting for the instance on the Dedicated Host. This parameter is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command. This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
plaAffinity :: Lens' Placement (Maybe Text)
plaAffinity = lens _plaAffinity (\s a -> s {_plaAffinity = a})

-- | The ID of the Dedicated Host on which the instance resides. This parameter is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command. This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
plaHostId :: Lens' Placement (Maybe Text)
plaHostId = lens _plaHostId (\s a -> s {_plaHostId = a})

-- | The number of the partition the instance is in. Valid only if the placement group strategy is set to @partition@ . This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
plaPartitionNumber :: Lens' Placement (Maybe Int)
plaPartitionNumber = lens _plaPartitionNumber (\s a -> s {_plaPartitionNumber = a})

-- | Reserved for future use. This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
plaSpreadDomain :: Lens' Placement (Maybe Text)
plaSpreadDomain = lens _plaSpreadDomain (\s a -> s {_plaSpreadDomain = a})

-- | The Availability Zone of the instance. If not specified, an Availability Zone will be automatically chosen for you based on the load balancing criteria for the Region. This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
plaAvailabilityZone :: Lens' Placement (Maybe Text)
plaAvailabilityZone = lens _plaAvailabilityZone (\s a -> s {_plaAvailabilityZone = a})

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command. This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
plaTenancy :: Lens' Placement (Maybe Tenancy)
plaTenancy = lens _plaTenancy (\s a -> s {_plaTenancy = a})

-- | The name of the placement group the instance is in.
plaGroupName :: Lens' Placement (Maybe Text)
plaGroupName = lens _plaGroupName (\s a -> s {_plaGroupName = a})

-- | The ARN of the host resource group in which to launch the instances. If you specify a host resource group ARN, omit the __Tenancy__ parameter or set it to @host@ . This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
plaHostResourceGroupARN :: Lens' Placement (Maybe Text)
plaHostResourceGroupARN = lens _plaHostResourceGroupARN (\s a -> s {_plaHostResourceGroupARN = a})

instance FromXML Placement where
  parseXML x =
    Placement'
      <$> (x .@? "affinity")
      <*> (x .@? "hostId")
      <*> (x .@? "partitionNumber")
      <*> (x .@? "spreadDomain")
      <*> (x .@? "availabilityZone")
      <*> (x .@? "tenancy")
      <*> (x .@? "groupName")
      <*> (x .@? "hostResourceGroupArn")

instance Hashable Placement

instance NFData Placement

instance ToQuery Placement where
  toQuery Placement' {..} =
    mconcat
      [ "Affinity" =: _plaAffinity,
        "HostId" =: _plaHostId,
        "PartitionNumber" =: _plaPartitionNumber,
        "SpreadDomain" =: _plaSpreadDomain,
        "AvailabilityZone" =: _plaAvailabilityZone,
        "Tenancy" =: _plaTenancy,
        "GroupName" =: _plaGroupName,
        "HostResourceGroupArn" =: _plaHostResourceGroupARN
      ]
