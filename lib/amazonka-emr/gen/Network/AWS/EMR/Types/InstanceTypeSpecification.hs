{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceTypeSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceTypeSpecification where

import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EBSBlockDevice
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration specification for each instance type in an instance fleet.
--
--
--
-- /See:/ 'instanceTypeSpecification' smart constructor.
data InstanceTypeSpecification = InstanceTypeSpecification'
  { _itsBidPrice ::
      !(Maybe Text),
    _itsWeightedCapacity :: !(Maybe Nat),
    _itsConfigurations ::
      !(Maybe [Configuration]),
    _itsEBSBlockDevices ::
      !(Maybe [EBSBlockDevice]),
    _itsInstanceType :: !(Maybe Text),
    _itsEBSOptimized :: !(Maybe Bool),
    _itsBidPriceAsPercentageOfOnDemandPrice ::
      !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceTypeSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itsBidPrice' - The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD.
--
-- * 'itsWeightedCapacity' - The number of units that a provisioned instance of this type provides toward fulfilling the target capacities defined in 'InstanceFleetConfig' . Capacity values represent performance characteristics such as vCPUs, memory, or I/O. If not specified, the default value is 1.
--
-- * 'itsConfigurations' - A configuration classification that applies when provisioning cluster instances, which can include configurations for applications and software bundled with Amazon EMR.
--
-- * 'itsEBSBlockDevices' - The configuration of Amazon Elastic Block Storage (Amazon EBS) attached to each instance as defined by @InstanceType@ .
--
-- * 'itsInstanceType' - The EC2 instance type, for example @m3.xlarge@ .
--
-- * 'itsEBSOptimized' - Evaluates to @TRUE@ when the specified @InstanceType@ is EBS-optimized.
--
-- * 'itsBidPriceAsPercentageOfOnDemandPrice' - The bid price, as a percentage of On-Demand price, for each EC2 Spot Instance as defined by @InstanceType@ . Expressed as a number (for example, 20 specifies 20%).
instanceTypeSpecification ::
  InstanceTypeSpecification
instanceTypeSpecification =
  InstanceTypeSpecification'
    { _itsBidPrice = Nothing,
      _itsWeightedCapacity = Nothing,
      _itsConfigurations = Nothing,
      _itsEBSBlockDevices = Nothing,
      _itsInstanceType = Nothing,
      _itsEBSOptimized = Nothing,
      _itsBidPriceAsPercentageOfOnDemandPrice = Nothing
    }

-- | The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD.
itsBidPrice :: Lens' InstanceTypeSpecification (Maybe Text)
itsBidPrice = lens _itsBidPrice (\s a -> s {_itsBidPrice = a})

-- | The number of units that a provisioned instance of this type provides toward fulfilling the target capacities defined in 'InstanceFleetConfig' . Capacity values represent performance characteristics such as vCPUs, memory, or I/O. If not specified, the default value is 1.
itsWeightedCapacity :: Lens' InstanceTypeSpecification (Maybe Natural)
itsWeightedCapacity = lens _itsWeightedCapacity (\s a -> s {_itsWeightedCapacity = a}) . mapping _Nat

-- | A configuration classification that applies when provisioning cluster instances, which can include configurations for applications and software bundled with Amazon EMR.
itsConfigurations :: Lens' InstanceTypeSpecification [Configuration]
itsConfigurations = lens _itsConfigurations (\s a -> s {_itsConfigurations = a}) . _Default . _Coerce

-- | The configuration of Amazon Elastic Block Storage (Amazon EBS) attached to each instance as defined by @InstanceType@ .
itsEBSBlockDevices :: Lens' InstanceTypeSpecification [EBSBlockDevice]
itsEBSBlockDevices = lens _itsEBSBlockDevices (\s a -> s {_itsEBSBlockDevices = a}) . _Default . _Coerce

-- | The EC2 instance type, for example @m3.xlarge@ .
itsInstanceType :: Lens' InstanceTypeSpecification (Maybe Text)
itsInstanceType = lens _itsInstanceType (\s a -> s {_itsInstanceType = a})

-- | Evaluates to @TRUE@ when the specified @InstanceType@ is EBS-optimized.
itsEBSOptimized :: Lens' InstanceTypeSpecification (Maybe Bool)
itsEBSOptimized = lens _itsEBSOptimized (\s a -> s {_itsEBSOptimized = a})

-- | The bid price, as a percentage of On-Demand price, for each EC2 Spot Instance as defined by @InstanceType@ . Expressed as a number (for example, 20 specifies 20%).
itsBidPriceAsPercentageOfOnDemandPrice :: Lens' InstanceTypeSpecification (Maybe Double)
itsBidPriceAsPercentageOfOnDemandPrice = lens _itsBidPriceAsPercentageOfOnDemandPrice (\s a -> s {_itsBidPriceAsPercentageOfOnDemandPrice = a})

instance FromJSON InstanceTypeSpecification where
  parseJSON =
    withObject
      "InstanceTypeSpecification"
      ( \x ->
          InstanceTypeSpecification'
            <$> (x .:? "BidPrice")
            <*> (x .:? "WeightedCapacity")
            <*> (x .:? "Configurations" .!= mempty)
            <*> (x .:? "EbsBlockDevices" .!= mempty)
            <*> (x .:? "InstanceType")
            <*> (x .:? "EbsOptimized")
            <*> (x .:? "BidPriceAsPercentageOfOnDemandPrice")
      )

instance Hashable InstanceTypeSpecification

instance NFData InstanceTypeSpecification
