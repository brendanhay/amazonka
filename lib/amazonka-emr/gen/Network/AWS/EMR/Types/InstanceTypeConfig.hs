{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceTypeConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceTypeConfig where

import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EBSConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An instance type configuration for each instance type in an instance fleet, which determines the EC2 instances Amazon EMR attempts to provision to fulfill On-Demand and Spot target capacities. There can be a maximum of five instance type configurations in a fleet.
--
--
--
-- /See:/ 'instanceTypeConfig' smart constructor.
data InstanceTypeConfig = InstanceTypeConfig'
  { _itcEBSConfiguration ::
      !(Maybe EBSConfiguration),
    _itcBidPrice :: !(Maybe Text),
    _itcWeightedCapacity :: !(Maybe Nat),
    _itcConfigurations :: !(Maybe [Configuration]),
    _itcBidPriceAsPercentageOfOnDemandPrice ::
      !(Maybe Double),
    _itcInstanceType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceTypeConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itcEBSConfiguration' - The configuration of Amazon Elastic Block Storage (Amazon EBS) attached to each instance as defined by @InstanceType@ .
--
-- * 'itcBidPrice' - The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- * 'itcWeightedCapacity' - The number of units that a provisioned instance of this type provides toward fulfilling the target capacities defined in 'InstanceFleetConfig' . This value is 1 for a master instance fleet, and must be 1 or greater for core and task instance fleets. Defaults to 1 if not specified.
--
-- * 'itcConfigurations' - A configuration classification that applies when provisioning cluster instances, which can include configurations for applications and software that run on the cluster.
--
-- * 'itcBidPriceAsPercentageOfOnDemandPrice' - The bid price, as a percentage of On-Demand price, for each EC2 Spot Instance as defined by @InstanceType@ . Expressed as a number (for example, 20 specifies 20%). If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- * 'itcInstanceType' - An EC2 instance type, such as @m3.xlarge@ .
instanceTypeConfig ::
  -- | 'itcInstanceType'
  Text ->
  InstanceTypeConfig
instanceTypeConfig pInstanceType_ =
  InstanceTypeConfig'
    { _itcEBSConfiguration = Nothing,
      _itcBidPrice = Nothing,
      _itcWeightedCapacity = Nothing,
      _itcConfigurations = Nothing,
      _itcBidPriceAsPercentageOfOnDemandPrice = Nothing,
      _itcInstanceType = pInstanceType_
    }

-- | The configuration of Amazon Elastic Block Storage (Amazon EBS) attached to each instance as defined by @InstanceType@ .
itcEBSConfiguration :: Lens' InstanceTypeConfig (Maybe EBSConfiguration)
itcEBSConfiguration = lens _itcEBSConfiguration (\s a -> s {_itcEBSConfiguration = a})

-- | The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
itcBidPrice :: Lens' InstanceTypeConfig (Maybe Text)
itcBidPrice = lens _itcBidPrice (\s a -> s {_itcBidPrice = a})

-- | The number of units that a provisioned instance of this type provides toward fulfilling the target capacities defined in 'InstanceFleetConfig' . This value is 1 for a master instance fleet, and must be 1 or greater for core and task instance fleets. Defaults to 1 if not specified.
itcWeightedCapacity :: Lens' InstanceTypeConfig (Maybe Natural)
itcWeightedCapacity = lens _itcWeightedCapacity (\s a -> s {_itcWeightedCapacity = a}) . mapping _Nat

-- | A configuration classification that applies when provisioning cluster instances, which can include configurations for applications and software that run on the cluster.
itcConfigurations :: Lens' InstanceTypeConfig [Configuration]
itcConfigurations = lens _itcConfigurations (\s a -> s {_itcConfigurations = a}) . _Default . _Coerce

-- | The bid price, as a percentage of On-Demand price, for each EC2 Spot Instance as defined by @InstanceType@ . Expressed as a number (for example, 20 specifies 20%). If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
itcBidPriceAsPercentageOfOnDemandPrice :: Lens' InstanceTypeConfig (Maybe Double)
itcBidPriceAsPercentageOfOnDemandPrice = lens _itcBidPriceAsPercentageOfOnDemandPrice (\s a -> s {_itcBidPriceAsPercentageOfOnDemandPrice = a})

-- | An EC2 instance type, such as @m3.xlarge@ .
itcInstanceType :: Lens' InstanceTypeConfig Text
itcInstanceType = lens _itcInstanceType (\s a -> s {_itcInstanceType = a})

instance Hashable InstanceTypeConfig

instance NFData InstanceTypeConfig

instance ToJSON InstanceTypeConfig where
  toJSON InstanceTypeConfig' {..} =
    object
      ( catMaybes
          [ ("EbsConfiguration" .=) <$> _itcEBSConfiguration,
            ("BidPrice" .=) <$> _itcBidPrice,
            ("WeightedCapacity" .=) <$> _itcWeightedCapacity,
            ("Configurations" .=) <$> _itcConfigurations,
            ("BidPriceAsPercentageOfOnDemandPrice" .=)
              <$> _itcBidPriceAsPercentageOfOnDemandPrice,
            Just ("InstanceType" .= _itcInstanceType)
          ]
      )
