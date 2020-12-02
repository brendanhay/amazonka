{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupConfig where

import Network.AWS.EMR.Types.AutoScalingPolicy
import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EBSConfiguration
import Network.AWS.EMR.Types.InstanceRoleType
import Network.AWS.EMR.Types.MarketType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration defining a new instance group.
--
--
--
-- /See:/ 'instanceGroupConfig' smart constructor.
data InstanceGroupConfig = InstanceGroupConfig'
  { _igcEBSConfiguration ::
      !(Maybe EBSConfiguration),
    _igcBidPrice :: !(Maybe Text),
    _igcConfigurations :: !(Maybe [Configuration]),
    _igcMarket :: !(Maybe MarketType),
    _igcName :: !(Maybe Text),
    _igcAutoScalingPolicy :: !(Maybe AutoScalingPolicy),
    _igcInstanceRole :: !InstanceRoleType,
    _igcInstanceType :: !Text,
    _igcInstanceCount :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceGroupConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'igcEBSConfiguration' - EBS configurations that will be attached to each EC2 instance in the instance group.
--
-- * 'igcBidPrice' - The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- * 'igcConfigurations' - The list of configurations supplied for an EMR cluster instance group. You can specify a separate configuration for each instance group (master, core, and task).
--
-- * 'igcMarket' - Market type of the EC2 instances used to create a cluster node.
--
-- * 'igcName' - Friendly name given to the instance group.
--
-- * 'igcAutoScalingPolicy' - An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See 'PutAutoScalingPolicy' .
--
-- * 'igcInstanceRole' - The role of the instance group in the cluster.
--
-- * 'igcInstanceType' - The EC2 instance type for all instances in the instance group.
--
-- * 'igcInstanceCount' - Target number of instances for the instance group.
instanceGroupConfig ::
  -- | 'igcInstanceRole'
  InstanceRoleType ->
  -- | 'igcInstanceType'
  Text ->
  -- | 'igcInstanceCount'
  Int ->
  InstanceGroupConfig
instanceGroupConfig pInstanceRole_ pInstanceType_ pInstanceCount_ =
  InstanceGroupConfig'
    { _igcEBSConfiguration = Nothing,
      _igcBidPrice = Nothing,
      _igcConfigurations = Nothing,
      _igcMarket = Nothing,
      _igcName = Nothing,
      _igcAutoScalingPolicy = Nothing,
      _igcInstanceRole = pInstanceRole_,
      _igcInstanceType = pInstanceType_,
      _igcInstanceCount = pInstanceCount_
    }

-- | EBS configurations that will be attached to each EC2 instance in the instance group.
igcEBSConfiguration :: Lens' InstanceGroupConfig (Maybe EBSConfiguration)
igcEBSConfiguration = lens _igcEBSConfiguration (\s a -> s {_igcEBSConfiguration = a})

-- | The bid price for each EC2 Spot Instance type as defined by @InstanceType@ . Expressed in USD. If neither @BidPrice@ nor @BidPriceAsPercentageOfOnDemandPrice@ is provided, @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
igcBidPrice :: Lens' InstanceGroupConfig (Maybe Text)
igcBidPrice = lens _igcBidPrice (\s a -> s {_igcBidPrice = a})

-- | The list of configurations supplied for an EMR cluster instance group. You can specify a separate configuration for each instance group (master, core, and task).
igcConfigurations :: Lens' InstanceGroupConfig [Configuration]
igcConfigurations = lens _igcConfigurations (\s a -> s {_igcConfigurations = a}) . _Default . _Coerce

-- | Market type of the EC2 instances used to create a cluster node.
igcMarket :: Lens' InstanceGroupConfig (Maybe MarketType)
igcMarket = lens _igcMarket (\s a -> s {_igcMarket = a})

-- | Friendly name given to the instance group.
igcName :: Lens' InstanceGroupConfig (Maybe Text)
igcName = lens _igcName (\s a -> s {_igcName = a})

-- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See 'PutAutoScalingPolicy' .
igcAutoScalingPolicy :: Lens' InstanceGroupConfig (Maybe AutoScalingPolicy)
igcAutoScalingPolicy = lens _igcAutoScalingPolicy (\s a -> s {_igcAutoScalingPolicy = a})

-- | The role of the instance group in the cluster.
igcInstanceRole :: Lens' InstanceGroupConfig InstanceRoleType
igcInstanceRole = lens _igcInstanceRole (\s a -> s {_igcInstanceRole = a})

-- | The EC2 instance type for all instances in the instance group.
igcInstanceType :: Lens' InstanceGroupConfig Text
igcInstanceType = lens _igcInstanceType (\s a -> s {_igcInstanceType = a})

-- | Target number of instances for the instance group.
igcInstanceCount :: Lens' InstanceGroupConfig Int
igcInstanceCount = lens _igcInstanceCount (\s a -> s {_igcInstanceCount = a})

instance Hashable InstanceGroupConfig

instance NFData InstanceGroupConfig

instance ToJSON InstanceGroupConfig where
  toJSON InstanceGroupConfig' {..} =
    object
      ( catMaybes
          [ ("EbsConfiguration" .=) <$> _igcEBSConfiguration,
            ("BidPrice" .=) <$> _igcBidPrice,
            ("Configurations" .=) <$> _igcConfigurations,
            ("Market" .=) <$> _igcMarket,
            ("Name" .=) <$> _igcName,
            ("AutoScalingPolicy" .=) <$> _igcAutoScalingPolicy,
            Just ("InstanceRole" .= _igcInstanceRole),
            Just ("InstanceType" .= _igcInstanceType),
            Just ("InstanceCount" .= _igcInstanceCount)
          ]
      )
