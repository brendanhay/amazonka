{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SpotProvisioningSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SpotProvisioningSpecification where

import Network.AWS.EMR.Types.SpotProvisioningAllocationStrategy
import Network.AWS.EMR.Types.SpotProvisioningTimeoutAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The launch specification for Spot Instances in the instance fleet, which determines the defined duration, provisioning timeout behavior, and allocation strategy.
--
--
--
-- /See:/ 'spotProvisioningSpecification' smart constructor.
data SpotProvisioningSpecification = SpotProvisioningSpecification'
  { _spsBlockDurationMinutes ::
      !(Maybe Nat),
    _spsAllocationStrategy ::
      !( Maybe
           SpotProvisioningAllocationStrategy
       ),
    _spsTimeoutDurationMinutes ::
      !Nat,
    _spsTimeoutAction ::
      !SpotProvisioningTimeoutAction
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SpotProvisioningSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spsBlockDurationMinutes' - The defined duration for Spot Instances (also known as Spot blocks) in minutes. When specified, the Spot Instance does not terminate before the defined duration expires, and defined duration pricing for Spot instances applies. Valid values are 60, 120, 180, 240, 300, or 360. The duration period starts as soon as a Spot Instance receives its instance ID. At the end of the duration, Amazon EC2 marks the Spot Instance for termination and provides a Spot Instance termination notice, which gives the instance a two-minute warning before it terminates.
--
-- * 'spsAllocationStrategy' - Specifies the strategy to use in launching Spot Instance fleets. Currently, the only option is capacity-optimized (the default), which launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
--
-- * 'spsTimeoutDurationMinutes' - The spot provisioning timeout period in minutes. If Spot Instances are not provisioned within this time period, the @TimeOutAction@ is taken. Minimum value is 5 and maximum value is 1440. The timeout applies only during initial provisioning, when the cluster is first created.
--
-- * 'spsTimeoutAction' - The action to take when @TargetSpotCapacity@ has not been fulfilled when the @TimeoutDurationMinutes@ has expired; that is, when all Spot Instances could not be provisioned within the Spot provisioning timeout. Valid values are @TERMINATE_CLUSTER@ and @SWITCH_TO_ON_DEMAND@ . SWITCH_TO_ON_DEMAND specifies that if no Spot Instances are available, On-Demand Instances should be provisioned to fulfill any remaining Spot capacity.
spotProvisioningSpecification ::
  -- | 'spsTimeoutDurationMinutes'
  Natural ->
  -- | 'spsTimeoutAction'
  SpotProvisioningTimeoutAction ->
  SpotProvisioningSpecification
spotProvisioningSpecification
  pTimeoutDurationMinutes_
  pTimeoutAction_ =
    SpotProvisioningSpecification'
      { _spsBlockDurationMinutes =
          Nothing,
        _spsAllocationStrategy = Nothing,
        _spsTimeoutDurationMinutes = _Nat # pTimeoutDurationMinutes_,
        _spsTimeoutAction = pTimeoutAction_
      }

-- | The defined duration for Spot Instances (also known as Spot blocks) in minutes. When specified, the Spot Instance does not terminate before the defined duration expires, and defined duration pricing for Spot instances applies. Valid values are 60, 120, 180, 240, 300, or 360. The duration period starts as soon as a Spot Instance receives its instance ID. At the end of the duration, Amazon EC2 marks the Spot Instance for termination and provides a Spot Instance termination notice, which gives the instance a two-minute warning before it terminates.
spsBlockDurationMinutes :: Lens' SpotProvisioningSpecification (Maybe Natural)
spsBlockDurationMinutes = lens _spsBlockDurationMinutes (\s a -> s {_spsBlockDurationMinutes = a}) . mapping _Nat

-- | Specifies the strategy to use in launching Spot Instance fleets. Currently, the only option is capacity-optimized (the default), which launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
spsAllocationStrategy :: Lens' SpotProvisioningSpecification (Maybe SpotProvisioningAllocationStrategy)
spsAllocationStrategy = lens _spsAllocationStrategy (\s a -> s {_spsAllocationStrategy = a})

-- | The spot provisioning timeout period in minutes. If Spot Instances are not provisioned within this time period, the @TimeOutAction@ is taken. Minimum value is 5 and maximum value is 1440. The timeout applies only during initial provisioning, when the cluster is first created.
spsTimeoutDurationMinutes :: Lens' SpotProvisioningSpecification Natural
spsTimeoutDurationMinutes = lens _spsTimeoutDurationMinutes (\s a -> s {_spsTimeoutDurationMinutes = a}) . _Nat

-- | The action to take when @TargetSpotCapacity@ has not been fulfilled when the @TimeoutDurationMinutes@ has expired; that is, when all Spot Instances could not be provisioned within the Spot provisioning timeout. Valid values are @TERMINATE_CLUSTER@ and @SWITCH_TO_ON_DEMAND@ . SWITCH_TO_ON_DEMAND specifies that if no Spot Instances are available, On-Demand Instances should be provisioned to fulfill any remaining Spot capacity.
spsTimeoutAction :: Lens' SpotProvisioningSpecification SpotProvisioningTimeoutAction
spsTimeoutAction = lens _spsTimeoutAction (\s a -> s {_spsTimeoutAction = a})

instance FromJSON SpotProvisioningSpecification where
  parseJSON =
    withObject
      "SpotProvisioningSpecification"
      ( \x ->
          SpotProvisioningSpecification'
            <$> (x .:? "BlockDurationMinutes")
            <*> (x .:? "AllocationStrategy")
            <*> (x .: "TimeoutDurationMinutes")
            <*> (x .: "TimeoutAction")
      )

instance Hashable SpotProvisioningSpecification

instance NFData SpotProvisioningSpecification

instance ToJSON SpotProvisioningSpecification where
  toJSON SpotProvisioningSpecification' {..} =
    object
      ( catMaybes
          [ ("BlockDurationMinutes" .=) <$> _spsBlockDurationMinutes,
            ("AllocationStrategy" .=) <$> _spsAllocationStrategy,
            Just ("TimeoutDurationMinutes" .= _spsTimeoutDurationMinutes),
            Just ("TimeoutAction" .= _spsTimeoutAction)
          ]
      )
