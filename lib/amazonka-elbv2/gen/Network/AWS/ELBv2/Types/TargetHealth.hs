{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetHealth where

import Network.AWS.ELBv2.Types.TargetHealthReasonEnum
import Network.AWS.ELBv2.Types.TargetHealthStateEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the current health of a target.
--
--
--
-- /See:/ 'targetHealth' smart constructor.
data TargetHealth = TargetHealth'
  { _thState ::
      !(Maybe TargetHealthStateEnum),
    _thReason :: !(Maybe TargetHealthReasonEnum),
    _thDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetHealth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'thState' - The state of the target.
--
-- * 'thReason' - The reason code. If the target state is @healthy@ , a reason code is not provided. If the target state is @initial@ , the reason code can be one of the following values:     * @Elb.RegistrationInProgress@ - The target is in the process of being registered with the load balancer.     * @Elb.InitialHealthChecking@ - The load balancer is still sending the target the minimum number of health checks required to determine its health status. If the target state is @unhealthy@ , the reason code can be one of the following values:     * @Target.ResponseCodeMismatch@ - The health checks did not return an expected HTTP code. Applies only to Application Load Balancers and Gateway Load Balancers.     * @Target.Timeout@ - The health check requests timed out. Applies only to Application Load Balancers and Gateway Load Balancers.     * @Target.FailedHealthChecks@ - The load balancer received an error while establishing a connection to the target or the target response was malformed.     * @Elb.InternalError@ - The health checks failed due to an internal error. Applies only to Application Load Balancers. If the target state is @unused@ , the reason code can be one of the following values:     * @Target.NotRegistered@ - The target is not registered with the target group.     * @Target.NotInUse@ - The target group is not used by any load balancer or the target is in an Availability Zone that is not enabled for its load balancer.     * @Target.InvalidState@ - The target is in the stopped or terminated state.     * @Target.IpUnusable@ - The target IP address is reserved for use by a load balancer. If the target state is @draining@ , the reason code can be the following value:     * @Target.DeregistrationInProgress@ - The target is in the process of being deregistered and the deregistration delay period has not expired. If the target state is @unavailable@ , the reason code can be the following value:     * @Target.HealthCheckDisabled@ - Health checks are disabled for the target group. Applies only to Application Load Balancers.     * @Elb.InternalError@ - Target health is unavailable due to an internal error. Applies only to Network Load Balancers.
--
-- * 'thDescription' - A description of the target health that provides additional details. If the state is @healthy@ , a description is not provided.
targetHealth ::
  TargetHealth
targetHealth =
  TargetHealth'
    { _thState = Nothing,
      _thReason = Nothing,
      _thDescription = Nothing
    }

-- | The state of the target.
thState :: Lens' TargetHealth (Maybe TargetHealthStateEnum)
thState = lens _thState (\s a -> s {_thState = a})

-- | The reason code. If the target state is @healthy@ , a reason code is not provided. If the target state is @initial@ , the reason code can be one of the following values:     * @Elb.RegistrationInProgress@ - The target is in the process of being registered with the load balancer.     * @Elb.InitialHealthChecking@ - The load balancer is still sending the target the minimum number of health checks required to determine its health status. If the target state is @unhealthy@ , the reason code can be one of the following values:     * @Target.ResponseCodeMismatch@ - The health checks did not return an expected HTTP code. Applies only to Application Load Balancers and Gateway Load Balancers.     * @Target.Timeout@ - The health check requests timed out. Applies only to Application Load Balancers and Gateway Load Balancers.     * @Target.FailedHealthChecks@ - The load balancer received an error while establishing a connection to the target or the target response was malformed.     * @Elb.InternalError@ - The health checks failed due to an internal error. Applies only to Application Load Balancers. If the target state is @unused@ , the reason code can be one of the following values:     * @Target.NotRegistered@ - The target is not registered with the target group.     * @Target.NotInUse@ - The target group is not used by any load balancer or the target is in an Availability Zone that is not enabled for its load balancer.     * @Target.InvalidState@ - The target is in the stopped or terminated state.     * @Target.IpUnusable@ - The target IP address is reserved for use by a load balancer. If the target state is @draining@ , the reason code can be the following value:     * @Target.DeregistrationInProgress@ - The target is in the process of being deregistered and the deregistration delay period has not expired. If the target state is @unavailable@ , the reason code can be the following value:     * @Target.HealthCheckDisabled@ - Health checks are disabled for the target group. Applies only to Application Load Balancers.     * @Elb.InternalError@ - Target health is unavailable due to an internal error. Applies only to Network Load Balancers.
thReason :: Lens' TargetHealth (Maybe TargetHealthReasonEnum)
thReason = lens _thReason (\s a -> s {_thReason = a})

-- | A description of the target health that provides additional details. If the state is @healthy@ , a description is not provided.
thDescription :: Lens' TargetHealth (Maybe Text)
thDescription = lens _thDescription (\s a -> s {_thDescription = a})

instance FromXML TargetHealth where
  parseXML x =
    TargetHealth'
      <$> (x .@? "State") <*> (x .@? "Reason") <*> (x .@? "Description")

instance Hashable TargetHealth

instance NFData TargetHealth
