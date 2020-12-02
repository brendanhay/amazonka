{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.InstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.InstanceState where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the state of an EC2 instance.
--
--
--
-- /See:/ 'instanceState' smart constructor.
data InstanceState = InstanceState'
  { _isInstanceId :: !(Maybe Text),
    _isState :: !(Maybe Text),
    _isReasonCode :: !(Maybe Text),
    _isDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isInstanceId' - The ID of the instance.
--
-- * 'isState' - The current state of the instance. Valid values: @InService@ | @OutOfService@ | @Unknown@
--
-- * 'isReasonCode' - Information about the cause of @OutOfService@ instances. Specifically, whether the cause is Elastic Load Balancing or the instance. Valid values: @ELB@ | @Instance@ | @N/A@
--
-- * 'isDescription' - A description of the instance state. This string can contain one or more of the following messages.     * @N/A@      * @A transient error occurred. Please try again later.@      * @Instance has failed at least the UnhealthyThreshold number of health checks consecutively.@      * @Instance has not passed the configured HealthyThreshold number of health checks consecutively.@      * @Instance registration is still in progress.@      * @Instance is in the EC2 Availability Zone for which LoadBalancer is not configured to route traffic to.@      * @Instance is not currently registered with the LoadBalancer.@      * @Instance deregistration currently in progress.@      * @Disable Availability Zone is currently in progress.@      * @Instance is in pending state.@      * @Instance is in stopped state.@      * @Instance is in terminated state.@
instanceState ::
  InstanceState
instanceState =
  InstanceState'
    { _isInstanceId = Nothing,
      _isState = Nothing,
      _isReasonCode = Nothing,
      _isDescription = Nothing
    }

-- | The ID of the instance.
isInstanceId :: Lens' InstanceState (Maybe Text)
isInstanceId = lens _isInstanceId (\s a -> s {_isInstanceId = a})

-- | The current state of the instance. Valid values: @InService@ | @OutOfService@ | @Unknown@
isState :: Lens' InstanceState (Maybe Text)
isState = lens _isState (\s a -> s {_isState = a})

-- | Information about the cause of @OutOfService@ instances. Specifically, whether the cause is Elastic Load Balancing or the instance. Valid values: @ELB@ | @Instance@ | @N/A@
isReasonCode :: Lens' InstanceState (Maybe Text)
isReasonCode = lens _isReasonCode (\s a -> s {_isReasonCode = a})

-- | A description of the instance state. This string can contain one or more of the following messages.     * @N/A@      * @A transient error occurred. Please try again later.@      * @Instance has failed at least the UnhealthyThreshold number of health checks consecutively.@      * @Instance has not passed the configured HealthyThreshold number of health checks consecutively.@      * @Instance registration is still in progress.@      * @Instance is in the EC2 Availability Zone for which LoadBalancer is not configured to route traffic to.@      * @Instance is not currently registered with the LoadBalancer.@      * @Instance deregistration currently in progress.@      * @Disable Availability Zone is currently in progress.@      * @Instance is in pending state.@      * @Instance is in stopped state.@      * @Instance is in terminated state.@
isDescription :: Lens' InstanceState (Maybe Text)
isDescription = lens _isDescription (\s a -> s {_isDescription = a})

instance FromXML InstanceState where
  parseXML x =
    InstanceState'
      <$> (x .@? "InstanceId")
      <*> (x .@? "State")
      <*> (x .@? "ReasonCode")
      <*> (x .@? "Description")

instance Hashable InstanceState

instance NFData InstanceState
