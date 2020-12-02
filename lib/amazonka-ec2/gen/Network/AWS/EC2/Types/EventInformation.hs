{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EventInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EventInformation where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an EC2 Fleet or Spot Fleet event.
--
--
--
-- /See:/ 'eventInformation' smart constructor.
data EventInformation = EventInformation'
  { _eiInstanceId ::
      !(Maybe Text),
    _eiEventDescription :: !(Maybe Text),
    _eiEventSubType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiInstanceId' - The ID of the instance. This information is available only for @instanceChange@ events.
--
-- * 'eiEventDescription' - The description of the event.
--
-- * 'eiEventSubType' - The event. The following are the @error@ events:     * @iamFleetRoleInvalid@ - The EC2 Fleet or Spot Fleet did not have the required permissions either to launch or terminate an instance.     * @spotFleetRequestConfigurationInvalid@ - The configuration is not valid. For more information, see the description of the event.     * @spotInstanceCountLimitExceeded@ - You've reached the limit on the number of Spot Instances that you can launch. The following are the @fleetRequestChange@ events:     * @active@ - The EC2 Fleet or Spot Fleet request has been validated and Amazon EC2 is attempting to maintain the target number of running Spot Instances.     * @cancelled@ - The EC2 Fleet or Spot Fleet request is canceled and has no running Spot Instances. The EC2 Fleet or Spot Fleet will be deleted two days after its instances were terminated.     * @cancelled_running@ - The EC2 Fleet or Spot Fleet request is canceled and does not launch additional Spot Instances. Existing Spot Instances continue to run until they are interrupted or terminated.     * @cancelled_terminating@ - The EC2 Fleet or Spot Fleet request is canceled and its Spot Instances are terminating.     * @expired@ - The EC2 Fleet or Spot Fleet request has expired. A subsequent event indicates that the instances were terminated, if the request was created with @TerminateInstancesWithExpiration@ set.     * @modify_in_progress@ - A request to modify the EC2 Fleet or Spot Fleet request was accepted and is in progress.     * @modify_successful@ - The EC2 Fleet or Spot Fleet request was modified.     * @price_update@ - The price for a launch configuration was adjusted because it was too high. This change is permanent.     * @submitted@ - The EC2 Fleet or Spot Fleet request is being evaluated and Amazon EC2 is preparing to launch the target number of Spot Instances. The following are the @instanceChange@ events:     * @launched@ - A request was fulfilled and a new instance was launched.     * @terminated@ - An instance was terminated by the user. The following are the @Information@ events:     * @launchSpecTemporarilyBlacklisted@ - The configuration is not valid and several attempts to launch instances have failed. For more information, see the description of the event.     * @launchSpecUnusable@ - The price in a launch specification is not valid because it is below the Spot price or the Spot price is above the On-Demand price.     * @fleetProgressHalted@ - The price in every launch specification is not valid. A launch specification might become valid if the Spot price changes.
eventInformation ::
  EventInformation
eventInformation =
  EventInformation'
    { _eiInstanceId = Nothing,
      _eiEventDescription = Nothing,
      _eiEventSubType = Nothing
    }

-- | The ID of the instance. This information is available only for @instanceChange@ events.
eiInstanceId :: Lens' EventInformation (Maybe Text)
eiInstanceId = lens _eiInstanceId (\s a -> s {_eiInstanceId = a})

-- | The description of the event.
eiEventDescription :: Lens' EventInformation (Maybe Text)
eiEventDescription = lens _eiEventDescription (\s a -> s {_eiEventDescription = a})

-- | The event. The following are the @error@ events:     * @iamFleetRoleInvalid@ - The EC2 Fleet or Spot Fleet did not have the required permissions either to launch or terminate an instance.     * @spotFleetRequestConfigurationInvalid@ - The configuration is not valid. For more information, see the description of the event.     * @spotInstanceCountLimitExceeded@ - You've reached the limit on the number of Spot Instances that you can launch. The following are the @fleetRequestChange@ events:     * @active@ - The EC2 Fleet or Spot Fleet request has been validated and Amazon EC2 is attempting to maintain the target number of running Spot Instances.     * @cancelled@ - The EC2 Fleet or Spot Fleet request is canceled and has no running Spot Instances. The EC2 Fleet or Spot Fleet will be deleted two days after its instances were terminated.     * @cancelled_running@ - The EC2 Fleet or Spot Fleet request is canceled and does not launch additional Spot Instances. Existing Spot Instances continue to run until they are interrupted or terminated.     * @cancelled_terminating@ - The EC2 Fleet or Spot Fleet request is canceled and its Spot Instances are terminating.     * @expired@ - The EC2 Fleet or Spot Fleet request has expired. A subsequent event indicates that the instances were terminated, if the request was created with @TerminateInstancesWithExpiration@ set.     * @modify_in_progress@ - A request to modify the EC2 Fleet or Spot Fleet request was accepted and is in progress.     * @modify_successful@ - The EC2 Fleet or Spot Fleet request was modified.     * @price_update@ - The price for a launch configuration was adjusted because it was too high. This change is permanent.     * @submitted@ - The EC2 Fleet or Spot Fleet request is being evaluated and Amazon EC2 is preparing to launch the target number of Spot Instances. The following are the @instanceChange@ events:     * @launched@ - A request was fulfilled and a new instance was launched.     * @terminated@ - An instance was terminated by the user. The following are the @Information@ events:     * @launchSpecTemporarilyBlacklisted@ - The configuration is not valid and several attempts to launch instances have failed. For more information, see the description of the event.     * @launchSpecUnusable@ - The price in a launch specification is not valid because it is below the Spot price or the Spot price is above the On-Demand price.     * @fleetProgressHalted@ - The price in every launch specification is not valid. A launch specification might become valid if the Spot price changes.
eiEventSubType :: Lens' EventInformation (Maybe Text)
eiEventSubType = lens _eiEventSubType (\s a -> s {_eiEventSubType = a})

instance FromXML EventInformation where
  parseXML x =
    EventInformation'
      <$> (x .@? "instanceId")
      <*> (x .@? "eventDescription")
      <*> (x .@? "eventSubType")

instance Hashable EventInformation

instance NFData EventInformation
