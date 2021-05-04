{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EventInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EventInformation where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an EC2 Fleet or Spot Fleet event.
--
-- /See:/ 'newEventInformation' smart constructor.
data EventInformation = EventInformation'
  { -- | The ID of the instance. This information is available only for
    -- @instanceChange@ events.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The description of the event.
    eventDescription :: Prelude.Maybe Prelude.Text,
    -- | The event.
    --
    -- The following are the @error@ events:
    --
    -- -   @iamFleetRoleInvalid@ - The EC2 Fleet or Spot Fleet did not have the
    --     required permissions either to launch or terminate an instance.
    --
    -- -   @spotFleetRequestConfigurationInvalid@ - The configuration is not
    --     valid. For more information, see the description of the event.
    --
    -- -   @spotInstanceCountLimitExceeded@ - You\'ve reached the limit on the
    --     number of Spot Instances that you can launch.
    --
    -- The following are the @fleetRequestChange@ events:
    --
    -- -   @active@ - The EC2 Fleet or Spot Fleet request has been validated
    --     and Amazon EC2 is attempting to maintain the target number of
    --     running Spot Instances.
    --
    -- -   @cancelled@ - The EC2 Fleet or Spot Fleet request is canceled and
    --     has no running Spot Instances. The EC2 Fleet or Spot Fleet will be
    --     deleted two days after its instances were terminated.
    --
    -- -   @cancelled_running@ - The EC2 Fleet or Spot Fleet request is
    --     canceled and does not launch additional Spot Instances. Existing
    --     Spot Instances continue to run until they are interrupted or
    --     terminated.
    --
    -- -   @cancelled_terminating@ - The EC2 Fleet or Spot Fleet request is
    --     canceled and its Spot Instances are terminating.
    --
    -- -   @expired@ - The EC2 Fleet or Spot Fleet request has expired. A
    --     subsequent event indicates that the instances were terminated, if
    --     the request was created with @TerminateInstancesWithExpiration@ set.
    --
    -- -   @modify_in_progress@ - A request to modify the EC2 Fleet or Spot
    --     Fleet request was accepted and is in progress.
    --
    -- -   @modify_successful@ - The EC2 Fleet or Spot Fleet request was
    --     modified.
    --
    -- -   @price_update@ - The price for a launch configuration was adjusted
    --     because it was too high. This change is permanent.
    --
    -- -   @submitted@ - The EC2 Fleet or Spot Fleet request is being evaluated
    --     and Amazon EC2 is preparing to launch the target number of Spot
    --     Instances.
    --
    -- The following are the @instanceChange@ events:
    --
    -- -   @launched@ - A request was fulfilled and a new instance was
    --     launched.
    --
    -- -   @terminated@ - An instance was terminated by the user.
    --
    -- The following are the @Information@ events:
    --
    -- -   @launchSpecTemporarilyBlacklisted@ - The configuration is not valid
    --     and several attempts to launch instances have failed. For more
    --     information, see the description of the event.
    --
    -- -   @launchSpecUnusable@ - The price in a launch specification is not
    --     valid because it is below the Spot price or the Spot price is above
    --     the On-Demand price.
    --
    -- -   @fleetProgressHalted@ - The price in every launch specification is
    --     not valid. A launch specification might become valid if the Spot
    --     price changes.
    eventSubType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EventInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'eventInformation_instanceId' - The ID of the instance. This information is available only for
-- @instanceChange@ events.
--
-- 'eventDescription', 'eventInformation_eventDescription' - The description of the event.
--
-- 'eventSubType', 'eventInformation_eventSubType' - The event.
--
-- The following are the @error@ events:
--
-- -   @iamFleetRoleInvalid@ - The EC2 Fleet or Spot Fleet did not have the
--     required permissions either to launch or terminate an instance.
--
-- -   @spotFleetRequestConfigurationInvalid@ - The configuration is not
--     valid. For more information, see the description of the event.
--
-- -   @spotInstanceCountLimitExceeded@ - You\'ve reached the limit on the
--     number of Spot Instances that you can launch.
--
-- The following are the @fleetRequestChange@ events:
--
-- -   @active@ - The EC2 Fleet or Spot Fleet request has been validated
--     and Amazon EC2 is attempting to maintain the target number of
--     running Spot Instances.
--
-- -   @cancelled@ - The EC2 Fleet or Spot Fleet request is canceled and
--     has no running Spot Instances. The EC2 Fleet or Spot Fleet will be
--     deleted two days after its instances were terminated.
--
-- -   @cancelled_running@ - The EC2 Fleet or Spot Fleet request is
--     canceled and does not launch additional Spot Instances. Existing
--     Spot Instances continue to run until they are interrupted or
--     terminated.
--
-- -   @cancelled_terminating@ - The EC2 Fleet or Spot Fleet request is
--     canceled and its Spot Instances are terminating.
--
-- -   @expired@ - The EC2 Fleet or Spot Fleet request has expired. A
--     subsequent event indicates that the instances were terminated, if
--     the request was created with @TerminateInstancesWithExpiration@ set.
--
-- -   @modify_in_progress@ - A request to modify the EC2 Fleet or Spot
--     Fleet request was accepted and is in progress.
--
-- -   @modify_successful@ - The EC2 Fleet or Spot Fleet request was
--     modified.
--
-- -   @price_update@ - The price for a launch configuration was adjusted
--     because it was too high. This change is permanent.
--
-- -   @submitted@ - The EC2 Fleet or Spot Fleet request is being evaluated
--     and Amazon EC2 is preparing to launch the target number of Spot
--     Instances.
--
-- The following are the @instanceChange@ events:
--
-- -   @launched@ - A request was fulfilled and a new instance was
--     launched.
--
-- -   @terminated@ - An instance was terminated by the user.
--
-- The following are the @Information@ events:
--
-- -   @launchSpecTemporarilyBlacklisted@ - The configuration is not valid
--     and several attempts to launch instances have failed. For more
--     information, see the description of the event.
--
-- -   @launchSpecUnusable@ - The price in a launch specification is not
--     valid because it is below the Spot price or the Spot price is above
--     the On-Demand price.
--
-- -   @fleetProgressHalted@ - The price in every launch specification is
--     not valid. A launch specification might become valid if the Spot
--     price changes.
newEventInformation ::
  EventInformation
newEventInformation =
  EventInformation'
    { instanceId = Prelude.Nothing,
      eventDescription = Prelude.Nothing,
      eventSubType = Prelude.Nothing
    }

-- | The ID of the instance. This information is available only for
-- @instanceChange@ events.
eventInformation_instanceId :: Lens.Lens' EventInformation (Prelude.Maybe Prelude.Text)
eventInformation_instanceId = Lens.lens (\EventInformation' {instanceId} -> instanceId) (\s@EventInformation' {} a -> s {instanceId = a} :: EventInformation)

-- | The description of the event.
eventInformation_eventDescription :: Lens.Lens' EventInformation (Prelude.Maybe Prelude.Text)
eventInformation_eventDescription = Lens.lens (\EventInformation' {eventDescription} -> eventDescription) (\s@EventInformation' {} a -> s {eventDescription = a} :: EventInformation)

-- | The event.
--
-- The following are the @error@ events:
--
-- -   @iamFleetRoleInvalid@ - The EC2 Fleet or Spot Fleet did not have the
--     required permissions either to launch or terminate an instance.
--
-- -   @spotFleetRequestConfigurationInvalid@ - The configuration is not
--     valid. For more information, see the description of the event.
--
-- -   @spotInstanceCountLimitExceeded@ - You\'ve reached the limit on the
--     number of Spot Instances that you can launch.
--
-- The following are the @fleetRequestChange@ events:
--
-- -   @active@ - The EC2 Fleet or Spot Fleet request has been validated
--     and Amazon EC2 is attempting to maintain the target number of
--     running Spot Instances.
--
-- -   @cancelled@ - The EC2 Fleet or Spot Fleet request is canceled and
--     has no running Spot Instances. The EC2 Fleet or Spot Fleet will be
--     deleted two days after its instances were terminated.
--
-- -   @cancelled_running@ - The EC2 Fleet or Spot Fleet request is
--     canceled and does not launch additional Spot Instances. Existing
--     Spot Instances continue to run until they are interrupted or
--     terminated.
--
-- -   @cancelled_terminating@ - The EC2 Fleet or Spot Fleet request is
--     canceled and its Spot Instances are terminating.
--
-- -   @expired@ - The EC2 Fleet or Spot Fleet request has expired. A
--     subsequent event indicates that the instances were terminated, if
--     the request was created with @TerminateInstancesWithExpiration@ set.
--
-- -   @modify_in_progress@ - A request to modify the EC2 Fleet or Spot
--     Fleet request was accepted and is in progress.
--
-- -   @modify_successful@ - The EC2 Fleet or Spot Fleet request was
--     modified.
--
-- -   @price_update@ - The price for a launch configuration was adjusted
--     because it was too high. This change is permanent.
--
-- -   @submitted@ - The EC2 Fleet or Spot Fleet request is being evaluated
--     and Amazon EC2 is preparing to launch the target number of Spot
--     Instances.
--
-- The following are the @instanceChange@ events:
--
-- -   @launched@ - A request was fulfilled and a new instance was
--     launched.
--
-- -   @terminated@ - An instance was terminated by the user.
--
-- The following are the @Information@ events:
--
-- -   @launchSpecTemporarilyBlacklisted@ - The configuration is not valid
--     and several attempts to launch instances have failed. For more
--     information, see the description of the event.
--
-- -   @launchSpecUnusable@ - The price in a launch specification is not
--     valid because it is below the Spot price or the Spot price is above
--     the On-Demand price.
--
-- -   @fleetProgressHalted@ - The price in every launch specification is
--     not valid. A launch specification might become valid if the Spot
--     price changes.
eventInformation_eventSubType :: Lens.Lens' EventInformation (Prelude.Maybe Prelude.Text)
eventInformation_eventSubType = Lens.lens (\EventInformation' {eventSubType} -> eventSubType) (\s@EventInformation' {} a -> s {eventSubType = a} :: EventInformation)

instance Prelude.FromXML EventInformation where
  parseXML x =
    EventInformation'
      Prelude.<$> (x Prelude..@? "instanceId")
      Prelude.<*> (x Prelude..@? "eventDescription")
      Prelude.<*> (x Prelude..@? "eventSubType")

instance Prelude.Hashable EventInformation

instance Prelude.NFData EventInformation
