{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EventInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EventInformation
  ( EventInformation (..),

    -- * Smart constructor
    mkEventInformation,

    -- * Lenses
    eiInstanceId,
    eiEventDescription,
    eiEventSubType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an EC2 Fleet or Spot Fleet event.
--
-- /See:/ 'mkEventInformation' smart constructor.
data EventInformation = EventInformation'
  { -- | The ID of the instance. This information is available only for @instanceChange@ events.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The description of the event.
    eventDescription :: Lude.Maybe Lude.Text,
    -- | The event.
    --
    -- The following are the @error@ events:
    --
    --     * @iamFleetRoleInvalid@ - The EC2 Fleet or Spot Fleet did not have the required permissions either to launch or terminate an instance.
    --
    --
    --     * @spotFleetRequestConfigurationInvalid@ - The configuration is not valid. For more information, see the description of the event.
    --
    --
    --     * @spotInstanceCountLimitExceeded@ - You've reached the limit on the number of Spot Instances that you can launch.
    --
    --
    -- The following are the @fleetRequestChange@ events:
    --
    --     * @active@ - The EC2 Fleet or Spot Fleet request has been validated and Amazon EC2 is attempting to maintain the target number of running Spot Instances.
    --
    --
    --     * @cancelled@ - The EC2 Fleet or Spot Fleet request is canceled and has no running Spot Instances. The EC2 Fleet or Spot Fleet will be deleted two days after its instances were terminated.
    --
    --
    --     * @cancelled_running@ - The EC2 Fleet or Spot Fleet request is canceled and does not launch additional Spot Instances. Existing Spot Instances continue to run until they are interrupted or terminated.
    --
    --
    --     * @cancelled_terminating@ - The EC2 Fleet or Spot Fleet request is canceled and its Spot Instances are terminating.
    --
    --
    --     * @expired@ - The EC2 Fleet or Spot Fleet request has expired. A subsequent event indicates that the instances were terminated, if the request was created with @TerminateInstancesWithExpiration@ set.
    --
    --
    --     * @modify_in_progress@ - A request to modify the EC2 Fleet or Spot Fleet request was accepted and is in progress.
    --
    --
    --     * @modify_successful@ - The EC2 Fleet or Spot Fleet request was modified.
    --
    --
    --     * @price_update@ - The price for a launch configuration was adjusted because it was too high. This change is permanent.
    --
    --
    --     * @submitted@ - The EC2 Fleet or Spot Fleet request is being evaluated and Amazon EC2 is preparing to launch the target number of Spot Instances.
    --
    --
    -- The following are the @instanceChange@ events:
    --
    --     * @launched@ - A request was fulfilled and a new instance was launched.
    --
    --
    --     * @terminated@ - An instance was terminated by the user.
    --
    --
    -- The following are the @Information@ events:
    --
    --     * @launchSpecTemporarilyBlacklisted@ - The configuration is not valid and several attempts to launch instances have failed. For more information, see the description of the event.
    --
    --
    --     * @launchSpecUnusable@ - The price in a launch specification is not valid because it is below the Spot price or the Spot price is above the On-Demand price.
    --
    --
    --     * @fleetProgressHalted@ - The price in every launch specification is not valid. A launch specification might become valid if the Spot price changes.
    eventSubType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventInformation' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance. This information is available only for @instanceChange@ events.
-- * 'eventDescription' - The description of the event.
-- * 'eventSubType' - The event.
--
-- The following are the @error@ events:
--
--     * @iamFleetRoleInvalid@ - The EC2 Fleet or Spot Fleet did not have the required permissions either to launch or terminate an instance.
--
--
--     * @spotFleetRequestConfigurationInvalid@ - The configuration is not valid. For more information, see the description of the event.
--
--
--     * @spotInstanceCountLimitExceeded@ - You've reached the limit on the number of Spot Instances that you can launch.
--
--
-- The following are the @fleetRequestChange@ events:
--
--     * @active@ - The EC2 Fleet or Spot Fleet request has been validated and Amazon EC2 is attempting to maintain the target number of running Spot Instances.
--
--
--     * @cancelled@ - The EC2 Fleet or Spot Fleet request is canceled and has no running Spot Instances. The EC2 Fleet or Spot Fleet will be deleted two days after its instances were terminated.
--
--
--     * @cancelled_running@ - The EC2 Fleet or Spot Fleet request is canceled and does not launch additional Spot Instances. Existing Spot Instances continue to run until they are interrupted or terminated.
--
--
--     * @cancelled_terminating@ - The EC2 Fleet or Spot Fleet request is canceled and its Spot Instances are terminating.
--
--
--     * @expired@ - The EC2 Fleet or Spot Fleet request has expired. A subsequent event indicates that the instances were terminated, if the request was created with @TerminateInstancesWithExpiration@ set.
--
--
--     * @modify_in_progress@ - A request to modify the EC2 Fleet or Spot Fleet request was accepted and is in progress.
--
--
--     * @modify_successful@ - The EC2 Fleet or Spot Fleet request was modified.
--
--
--     * @price_update@ - The price for a launch configuration was adjusted because it was too high. This change is permanent.
--
--
--     * @submitted@ - The EC2 Fleet or Spot Fleet request is being evaluated and Amazon EC2 is preparing to launch the target number of Spot Instances.
--
--
-- The following are the @instanceChange@ events:
--
--     * @launched@ - A request was fulfilled and a new instance was launched.
--
--
--     * @terminated@ - An instance was terminated by the user.
--
--
-- The following are the @Information@ events:
--
--     * @launchSpecTemporarilyBlacklisted@ - The configuration is not valid and several attempts to launch instances have failed. For more information, see the description of the event.
--
--
--     * @launchSpecUnusable@ - The price in a launch specification is not valid because it is below the Spot price or the Spot price is above the On-Demand price.
--
--
--     * @fleetProgressHalted@ - The price in every launch specification is not valid. A launch specification might become valid if the Spot price changes.
mkEventInformation ::
  EventInformation
mkEventInformation =
  EventInformation'
    { instanceId = Lude.Nothing,
      eventDescription = Lude.Nothing,
      eventSubType = Lude.Nothing
    }

-- | The ID of the instance. This information is available only for @instanceChange@ events.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiInstanceId :: Lens.Lens' EventInformation (Lude.Maybe Lude.Text)
eiInstanceId = Lens.lens (instanceId :: EventInformation -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: EventInformation)
{-# DEPRECATED eiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The description of the event.
--
-- /Note:/ Consider using 'eventDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiEventDescription :: Lens.Lens' EventInformation (Lude.Maybe Lude.Text)
eiEventDescription = Lens.lens (eventDescription :: EventInformation -> Lude.Maybe Lude.Text) (\s a -> s {eventDescription = a} :: EventInformation)
{-# DEPRECATED eiEventDescription "Use generic-lens or generic-optics with 'eventDescription' instead." #-}

-- | The event.
--
-- The following are the @error@ events:
--
--     * @iamFleetRoleInvalid@ - The EC2 Fleet or Spot Fleet did not have the required permissions either to launch or terminate an instance.
--
--
--     * @spotFleetRequestConfigurationInvalid@ - The configuration is not valid. For more information, see the description of the event.
--
--
--     * @spotInstanceCountLimitExceeded@ - You've reached the limit on the number of Spot Instances that you can launch.
--
--
-- The following are the @fleetRequestChange@ events:
--
--     * @active@ - The EC2 Fleet or Spot Fleet request has been validated and Amazon EC2 is attempting to maintain the target number of running Spot Instances.
--
--
--     * @cancelled@ - The EC2 Fleet or Spot Fleet request is canceled and has no running Spot Instances. The EC2 Fleet or Spot Fleet will be deleted two days after its instances were terminated.
--
--
--     * @cancelled_running@ - The EC2 Fleet or Spot Fleet request is canceled and does not launch additional Spot Instances. Existing Spot Instances continue to run until they are interrupted or terminated.
--
--
--     * @cancelled_terminating@ - The EC2 Fleet or Spot Fleet request is canceled and its Spot Instances are terminating.
--
--
--     * @expired@ - The EC2 Fleet or Spot Fleet request has expired. A subsequent event indicates that the instances were terminated, if the request was created with @TerminateInstancesWithExpiration@ set.
--
--
--     * @modify_in_progress@ - A request to modify the EC2 Fleet or Spot Fleet request was accepted and is in progress.
--
--
--     * @modify_successful@ - The EC2 Fleet or Spot Fleet request was modified.
--
--
--     * @price_update@ - The price for a launch configuration was adjusted because it was too high. This change is permanent.
--
--
--     * @submitted@ - The EC2 Fleet or Spot Fleet request is being evaluated and Amazon EC2 is preparing to launch the target number of Spot Instances.
--
--
-- The following are the @instanceChange@ events:
--
--     * @launched@ - A request was fulfilled and a new instance was launched.
--
--
--     * @terminated@ - An instance was terminated by the user.
--
--
-- The following are the @Information@ events:
--
--     * @launchSpecTemporarilyBlacklisted@ - The configuration is not valid and several attempts to launch instances have failed. For more information, see the description of the event.
--
--
--     * @launchSpecUnusable@ - The price in a launch specification is not valid because it is below the Spot price or the Spot price is above the On-Demand price.
--
--
--     * @fleetProgressHalted@ - The price in every launch specification is not valid. A launch specification might become valid if the Spot price changes.
--
--
--
-- /Note:/ Consider using 'eventSubType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiEventSubType :: Lens.Lens' EventInformation (Lude.Maybe Lude.Text)
eiEventSubType = Lens.lens (eventSubType :: EventInformation -> Lude.Maybe Lude.Text) (\s a -> s {eventSubType = a} :: EventInformation)
{-# DEPRECATED eiEventSubType "Use generic-lens or generic-optics with 'eventSubType' instead." #-}

instance Lude.FromXML EventInformation where
  parseXML x =
    EventInformation'
      Lude.<$> (x Lude..@? "instanceId")
      Lude.<*> (x Lude..@? "eventDescription")
      Lude.<*> (x Lude..@? "eventSubType")
