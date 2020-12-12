{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.UpdateMeetingRoomConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.UpdateMeetingRoomConfiguration
  ( UpdateMeetingRoomConfiguration (..),

    -- * Smart constructor
    mkUpdateMeetingRoomConfiguration,

    -- * Lenses
    umrcInstantBooking,
    umrcEndOfMeetingReminder,
    umrcRequireCheckIn,
    umrcRoomUtilizationMetricsEnabled,
  )
where

import Network.AWS.AlexaBusiness.Types.UpdateEndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.UpdateInstantBooking
import Network.AWS.AlexaBusiness.Types.UpdateRequireCheckIn
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Updates meeting room settings of a room profile.
--
-- /See:/ 'mkUpdateMeetingRoomConfiguration' smart constructor.
data UpdateMeetingRoomConfiguration = UpdateMeetingRoomConfiguration'
  { instantBooking ::
      Lude.Maybe
        UpdateInstantBooking,
    endOfMeetingReminder ::
      Lude.Maybe
        UpdateEndOfMeetingReminder,
    requireCheckIn ::
      Lude.Maybe
        UpdateRequireCheckIn,
    roomUtilizationMetricsEnabled ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMeetingRoomConfiguration' with the minimum fields required to make a request.
--
-- * 'endOfMeetingReminder' - Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
-- * 'instantBooking' - Settings to automatically book an available room available for a configured duration when joining a meeting with Alexa.
-- * 'requireCheckIn' - Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into to make the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.”
-- * 'roomUtilizationMetricsEnabled' - Whether room utilization metrics are enabled or not.
mkUpdateMeetingRoomConfiguration ::
  UpdateMeetingRoomConfiguration
mkUpdateMeetingRoomConfiguration =
  UpdateMeetingRoomConfiguration'
    { instantBooking = Lude.Nothing,
      endOfMeetingReminder = Lude.Nothing,
      requireCheckIn = Lude.Nothing,
      roomUtilizationMetricsEnabled = Lude.Nothing
    }

-- | Settings to automatically book an available room available for a configured duration when joining a meeting with Alexa.
--
-- /Note:/ Consider using 'instantBooking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrcInstantBooking :: Lens.Lens' UpdateMeetingRoomConfiguration (Lude.Maybe UpdateInstantBooking)
umrcInstantBooking = Lens.lens (instantBooking :: UpdateMeetingRoomConfiguration -> Lude.Maybe UpdateInstantBooking) (\s a -> s {instantBooking = a} :: UpdateMeetingRoomConfiguration)
{-# DEPRECATED umrcInstantBooking "Use generic-lens or generic-optics with 'instantBooking' instead." #-}

-- | Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
--
-- /Note:/ Consider using 'endOfMeetingReminder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrcEndOfMeetingReminder :: Lens.Lens' UpdateMeetingRoomConfiguration (Lude.Maybe UpdateEndOfMeetingReminder)
umrcEndOfMeetingReminder = Lens.lens (endOfMeetingReminder :: UpdateMeetingRoomConfiguration -> Lude.Maybe UpdateEndOfMeetingReminder) (\s a -> s {endOfMeetingReminder = a} :: UpdateMeetingRoomConfiguration)
{-# DEPRECATED umrcEndOfMeetingReminder "Use generic-lens or generic-optics with 'endOfMeetingReminder' instead." #-}

-- | Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into to make the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.”
--
-- /Note:/ Consider using 'requireCheckIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrcRequireCheckIn :: Lens.Lens' UpdateMeetingRoomConfiguration (Lude.Maybe UpdateRequireCheckIn)
umrcRequireCheckIn = Lens.lens (requireCheckIn :: UpdateMeetingRoomConfiguration -> Lude.Maybe UpdateRequireCheckIn) (\s a -> s {requireCheckIn = a} :: UpdateMeetingRoomConfiguration)
{-# DEPRECATED umrcRequireCheckIn "Use generic-lens or generic-optics with 'requireCheckIn' instead." #-}

-- | Whether room utilization metrics are enabled or not.
--
-- /Note:/ Consider using 'roomUtilizationMetricsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrcRoomUtilizationMetricsEnabled :: Lens.Lens' UpdateMeetingRoomConfiguration (Lude.Maybe Lude.Bool)
umrcRoomUtilizationMetricsEnabled = Lens.lens (roomUtilizationMetricsEnabled :: UpdateMeetingRoomConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {roomUtilizationMetricsEnabled = a} :: UpdateMeetingRoomConfiguration)
{-# DEPRECATED umrcRoomUtilizationMetricsEnabled "Use generic-lens or generic-optics with 'roomUtilizationMetricsEnabled' instead." #-}

instance Lude.ToJSON UpdateMeetingRoomConfiguration where
  toJSON UpdateMeetingRoomConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstantBooking" Lude..=) Lude.<$> instantBooking,
            ("EndOfMeetingReminder" Lude..=) Lude.<$> endOfMeetingReminder,
            ("RequireCheckIn" Lude..=) Lude.<$> requireCheckIn,
            ("RoomUtilizationMetricsEnabled" Lude..=)
              Lude.<$> roomUtilizationMetricsEnabled
          ]
      )
