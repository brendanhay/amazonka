-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration
  ( MeetingRoomConfiguration (..),

    -- * Smart constructor
    mkMeetingRoomConfiguration,

    -- * Lenses
    mrcInstantBooking,
    mrcEndOfMeetingReminder,
    mrcRequireCheckIn,
    mrcRoomUtilizationMetricsEnabled,
  )
where

import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.InstantBooking
import Network.AWS.AlexaBusiness.Types.RequireCheckIn
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Meeting room settings of a room profile.
--
-- /See:/ 'mkMeetingRoomConfiguration' smart constructor.
data MeetingRoomConfiguration = MeetingRoomConfiguration'
  { instantBooking ::
      Lude.Maybe InstantBooking,
    endOfMeetingReminder ::
      Lude.Maybe EndOfMeetingReminder,
    requireCheckIn ::
      Lude.Maybe RequireCheckIn,
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

-- | Creates a value of 'MeetingRoomConfiguration' with the minimum fields required to make a request.
--
-- * 'endOfMeetingReminder' - Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
-- * 'instantBooking' - Settings to automatically book the room if available for a configured duration when joining a meeting with Alexa.
-- * 'requireCheckIn' - Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into. This makes the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.”
-- * 'roomUtilizationMetricsEnabled' - Whether room utilization metrics are enabled or not.
mkMeetingRoomConfiguration ::
  MeetingRoomConfiguration
mkMeetingRoomConfiguration =
  MeetingRoomConfiguration'
    { instantBooking = Lude.Nothing,
      endOfMeetingReminder = Lude.Nothing,
      requireCheckIn = Lude.Nothing,
      roomUtilizationMetricsEnabled = Lude.Nothing
    }

-- | Settings to automatically book the room if available for a configured duration when joining a meeting with Alexa.
--
-- /Note:/ Consider using 'instantBooking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrcInstantBooking :: Lens.Lens' MeetingRoomConfiguration (Lude.Maybe InstantBooking)
mrcInstantBooking = Lens.lens (instantBooking :: MeetingRoomConfiguration -> Lude.Maybe InstantBooking) (\s a -> s {instantBooking = a} :: MeetingRoomConfiguration)
{-# DEPRECATED mrcInstantBooking "Use generic-lens or generic-optics with 'instantBooking' instead." #-}

-- | Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
--
-- /Note:/ Consider using 'endOfMeetingReminder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrcEndOfMeetingReminder :: Lens.Lens' MeetingRoomConfiguration (Lude.Maybe EndOfMeetingReminder)
mrcEndOfMeetingReminder = Lens.lens (endOfMeetingReminder :: MeetingRoomConfiguration -> Lude.Maybe EndOfMeetingReminder) (\s a -> s {endOfMeetingReminder = a} :: MeetingRoomConfiguration)
{-# DEPRECATED mrcEndOfMeetingReminder "Use generic-lens or generic-optics with 'endOfMeetingReminder' instead." #-}

-- | Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into. This makes the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.”
--
-- /Note:/ Consider using 'requireCheckIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrcRequireCheckIn :: Lens.Lens' MeetingRoomConfiguration (Lude.Maybe RequireCheckIn)
mrcRequireCheckIn = Lens.lens (requireCheckIn :: MeetingRoomConfiguration -> Lude.Maybe RequireCheckIn) (\s a -> s {requireCheckIn = a} :: MeetingRoomConfiguration)
{-# DEPRECATED mrcRequireCheckIn "Use generic-lens or generic-optics with 'requireCheckIn' instead." #-}

-- | Whether room utilization metrics are enabled or not.
--
-- /Note:/ Consider using 'roomUtilizationMetricsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrcRoomUtilizationMetricsEnabled :: Lens.Lens' MeetingRoomConfiguration (Lude.Maybe Lude.Bool)
mrcRoomUtilizationMetricsEnabled = Lens.lens (roomUtilizationMetricsEnabled :: MeetingRoomConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {roomUtilizationMetricsEnabled = a} :: MeetingRoomConfiguration)
{-# DEPRECATED mrcRoomUtilizationMetricsEnabled "Use generic-lens or generic-optics with 'roomUtilizationMetricsEnabled' instead." #-}

instance Lude.FromJSON MeetingRoomConfiguration where
  parseJSON =
    Lude.withObject
      "MeetingRoomConfiguration"
      ( \x ->
          MeetingRoomConfiguration'
            Lude.<$> (x Lude..:? "InstantBooking")
            Lude.<*> (x Lude..:? "EndOfMeetingReminder")
            Lude.<*> (x Lude..:? "RequireCheckIn")
            Lude.<*> (x Lude..:? "RoomUtilizationMetricsEnabled")
      )
