{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.CreateMeetingRoomConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.CreateMeetingRoomConfiguration
  ( CreateMeetingRoomConfiguration (..),

    -- * Smart constructor
    mkCreateMeetingRoomConfiguration,

    -- * Lenses
    cmrcInstantBooking,
    cmrcEndOfMeetingReminder,
    cmrcRequireCheckIn,
    cmrcRoomUtilizationMetricsEnabled,
  )
where

import Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.CreateInstantBooking
import Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Creates meeting room settings of a room profile.
--
-- /See:/ 'mkCreateMeetingRoomConfiguration' smart constructor.
data CreateMeetingRoomConfiguration = CreateMeetingRoomConfiguration'
  { instantBooking ::
      Lude.Maybe
        CreateInstantBooking,
    endOfMeetingReminder ::
      Lude.Maybe
        CreateEndOfMeetingReminder,
    requireCheckIn ::
      Lude.Maybe
        CreateRequireCheckIn,
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

-- | Creates a value of 'CreateMeetingRoomConfiguration' with the minimum fields required to make a request.
--
-- * 'endOfMeetingReminder' - Undocumented field.
-- * 'instantBooking' - Settings to automatically book a room for a configured duration if it's free when joining a meeting with Alexa.
-- * 'requireCheckIn' - Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into to make the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.”
-- * 'roomUtilizationMetricsEnabled' - Whether room utilization metrics are enabled or not.
mkCreateMeetingRoomConfiguration ::
  CreateMeetingRoomConfiguration
mkCreateMeetingRoomConfiguration =
  CreateMeetingRoomConfiguration'
    { instantBooking = Lude.Nothing,
      endOfMeetingReminder = Lude.Nothing,
      requireCheckIn = Lude.Nothing,
      roomUtilizationMetricsEnabled = Lude.Nothing
    }

-- | Settings to automatically book a room for a configured duration if it's free when joining a meeting with Alexa.
--
-- /Note:/ Consider using 'instantBooking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrcInstantBooking :: Lens.Lens' CreateMeetingRoomConfiguration (Lude.Maybe CreateInstantBooking)
cmrcInstantBooking = Lens.lens (instantBooking :: CreateMeetingRoomConfiguration -> Lude.Maybe CreateInstantBooking) (\s a -> s {instantBooking = a} :: CreateMeetingRoomConfiguration)
{-# DEPRECATED cmrcInstantBooking "Use generic-lens or generic-optics with 'instantBooking' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endOfMeetingReminder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrcEndOfMeetingReminder :: Lens.Lens' CreateMeetingRoomConfiguration (Lude.Maybe CreateEndOfMeetingReminder)
cmrcEndOfMeetingReminder = Lens.lens (endOfMeetingReminder :: CreateMeetingRoomConfiguration -> Lude.Maybe CreateEndOfMeetingReminder) (\s a -> s {endOfMeetingReminder = a} :: CreateMeetingRoomConfiguration)
{-# DEPRECATED cmrcEndOfMeetingReminder "Use generic-lens or generic-optics with 'endOfMeetingReminder' instead." #-}

-- | Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into to make the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.”
--
-- /Note:/ Consider using 'requireCheckIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrcRequireCheckIn :: Lens.Lens' CreateMeetingRoomConfiguration (Lude.Maybe CreateRequireCheckIn)
cmrcRequireCheckIn = Lens.lens (requireCheckIn :: CreateMeetingRoomConfiguration -> Lude.Maybe CreateRequireCheckIn) (\s a -> s {requireCheckIn = a} :: CreateMeetingRoomConfiguration)
{-# DEPRECATED cmrcRequireCheckIn "Use generic-lens or generic-optics with 'requireCheckIn' instead." #-}

-- | Whether room utilization metrics are enabled or not.
--
-- /Note:/ Consider using 'roomUtilizationMetricsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrcRoomUtilizationMetricsEnabled :: Lens.Lens' CreateMeetingRoomConfiguration (Lude.Maybe Lude.Bool)
cmrcRoomUtilizationMetricsEnabled = Lens.lens (roomUtilizationMetricsEnabled :: CreateMeetingRoomConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {roomUtilizationMetricsEnabled = a} :: CreateMeetingRoomConfiguration)
{-# DEPRECATED cmrcRoomUtilizationMetricsEnabled "Use generic-lens or generic-optics with 'roomUtilizationMetricsEnabled' instead." #-}

instance Lude.ToJSON CreateMeetingRoomConfiguration where
  toJSON CreateMeetingRoomConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstantBooking" Lude..=) Lude.<$> instantBooking,
            ("EndOfMeetingReminder" Lude..=) Lude.<$> endOfMeetingReminder,
            ("RequireCheckIn" Lude..=) Lude.<$> requireCheckIn,
            ("RoomUtilizationMetricsEnabled" Lude..=)
              Lude.<$> roomUtilizationMetricsEnabled
          ]
      )
