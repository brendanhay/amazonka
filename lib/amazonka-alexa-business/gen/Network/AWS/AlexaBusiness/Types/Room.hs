-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Room
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Room
  ( Room (..),

    -- * Smart constructor
    mkRoom,

    -- * Lenses
    rProfileARN,
    rProviderCalendarId,
    rRoomARN,
    rRoomName,
    rDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A room with attributes.
--
-- /See:/ 'mkRoom' smart constructor.
data Room = Room'
  { profileARN :: Lude.Maybe Lude.Text,
    providerCalendarId :: Lude.Maybe Lude.Text,
    roomARN :: Lude.Maybe Lude.Text,
    roomName :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Room' with the minimum fields required to make a request.
--
-- * 'description' - The description of a room.
-- * 'profileARN' - The profile ARN of a room.
-- * 'providerCalendarId' - The provider calendar ARN of a room.
-- * 'roomARN' - The ARN of a room.
-- * 'roomName' - The name of a room.
mkRoom ::
  Room
mkRoom =
  Room'
    { profileARN = Lude.Nothing,
      providerCalendarId = Lude.Nothing,
      roomARN = Lude.Nothing,
      roomName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The profile ARN of a room.
--
-- /Note:/ Consider using 'profileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rProfileARN :: Lens.Lens' Room (Lude.Maybe Lude.Text)
rProfileARN = Lens.lens (profileARN :: Room -> Lude.Maybe Lude.Text) (\s a -> s {profileARN = a} :: Room)
{-# DEPRECATED rProfileARN "Use generic-lens or generic-optics with 'profileARN' instead." #-}

-- | The provider calendar ARN of a room.
--
-- /Note:/ Consider using 'providerCalendarId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rProviderCalendarId :: Lens.Lens' Room (Lude.Maybe Lude.Text)
rProviderCalendarId = Lens.lens (providerCalendarId :: Room -> Lude.Maybe Lude.Text) (\s a -> s {providerCalendarId = a} :: Room)
{-# DEPRECATED rProviderCalendarId "Use generic-lens or generic-optics with 'providerCalendarId' instead." #-}

-- | The ARN of a room.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRoomARN :: Lens.Lens' Room (Lude.Maybe Lude.Text)
rRoomARN = Lens.lens (roomARN :: Room -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: Room)
{-# DEPRECATED rRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

-- | The name of a room.
--
-- /Note:/ Consider using 'roomName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRoomName :: Lens.Lens' Room (Lude.Maybe Lude.Text)
rRoomName = Lens.lens (roomName :: Room -> Lude.Maybe Lude.Text) (\s a -> s {roomName = a} :: Room)
{-# DEPRECATED rRoomName "Use generic-lens or generic-optics with 'roomName' instead." #-}

-- | The description of a room.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDescription :: Lens.Lens' Room (Lude.Maybe Lude.Text)
rDescription = Lens.lens (description :: Room -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Room)
{-# DEPRECATED rDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON Room where
  parseJSON =
    Lude.withObject
      "Room"
      ( \x ->
          Room'
            Lude.<$> (x Lude..:? "ProfileArn")
            Lude.<*> (x Lude..:? "ProviderCalendarId")
            Lude.<*> (x Lude..:? "RoomArn")
            Lude.<*> (x Lude..:? "RoomName")
            Lude.<*> (x Lude..:? "Description")
      )
