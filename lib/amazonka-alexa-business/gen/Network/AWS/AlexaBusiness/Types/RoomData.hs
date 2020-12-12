{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.RoomData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.RoomData
  ( RoomData (..),

    -- * Smart constructor
    mkRoomData,

    -- * Lenses
    rdProfileARN,
    rdProviderCalendarId,
    rdProfileName,
    rdRoomARN,
    rdRoomName,
    rdDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The data of a room.
--
-- /See:/ 'mkRoomData' smart constructor.
data RoomData = RoomData'
  { profileARN :: Lude.Maybe Lude.Text,
    providerCalendarId :: Lude.Maybe Lude.Text,
    profileName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'RoomData' with the minimum fields required to make a request.
--
-- * 'description' - The description of a room.
-- * 'profileARN' - The profile ARN of a room.
-- * 'profileName' - The profile name of a room.
-- * 'providerCalendarId' - The provider calendar ARN of a room.
-- * 'roomARN' - The ARN of a room.
-- * 'roomName' - The name of a room.
mkRoomData ::
  RoomData
mkRoomData =
  RoomData'
    { profileARN = Lude.Nothing,
      providerCalendarId = Lude.Nothing,
      profileName = Lude.Nothing,
      roomARN = Lude.Nothing,
      roomName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The profile ARN of a room.
--
-- /Note:/ Consider using 'profileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProfileARN :: Lens.Lens' RoomData (Lude.Maybe Lude.Text)
rdProfileARN = Lens.lens (profileARN :: RoomData -> Lude.Maybe Lude.Text) (\s a -> s {profileARN = a} :: RoomData)
{-# DEPRECATED rdProfileARN "Use generic-lens or generic-optics with 'profileARN' instead." #-}

-- | The provider calendar ARN of a room.
--
-- /Note:/ Consider using 'providerCalendarId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProviderCalendarId :: Lens.Lens' RoomData (Lude.Maybe Lude.Text)
rdProviderCalendarId = Lens.lens (providerCalendarId :: RoomData -> Lude.Maybe Lude.Text) (\s a -> s {providerCalendarId = a} :: RoomData)
{-# DEPRECATED rdProviderCalendarId "Use generic-lens or generic-optics with 'providerCalendarId' instead." #-}

-- | The profile name of a room.
--
-- /Note:/ Consider using 'profileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProfileName :: Lens.Lens' RoomData (Lude.Maybe Lude.Text)
rdProfileName = Lens.lens (profileName :: RoomData -> Lude.Maybe Lude.Text) (\s a -> s {profileName = a} :: RoomData)
{-# DEPRECATED rdProfileName "Use generic-lens or generic-optics with 'profileName' instead." #-}

-- | The ARN of a room.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRoomARN :: Lens.Lens' RoomData (Lude.Maybe Lude.Text)
rdRoomARN = Lens.lens (roomARN :: RoomData -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: RoomData)
{-# DEPRECATED rdRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

-- | The name of a room.
--
-- /Note:/ Consider using 'roomName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRoomName :: Lens.Lens' RoomData (Lude.Maybe Lude.Text)
rdRoomName = Lens.lens (roomName :: RoomData -> Lude.Maybe Lude.Text) (\s a -> s {roomName = a} :: RoomData)
{-# DEPRECATED rdRoomName "Use generic-lens or generic-optics with 'roomName' instead." #-}

-- | The description of a room.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDescription :: Lens.Lens' RoomData (Lude.Maybe Lude.Text)
rdDescription = Lens.lens (description :: RoomData -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RoomData)
{-# DEPRECATED rdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON RoomData where
  parseJSON =
    Lude.withObject
      "RoomData"
      ( \x ->
          RoomData'
            Lude.<$> (x Lude..:? "ProfileArn")
            Lude.<*> (x Lude..:? "ProviderCalendarId")
            Lude.<*> (x Lude..:? "ProfileName")
            Lude.<*> (x Lude..:? "RoomArn")
            Lude.<*> (x Lude..:? "RoomName")
            Lude.<*> (x Lude..:? "Description")
      )
