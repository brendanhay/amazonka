{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    rDescription,
    rProfileArn,
    rProviderCalendarId,
    rRoomArn,
    rRoomName,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.Description as Types
import qualified Network.AWS.AlexaBusiness.Types.ProfileArn as Types
import qualified Network.AWS.AlexaBusiness.Types.ProviderCalendarId as Types
import qualified Network.AWS.AlexaBusiness.Types.RoomArn as Types
import qualified Network.AWS.AlexaBusiness.Types.RoomName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A room with attributes.
--
-- /See:/ 'mkRoom' smart constructor.
data Room = Room'
  { -- | The description of a room.
    description :: Core.Maybe Types.Description,
    -- | The profile ARN of a room.
    profileArn :: Core.Maybe Types.ProfileArn,
    -- | The provider calendar ARN of a room.
    providerCalendarId :: Core.Maybe Types.ProviderCalendarId,
    -- | The ARN of a room.
    roomArn :: Core.Maybe Types.RoomArn,
    -- | The name of a room.
    roomName :: Core.Maybe Types.RoomName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Room' value with any optional fields omitted.
mkRoom ::
  Room
mkRoom =
  Room'
    { description = Core.Nothing,
      profileArn = Core.Nothing,
      providerCalendarId = Core.Nothing,
      roomArn = Core.Nothing,
      roomName = Core.Nothing
    }

-- | The description of a room.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDescription :: Lens.Lens' Room (Core.Maybe Types.Description)
rDescription = Lens.field @"description"
{-# DEPRECATED rDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The profile ARN of a room.
--
-- /Note:/ Consider using 'profileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rProfileArn :: Lens.Lens' Room (Core.Maybe Types.ProfileArn)
rProfileArn = Lens.field @"profileArn"
{-# DEPRECATED rProfileArn "Use generic-lens or generic-optics with 'profileArn' instead." #-}

-- | The provider calendar ARN of a room.
--
-- /Note:/ Consider using 'providerCalendarId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rProviderCalendarId :: Lens.Lens' Room (Core.Maybe Types.ProviderCalendarId)
rProviderCalendarId = Lens.field @"providerCalendarId"
{-# DEPRECATED rProviderCalendarId "Use generic-lens or generic-optics with 'providerCalendarId' instead." #-}

-- | The ARN of a room.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRoomArn :: Lens.Lens' Room (Core.Maybe Types.RoomArn)
rRoomArn = Lens.field @"roomArn"
{-# DEPRECATED rRoomArn "Use generic-lens or generic-optics with 'roomArn' instead." #-}

-- | The name of a room.
--
-- /Note:/ Consider using 'roomName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRoomName :: Lens.Lens' Room (Core.Maybe Types.RoomName)
rRoomName = Lens.field @"roomName"
{-# DEPRECATED rRoomName "Use generic-lens or generic-optics with 'roomName' instead." #-}

instance Core.FromJSON Room where
  parseJSON =
    Core.withObject "Room" Core.$
      \x ->
        Room'
          Core.<$> (x Core..:? "Description")
          Core.<*> (x Core..:? "ProfileArn")
          Core.<*> (x Core..:? "ProviderCalendarId")
          Core.<*> (x Core..:? "RoomArn")
          Core.<*> (x Core..:? "RoomName")
