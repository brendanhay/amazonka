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
    rdDescription,
    rdProfileArn,
    rdProfileName,
    rdProviderCalendarId,
    rdRoomArn,
    rdRoomName,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.Description as Types
import qualified Network.AWS.AlexaBusiness.Types.ProfileArn as Types
import qualified Network.AWS.AlexaBusiness.Types.ProfileName as Types
import qualified Network.AWS.AlexaBusiness.Types.ProviderCalendarId as Types
import qualified Network.AWS.AlexaBusiness.Types.RoomArn as Types
import qualified Network.AWS.AlexaBusiness.Types.RoomName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The data of a room.
--
-- /See:/ 'mkRoomData' smart constructor.
data RoomData = RoomData'
  { -- | The description of a room.
    description :: Core.Maybe Types.Description,
    -- | The profile ARN of a room.
    profileArn :: Core.Maybe Types.ProfileArn,
    -- | The profile name of a room.
    profileName :: Core.Maybe Types.ProfileName,
    -- | The provider calendar ARN of a room.
    providerCalendarId :: Core.Maybe Types.ProviderCalendarId,
    -- | The ARN of a room.
    roomArn :: Core.Maybe Types.RoomArn,
    -- | The name of a room.
    roomName :: Core.Maybe Types.RoomName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RoomData' value with any optional fields omitted.
mkRoomData ::
  RoomData
mkRoomData =
  RoomData'
    { description = Core.Nothing,
      profileArn = Core.Nothing,
      profileName = Core.Nothing,
      providerCalendarId = Core.Nothing,
      roomArn = Core.Nothing,
      roomName = Core.Nothing
    }

-- | The description of a room.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDescription :: Lens.Lens' RoomData (Core.Maybe Types.Description)
rdDescription = Lens.field @"description"
{-# DEPRECATED rdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The profile ARN of a room.
--
-- /Note:/ Consider using 'profileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProfileArn :: Lens.Lens' RoomData (Core.Maybe Types.ProfileArn)
rdProfileArn = Lens.field @"profileArn"
{-# DEPRECATED rdProfileArn "Use generic-lens or generic-optics with 'profileArn' instead." #-}

-- | The profile name of a room.
--
-- /Note:/ Consider using 'profileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProfileName :: Lens.Lens' RoomData (Core.Maybe Types.ProfileName)
rdProfileName = Lens.field @"profileName"
{-# DEPRECATED rdProfileName "Use generic-lens or generic-optics with 'profileName' instead." #-}

-- | The provider calendar ARN of a room.
--
-- /Note:/ Consider using 'providerCalendarId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdProviderCalendarId :: Lens.Lens' RoomData (Core.Maybe Types.ProviderCalendarId)
rdProviderCalendarId = Lens.field @"providerCalendarId"
{-# DEPRECATED rdProviderCalendarId "Use generic-lens or generic-optics with 'providerCalendarId' instead." #-}

-- | The ARN of a room.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRoomArn :: Lens.Lens' RoomData (Core.Maybe Types.RoomArn)
rdRoomArn = Lens.field @"roomArn"
{-# DEPRECATED rdRoomArn "Use generic-lens or generic-optics with 'roomArn' instead." #-}

-- | The name of a room.
--
-- /Note:/ Consider using 'roomName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRoomName :: Lens.Lens' RoomData (Core.Maybe Types.RoomName)
rdRoomName = Lens.field @"roomName"
{-# DEPRECATED rdRoomName "Use generic-lens or generic-optics with 'roomName' instead." #-}

instance Core.FromJSON RoomData where
  parseJSON =
    Core.withObject "RoomData" Core.$
      \x ->
        RoomData'
          Core.<$> (x Core..:? "Description")
          Core.<*> (x Core..:? "ProfileArn")
          Core.<*> (x Core..:? "ProfileName")
          Core.<*> (x Core..:? "ProviderCalendarId")
          Core.<*> (x Core..:? "RoomArn")
          Core.<*> (x Core..:? "RoomName")
