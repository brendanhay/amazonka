{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.MeetingSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.MeetingSetting
  ( MeetingSetting (..),

    -- * Smart constructor
    mkMeetingSetting,

    -- * Lenses
    msRequirePin,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.RequirePin as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The values that indicate whether a pin is always required (YES), never required (NO), or OPTIONAL.
--
--
--     * If YES, Alexa will always ask for a meeting pin.
--
--
--     * If NO, Alexa will never ask for a meeting pin.
--
--
--     * If OPTIONAL, Alexa will ask if you have a meeting pin and if the customer responds with yes, it will ask for the meeting pin.
--
--
--
-- /See:/ 'mkMeetingSetting' smart constructor.
newtype MeetingSetting = MeetingSetting'
  { -- | The values that indicate whether the pin is always required.
    requirePin :: Types.RequirePin
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MeetingSetting' value with any optional fields omitted.
mkMeetingSetting ::
  -- | 'requirePin'
  Types.RequirePin ->
  MeetingSetting
mkMeetingSetting requirePin = MeetingSetting' {requirePin}

-- | The values that indicate whether the pin is always required.
--
-- /Note:/ Consider using 'requirePin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msRequirePin :: Lens.Lens' MeetingSetting Types.RequirePin
msRequirePin = Lens.field @"requirePin"
{-# DEPRECATED msRequirePin "Use generic-lens or generic-optics with 'requirePin' instead." #-}

instance Core.FromJSON MeetingSetting where
  toJSON MeetingSetting {..} =
    Core.object
      (Core.catMaybes [Core.Just ("RequirePin" Core..= requirePin)])

instance Core.FromJSON MeetingSetting where
  parseJSON =
    Core.withObject "MeetingSetting" Core.$
      \x -> MeetingSetting' Core.<$> (x Core..: "RequirePin")
