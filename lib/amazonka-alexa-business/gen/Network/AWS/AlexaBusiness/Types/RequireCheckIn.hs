{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.RequireCheckIn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.RequireCheckIn
  ( RequireCheckIn (..),

    -- * Smart constructor
    mkRequireCheckIn,

    -- * Lenses
    rciEnabled,
    rciReleaseAfterMinutes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for the require check in feature that are applied to a room profile. Require check in allows a meeting room’s Alexa or AVS device to prompt the user to check in; otherwise, the room will be released.
--
-- /See:/ 'mkRequireCheckIn' smart constructor.
data RequireCheckIn = RequireCheckIn'
  { -- | Whether require check in is enabled or not.
    enabled :: Core.Maybe Core.Bool,
    -- | Duration between 5 and 20 minutes to determine when to release the room if it's not checked into.
    releaseAfterMinutes :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RequireCheckIn' value with any optional fields omitted.
mkRequireCheckIn ::
  RequireCheckIn
mkRequireCheckIn =
  RequireCheckIn'
    { enabled = Core.Nothing,
      releaseAfterMinutes = Core.Nothing
    }

-- | Whether require check in is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciEnabled :: Lens.Lens' RequireCheckIn (Core.Maybe Core.Bool)
rciEnabled = Lens.field @"enabled"
{-# DEPRECATED rciEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Duration between 5 and 20 minutes to determine when to release the room if it's not checked into.
--
-- /Note:/ Consider using 'releaseAfterMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciReleaseAfterMinutes :: Lens.Lens' RequireCheckIn (Core.Maybe Core.Int)
rciReleaseAfterMinutes = Lens.field @"releaseAfterMinutes"
{-# DEPRECATED rciReleaseAfterMinutes "Use generic-lens or generic-optics with 'releaseAfterMinutes' instead." #-}

instance Core.FromJSON RequireCheckIn where
  parseJSON =
    Core.withObject "RequireCheckIn" Core.$
      \x ->
        RequireCheckIn'
          Core.<$> (x Core..:? "Enabled") Core.<*> (x Core..:? "ReleaseAfterMinutes")
