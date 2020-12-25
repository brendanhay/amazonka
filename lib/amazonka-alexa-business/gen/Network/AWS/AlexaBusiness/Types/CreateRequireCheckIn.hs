{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn
  ( CreateRequireCheckIn (..),

    -- * Smart constructor
    mkCreateRequireCheckIn,

    -- * Lenses
    crciReleaseAfterMinutes,
    crciEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Creates settings for the require check in feature that are applied to a room profile. Require check in allows a meeting room’s Alexa or AVS device to prompt the user to check in; otherwise, the room will be released.
--
-- /See:/ 'mkCreateRequireCheckIn' smart constructor.
data CreateRequireCheckIn = CreateRequireCheckIn'
  { -- | Duration between 5 and 20 minutes to determine when to release the room if it's not checked into.
    releaseAfterMinutes :: Core.Int,
    -- | Whether require check in is enabled or not.
    enabled :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRequireCheckIn' value with any optional fields omitted.
mkCreateRequireCheckIn ::
  -- | 'releaseAfterMinutes'
  Core.Int ->
  -- | 'enabled'
  Core.Bool ->
  CreateRequireCheckIn
mkCreateRequireCheckIn releaseAfterMinutes enabled =
  CreateRequireCheckIn' {releaseAfterMinutes, enabled}

-- | Duration between 5 and 20 minutes to determine when to release the room if it's not checked into.
--
-- /Note:/ Consider using 'releaseAfterMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crciReleaseAfterMinutes :: Lens.Lens' CreateRequireCheckIn Core.Int
crciReleaseAfterMinutes = Lens.field @"releaseAfterMinutes"
{-# DEPRECATED crciReleaseAfterMinutes "Use generic-lens or generic-optics with 'releaseAfterMinutes' instead." #-}

-- | Whether require check in is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crciEnabled :: Lens.Lens' CreateRequireCheckIn Core.Bool
crciEnabled = Lens.field @"enabled"
{-# DEPRECATED crciEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Core.FromJSON CreateRequireCheckIn where
  toJSON CreateRequireCheckIn {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ReleaseAfterMinutes" Core..= releaseAfterMinutes),
            Core.Just ("Enabled" Core..= enabled)
          ]
      )
