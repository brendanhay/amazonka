{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.UpdateRequireCheckIn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.UpdateRequireCheckIn
  ( UpdateRequireCheckIn (..)
  -- * Smart constructor
  , mkUpdateRequireCheckIn
  -- * Lenses
  , urciEnabled
  , urciReleaseAfterMinutes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Updates settings for the require check in feature that are applied to a room profile. Require check in allows a meeting room’s Alexa or AVS device to prompt the user to check in; otherwise, the room will be released. 
--
-- /See:/ 'mkUpdateRequireCheckIn' smart constructor.
data UpdateRequireCheckIn = UpdateRequireCheckIn'
  { enabled :: Core.Maybe Core.Bool
    -- ^ Whether require check in is enabled or not.
  , releaseAfterMinutes :: Core.Maybe Core.Int
    -- ^ Duration between 5 and 20 minutes to determine when to release the room if it's not checked into. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRequireCheckIn' value with any optional fields omitted.
mkUpdateRequireCheckIn
    :: UpdateRequireCheckIn
mkUpdateRequireCheckIn
  = UpdateRequireCheckIn'{enabled = Core.Nothing,
                          releaseAfterMinutes = Core.Nothing}

-- | Whether require check in is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urciEnabled :: Lens.Lens' UpdateRequireCheckIn (Core.Maybe Core.Bool)
urciEnabled = Lens.field @"enabled"
{-# INLINEABLE urciEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | Duration between 5 and 20 minutes to determine when to release the room if it's not checked into. 
--
-- /Note:/ Consider using 'releaseAfterMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urciReleaseAfterMinutes :: Lens.Lens' UpdateRequireCheckIn (Core.Maybe Core.Int)
urciReleaseAfterMinutes = Lens.field @"releaseAfterMinutes"
{-# INLINEABLE urciReleaseAfterMinutes #-}
{-# DEPRECATED releaseAfterMinutes "Use generic-lens or generic-optics with 'releaseAfterMinutes' instead"  #-}

instance Core.FromJSON UpdateRequireCheckIn where
        toJSON UpdateRequireCheckIn{..}
          = Core.object
              (Core.catMaybes
                 [("Enabled" Core..=) Core.<$> enabled,
                  ("ReleaseAfterMinutes" Core..=) Core.<$> releaseAfterMinutes])
