{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AvailBlanking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AvailBlanking
  ( AvailBlanking (..),

    -- * Smart constructor
    mkAvailBlanking,

    -- * Lenses
    abAvailBlankingImage,
    abState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AvailBlankingState as Types
import qualified Network.AWS.MediaLive.Types.InputLocation as Types
import qualified Network.AWS.Prelude as Core

-- | Avail Blanking
--
-- /See:/ 'mkAvailBlanking' smart constructor.
data AvailBlanking = AvailBlanking'
  { -- | Blanking image to be used. Leave empty for solid black. Only bmp and png images are supported.
    availBlankingImage :: Core.Maybe Types.InputLocation,
    -- | When set to enabled, causes video, audio and captions to be blanked when insertion metadata is added.
    state :: Core.Maybe Types.AvailBlankingState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AvailBlanking' value with any optional fields omitted.
mkAvailBlanking ::
  AvailBlanking
mkAvailBlanking =
  AvailBlanking'
    { availBlankingImage = Core.Nothing,
      state = Core.Nothing
    }

-- | Blanking image to be used. Leave empty for solid black. Only bmp and png images are supported.
--
-- /Note:/ Consider using 'availBlankingImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abAvailBlankingImage :: Lens.Lens' AvailBlanking (Core.Maybe Types.InputLocation)
abAvailBlankingImage = Lens.field @"availBlankingImage"
{-# DEPRECATED abAvailBlankingImage "Use generic-lens or generic-optics with 'availBlankingImage' instead." #-}

-- | When set to enabled, causes video, audio and captions to be blanked when insertion metadata is added.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abState :: Lens.Lens' AvailBlanking (Core.Maybe Types.AvailBlankingState)
abState = Lens.field @"state"
{-# DEPRECATED abState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON AvailBlanking where
  toJSON AvailBlanking {..} =
    Core.object
      ( Core.catMaybes
          [ ("availBlankingImage" Core..=) Core.<$> availBlankingImage,
            ("state" Core..=) Core.<$> state
          ]
      )

instance Core.FromJSON AvailBlanking where
  parseJSON =
    Core.withObject "AvailBlanking" Core.$
      \x ->
        AvailBlanking'
          Core.<$> (x Core..:? "availBlankingImage") Core.<*> (x Core..:? "state")
