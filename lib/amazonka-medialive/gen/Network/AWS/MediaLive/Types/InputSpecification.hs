{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSpecification
  ( InputSpecification (..),

    -- * Smart constructor
    mkInputSpecification,

    -- * Lenses
    isCodec,
    isMaximumBitrate,
    isResolution,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.InputCodec as Types
import qualified Network.AWS.MediaLive.Types.InputMaximumBitrate as Types
import qualified Network.AWS.MediaLive.Types.InputResolution as Types
import qualified Network.AWS.Prelude as Core

-- | Placeholder documentation for InputSpecification
--
-- /See:/ 'mkInputSpecification' smart constructor.
data InputSpecification = InputSpecification'
  { -- | Input codec
    codec :: Core.Maybe Types.InputCodec,
    -- | Maximum input bitrate, categorized coarsely
    maximumBitrate :: Core.Maybe Types.InputMaximumBitrate,
    -- | Input resolution, categorized coarsely
    resolution :: Core.Maybe Types.InputResolution
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputSpecification' value with any optional fields omitted.
mkInputSpecification ::
  InputSpecification
mkInputSpecification =
  InputSpecification'
    { codec = Core.Nothing,
      maximumBitrate = Core.Nothing,
      resolution = Core.Nothing
    }

-- | Input codec
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCodec :: Lens.Lens' InputSpecification (Core.Maybe Types.InputCodec)
isCodec = Lens.field @"codec"
{-# DEPRECATED isCodec "Use generic-lens or generic-optics with 'codec' instead." #-}

-- | Maximum input bitrate, categorized coarsely
--
-- /Note:/ Consider using 'maximumBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isMaximumBitrate :: Lens.Lens' InputSpecification (Core.Maybe Types.InputMaximumBitrate)
isMaximumBitrate = Lens.field @"maximumBitrate"
{-# DEPRECATED isMaximumBitrate "Use generic-lens or generic-optics with 'maximumBitrate' instead." #-}

-- | Input resolution, categorized coarsely
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isResolution :: Lens.Lens' InputSpecification (Core.Maybe Types.InputResolution)
isResolution = Lens.field @"resolution"
{-# DEPRECATED isResolution "Use generic-lens or generic-optics with 'resolution' instead." #-}

instance Core.FromJSON InputSpecification where
  toJSON InputSpecification {..} =
    Core.object
      ( Core.catMaybes
          [ ("codec" Core..=) Core.<$> codec,
            ("maximumBitrate" Core..=) Core.<$> maximumBitrate,
            ("resolution" Core..=) Core.<$> resolution
          ]
      )

instance Core.FromJSON InputSpecification where
  parseJSON =
    Core.withObject "InputSpecification" Core.$
      \x ->
        InputSpecification'
          Core.<$> (x Core..:? "codec")
          Core.<*> (x Core..:? "maximumBitrate")
          Core.<*> (x Core..:? "resolution")
