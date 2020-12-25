{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ContentModerationDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ContentModerationDetection
  ( ContentModerationDetection (..),

    -- * Smart constructor
    mkContentModerationDetection,

    -- * Lenses
    cmdModerationLabel,
    cmdTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.ModerationLabel as Types

-- | Information about an unsafe content label detection in a stored video.
--
-- /See:/ 'mkContentModerationDetection' smart constructor.
data ContentModerationDetection = ContentModerationDetection'
  { -- | The unsafe content label detected by in the stored video.
    moderationLabel :: Core.Maybe Types.ModerationLabel,
    -- | Time, in milliseconds from the beginning of the video, that the unsafe content label was detected.
    timestamp :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContentModerationDetection' value with any optional fields omitted.
mkContentModerationDetection ::
  ContentModerationDetection
mkContentModerationDetection =
  ContentModerationDetection'
    { moderationLabel = Core.Nothing,
      timestamp = Core.Nothing
    }

-- | The unsafe content label detected by in the stored video.
--
-- /Note:/ Consider using 'moderationLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdModerationLabel :: Lens.Lens' ContentModerationDetection (Core.Maybe Types.ModerationLabel)
cmdModerationLabel = Lens.field @"moderationLabel"
{-# DEPRECATED cmdModerationLabel "Use generic-lens or generic-optics with 'moderationLabel' instead." #-}

-- | Time, in milliseconds from the beginning of the video, that the unsafe content label was detected.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdTimestamp :: Lens.Lens' ContentModerationDetection (Core.Maybe Core.Integer)
cmdTimestamp = Lens.field @"timestamp"
{-# DEPRECATED cmdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Core.FromJSON ContentModerationDetection where
  parseJSON =
    Core.withObject "ContentModerationDetection" Core.$
      \x ->
        ContentModerationDetection'
          Core.<$> (x Core..:? "ModerationLabel") Core.<*> (x Core..:? "Timestamp")
