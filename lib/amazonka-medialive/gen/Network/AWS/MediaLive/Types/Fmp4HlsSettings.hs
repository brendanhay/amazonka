{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Fmp4HlsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Fmp4HlsSettings
  ( Fmp4HlsSettings (..)
  -- * Smart constructor
  , mkFmp4HlsSettings
  -- * Lenses
  , fhsAudioRenditionSets
  , fhsNielsenId3Behavior
  , fhsTimedMetadataBehavior
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.Fmp4NielsenId3Behavior as Types
import qualified Network.AWS.MediaLive.Types.Fmp4TimedMetadataBehavior as Types
import qualified Network.AWS.Prelude as Core

-- | Fmp4 Hls Settings
--
-- /See:/ 'mkFmp4HlsSettings' smart constructor.
data Fmp4HlsSettings = Fmp4HlsSettings'
  { audioRenditionSets :: Core.Maybe Core.Text
    -- ^ List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
  , nielsenId3Behavior :: Core.Maybe Types.Fmp4NielsenId3Behavior
    -- ^ If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
  , timedMetadataBehavior :: Core.Maybe Types.Fmp4TimedMetadataBehavior
    -- ^ When set to passthrough, timed metadata is passed through from input to output.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Fmp4HlsSettings' value with any optional fields omitted.
mkFmp4HlsSettings
    :: Fmp4HlsSettings
mkFmp4HlsSettings
  = Fmp4HlsSettings'{audioRenditionSets = Core.Nothing,
                     nielsenId3Behavior = Core.Nothing,
                     timedMetadataBehavior = Core.Nothing}

-- | List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
--
-- /Note:/ Consider using 'audioRenditionSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fhsAudioRenditionSets :: Lens.Lens' Fmp4HlsSettings (Core.Maybe Core.Text)
fhsAudioRenditionSets = Lens.field @"audioRenditionSets"
{-# INLINEABLE fhsAudioRenditionSets #-}
{-# DEPRECATED audioRenditionSets "Use generic-lens or generic-optics with 'audioRenditionSets' instead"  #-}

-- | If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- /Note:/ Consider using 'nielsenId3Behavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fhsNielsenId3Behavior :: Lens.Lens' Fmp4HlsSettings (Core.Maybe Types.Fmp4NielsenId3Behavior)
fhsNielsenId3Behavior = Lens.field @"nielsenId3Behavior"
{-# INLINEABLE fhsNielsenId3Behavior #-}
{-# DEPRECATED nielsenId3Behavior "Use generic-lens or generic-optics with 'nielsenId3Behavior' instead"  #-}

-- | When set to passthrough, timed metadata is passed through from input to output.
--
-- /Note:/ Consider using 'timedMetadataBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fhsTimedMetadataBehavior :: Lens.Lens' Fmp4HlsSettings (Core.Maybe Types.Fmp4TimedMetadataBehavior)
fhsTimedMetadataBehavior = Lens.field @"timedMetadataBehavior"
{-# INLINEABLE fhsTimedMetadataBehavior #-}
{-# DEPRECATED timedMetadataBehavior "Use generic-lens or generic-optics with 'timedMetadataBehavior' instead"  #-}

instance Core.FromJSON Fmp4HlsSettings where
        toJSON Fmp4HlsSettings{..}
          = Core.object
              (Core.catMaybes
                 [("audioRenditionSets" Core..=) Core.<$> audioRenditionSets,
                  ("nielsenId3Behavior" Core..=) Core.<$> nielsenId3Behavior,
                  ("timedMetadataBehavior" Core..=) Core.<$> timedMetadataBehavior])

instance Core.FromJSON Fmp4HlsSettings where
        parseJSON
          = Core.withObject "Fmp4HlsSettings" Core.$
              \ x ->
                Fmp4HlsSettings' Core.<$>
                  (x Core..:? "audioRenditionSets") Core.<*>
                    x Core..:? "nielsenId3Behavior"
                    Core.<*> x Core..:? "timedMetadataBehavior"
