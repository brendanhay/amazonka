{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioSelectorSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AudioSelectorSettings
  ( AudioSelectorSettings (..)
  -- * Smart constructor
  , mkAudioSelectorSettings
  -- * Lenses
  , assAudioLanguageSelection
  , assAudioPidSelection
  , assAudioTrackSelection
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AudioLanguageSelection as Types
import qualified Network.AWS.MediaLive.Types.AudioPidSelection as Types
import qualified Network.AWS.MediaLive.Types.AudioTrackSelection as Types
import qualified Network.AWS.Prelude as Core

-- | Audio Selector Settings
--
-- /See:/ 'mkAudioSelectorSettings' smart constructor.
data AudioSelectorSettings = AudioSelectorSettings'
  { audioLanguageSelection :: Core.Maybe Types.AudioLanguageSelection
  , audioPidSelection :: Core.Maybe Types.AudioPidSelection
  , audioTrackSelection :: Core.Maybe Types.AudioTrackSelection
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AudioSelectorSettings' value with any optional fields omitted.
mkAudioSelectorSettings
    :: AudioSelectorSettings
mkAudioSelectorSettings
  = AudioSelectorSettings'{audioLanguageSelection = Core.Nothing,
                           audioPidSelection = Core.Nothing,
                           audioTrackSelection = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'audioLanguageSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assAudioLanguageSelection :: Lens.Lens' AudioSelectorSettings (Core.Maybe Types.AudioLanguageSelection)
assAudioLanguageSelection = Lens.field @"audioLanguageSelection"
{-# INLINEABLE assAudioLanguageSelection #-}
{-# DEPRECATED audioLanguageSelection "Use generic-lens or generic-optics with 'audioLanguageSelection' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'audioPidSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assAudioPidSelection :: Lens.Lens' AudioSelectorSettings (Core.Maybe Types.AudioPidSelection)
assAudioPidSelection = Lens.field @"audioPidSelection"
{-# INLINEABLE assAudioPidSelection #-}
{-# DEPRECATED audioPidSelection "Use generic-lens or generic-optics with 'audioPidSelection' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'audioTrackSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assAudioTrackSelection :: Lens.Lens' AudioSelectorSettings (Core.Maybe Types.AudioTrackSelection)
assAudioTrackSelection = Lens.field @"audioTrackSelection"
{-# INLINEABLE assAudioTrackSelection #-}
{-# DEPRECATED audioTrackSelection "Use generic-lens or generic-optics with 'audioTrackSelection' instead"  #-}

instance Core.FromJSON AudioSelectorSettings where
        toJSON AudioSelectorSettings{..}
          = Core.object
              (Core.catMaybes
                 [("audioLanguageSelection" Core..=) Core.<$>
                    audioLanguageSelection,
                  ("audioPidSelection" Core..=) Core.<$> audioPidSelection,
                  ("audioTrackSelection" Core..=) Core.<$> audioTrackSelection])

instance Core.FromJSON AudioSelectorSettings where
        parseJSON
          = Core.withObject "AudioSelectorSettings" Core.$
              \ x ->
                AudioSelectorSettings' Core.<$>
                  (x Core..:? "audioLanguageSelection") Core.<*>
                    x Core..:? "audioPidSelection"
                    Core.<*> x Core..:? "audioTrackSelection"
