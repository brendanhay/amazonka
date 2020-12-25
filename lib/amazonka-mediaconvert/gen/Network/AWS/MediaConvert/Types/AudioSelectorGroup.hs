{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioSelectorGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioSelectorGroup
  ( AudioSelectorGroup (..),

    -- * Smart constructor
    mkAudioSelectorGroup,

    -- * Lenses
    asgAudioSelectorNames,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Group of Audio Selectors
--
-- /See:/ 'mkAudioSelectorGroup' smart constructor.
newtype AudioSelectorGroup = AudioSelectorGroup'
  { -- | Name of an Audio Selector within the same input to include in the group.  Audio selector names are standardized, based on their order within the input (e.g., "Audio Selector 1"). The audio selector name parameter can be repeated to add any number of audio selectors to the group.
    audioSelectorNames :: Core.Maybe [Core.Text]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AudioSelectorGroup' value with any optional fields omitted.
mkAudioSelectorGroup ::
  AudioSelectorGroup
mkAudioSelectorGroup =
  AudioSelectorGroup' {audioSelectorNames = Core.Nothing}

-- | Name of an Audio Selector within the same input to include in the group.  Audio selector names are standardized, based on their order within the input (e.g., "Audio Selector 1"). The audio selector name parameter can be repeated to add any number of audio selectors to the group.
--
-- /Note:/ Consider using 'audioSelectorNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgAudioSelectorNames :: Lens.Lens' AudioSelectorGroup (Core.Maybe [Core.Text])
asgAudioSelectorNames = Lens.field @"audioSelectorNames"
{-# DEPRECATED asgAudioSelectorNames "Use generic-lens or generic-optics with 'audioSelectorNames' instead." #-}

instance Core.FromJSON AudioSelectorGroup where
  toJSON AudioSelectorGroup {..} =
    Core.object
      ( Core.catMaybes
          [("audioSelectorNames" Core..=) Core.<$> audioSelectorNames]
      )

instance Core.FromJSON AudioSelectorGroup where
  parseJSON =
    Core.withObject "AudioSelectorGroup" Core.$
      \x ->
        AudioSelectorGroup' Core.<$> (x Core..:? "audioSelectorNames")
