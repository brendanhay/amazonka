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
import qualified Network.AWS.Prelude as Lude

-- | Group of Audio Selectors
--
-- /See:/ 'mkAudioSelectorGroup' smart constructor.
newtype AudioSelectorGroup = AudioSelectorGroup'
  { audioSelectorNames ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioSelectorGroup' with the minimum fields required to make a request.
--
-- * 'audioSelectorNames' - Name of an Audio Selector within the same input to include in the group.  Audio selector names are standardized, based on their order within the input (e.g., "Audio Selector 1"). The audio selector name parameter can be repeated to add any number of audio selectors to the group.
mkAudioSelectorGroup ::
  AudioSelectorGroup
mkAudioSelectorGroup =
  AudioSelectorGroup' {audioSelectorNames = Lude.Nothing}

-- | Name of an Audio Selector within the same input to include in the group.  Audio selector names are standardized, based on their order within the input (e.g., "Audio Selector 1"). The audio selector name parameter can be repeated to add any number of audio selectors to the group.
--
-- /Note:/ Consider using 'audioSelectorNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgAudioSelectorNames :: Lens.Lens' AudioSelectorGroup (Lude.Maybe [Lude.Text])
asgAudioSelectorNames = Lens.lens (audioSelectorNames :: AudioSelectorGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {audioSelectorNames = a} :: AudioSelectorGroup)
{-# DEPRECATED asgAudioSelectorNames "Use generic-lens or generic-optics with 'audioSelectorNames' instead." #-}

instance Lude.FromJSON AudioSelectorGroup where
  parseJSON =
    Lude.withObject
      "AudioSelectorGroup"
      ( \x ->
          AudioSelectorGroup'
            Lude.<$> (x Lude..:? "audioSelectorNames" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON AudioSelectorGroup where
  toJSON AudioSelectorGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [("audioSelectorNames" Lude..=) Lude.<$> audioSelectorNames]
      )
