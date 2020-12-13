{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioChannelMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioChannelMapping
  ( AudioChannelMapping (..),

    -- * Smart constructor
    mkAudioChannelMapping,

    -- * Lenses
    acmOutputChannel,
    acmInputChannelLevels,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputChannelLevel
import qualified Network.AWS.Prelude as Lude

-- | Audio Channel Mapping
--
-- /See:/ 'mkAudioChannelMapping' smart constructor.
data AudioChannelMapping = AudioChannelMapping'
  { -- | The index of the output channel being produced.
    outputChannel :: Lude.Natural,
    -- | Indices and gain values for each input channel that should be remixed into this output channel.
    inputChannelLevels :: [InputChannelLevel]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioChannelMapping' with the minimum fields required to make a request.
--
-- * 'outputChannel' - The index of the output channel being produced.
-- * 'inputChannelLevels' - Indices and gain values for each input channel that should be remixed into this output channel.
mkAudioChannelMapping ::
  -- | 'outputChannel'
  Lude.Natural ->
  AudioChannelMapping
mkAudioChannelMapping pOutputChannel_ =
  AudioChannelMapping'
    { outputChannel = pOutputChannel_,
      inputChannelLevels = Lude.mempty
    }

-- | The index of the output channel being produced.
--
-- /Note:/ Consider using 'outputChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acmOutputChannel :: Lens.Lens' AudioChannelMapping Lude.Natural
acmOutputChannel = Lens.lens (outputChannel :: AudioChannelMapping -> Lude.Natural) (\s a -> s {outputChannel = a} :: AudioChannelMapping)
{-# DEPRECATED acmOutputChannel "Use generic-lens or generic-optics with 'outputChannel' instead." #-}

-- | Indices and gain values for each input channel that should be remixed into this output channel.
--
-- /Note:/ Consider using 'inputChannelLevels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acmInputChannelLevels :: Lens.Lens' AudioChannelMapping [InputChannelLevel]
acmInputChannelLevels = Lens.lens (inputChannelLevels :: AudioChannelMapping -> [InputChannelLevel]) (\s a -> s {inputChannelLevels = a} :: AudioChannelMapping)
{-# DEPRECATED acmInputChannelLevels "Use generic-lens or generic-optics with 'inputChannelLevels' instead." #-}

instance Lude.FromJSON AudioChannelMapping where
  parseJSON =
    Lude.withObject
      "AudioChannelMapping"
      ( \x ->
          AudioChannelMapping'
            Lude.<$> (x Lude..: "outputChannel")
            Lude.<*> (x Lude..:? "inputChannelLevels" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON AudioChannelMapping where
  toJSON AudioChannelMapping' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("outputChannel" Lude..= outputChannel),
            Lude.Just ("inputChannelLevels" Lude..= inputChannelLevels)
          ]
      )
