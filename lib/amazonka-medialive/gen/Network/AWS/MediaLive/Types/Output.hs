{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Output
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Output
  ( Output (..),

    -- * Smart constructor
    mkOutput,

    -- * Lenses
    oCaptionDescriptionNames,
    oVideoDescriptionName,
    oOutputName,
    oAudioDescriptionNames,
    oOutputSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.OutputSettings
import qualified Network.AWS.Prelude as Lude

-- | Output settings. There can be multiple outputs within a group.
--
-- /See:/ 'mkOutput' smart constructor.
data Output = Output'
  { -- | The names of the CaptionDescriptions used as caption sources for this output.
    captionDescriptionNames :: Lude.Maybe [Lude.Text],
    -- | The name of the VideoDescription used as the source for this output.
    videoDescriptionName :: Lude.Maybe Lude.Text,
    -- | The name used to identify an output.
    outputName :: Lude.Maybe Lude.Text,
    -- | The names of the AudioDescriptions used as audio sources for this output.
    audioDescriptionNames :: Lude.Maybe [Lude.Text],
    -- | Output type-specific settings.
    outputSettings :: OutputSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Output' with the minimum fields required to make a request.
--
-- * 'captionDescriptionNames' - The names of the CaptionDescriptions used as caption sources for this output.
-- * 'videoDescriptionName' - The name of the VideoDescription used as the source for this output.
-- * 'outputName' - The name used to identify an output.
-- * 'audioDescriptionNames' - The names of the AudioDescriptions used as audio sources for this output.
-- * 'outputSettings' - Output type-specific settings.
mkOutput ::
  -- | 'outputSettings'
  OutputSettings ->
  Output
mkOutput pOutputSettings_ =
  Output'
    { captionDescriptionNames = Lude.Nothing,
      videoDescriptionName = Lude.Nothing,
      outputName = Lude.Nothing,
      audioDescriptionNames = Lude.Nothing,
      outputSettings = pOutputSettings_
    }

-- | The names of the CaptionDescriptions used as caption sources for this output.
--
-- /Note:/ Consider using 'captionDescriptionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oCaptionDescriptionNames :: Lens.Lens' Output (Lude.Maybe [Lude.Text])
oCaptionDescriptionNames = Lens.lens (captionDescriptionNames :: Output -> Lude.Maybe [Lude.Text]) (\s a -> s {captionDescriptionNames = a} :: Output)
{-# DEPRECATED oCaptionDescriptionNames "Use generic-lens or generic-optics with 'captionDescriptionNames' instead." #-}

-- | The name of the VideoDescription used as the source for this output.
--
-- /Note:/ Consider using 'videoDescriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oVideoDescriptionName :: Lens.Lens' Output (Lude.Maybe Lude.Text)
oVideoDescriptionName = Lens.lens (videoDescriptionName :: Output -> Lude.Maybe Lude.Text) (\s a -> s {videoDescriptionName = a} :: Output)
{-# DEPRECATED oVideoDescriptionName "Use generic-lens or generic-optics with 'videoDescriptionName' instead." #-}

-- | The name used to identify an output.
--
-- /Note:/ Consider using 'outputName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOutputName :: Lens.Lens' Output (Lude.Maybe Lude.Text)
oOutputName = Lens.lens (outputName :: Output -> Lude.Maybe Lude.Text) (\s a -> s {outputName = a} :: Output)
{-# DEPRECATED oOutputName "Use generic-lens or generic-optics with 'outputName' instead." #-}

-- | The names of the AudioDescriptions used as audio sources for this output.
--
-- /Note:/ Consider using 'audioDescriptionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oAudioDescriptionNames :: Lens.Lens' Output (Lude.Maybe [Lude.Text])
oAudioDescriptionNames = Lens.lens (audioDescriptionNames :: Output -> Lude.Maybe [Lude.Text]) (\s a -> s {audioDescriptionNames = a} :: Output)
{-# DEPRECATED oAudioDescriptionNames "Use generic-lens or generic-optics with 'audioDescriptionNames' instead." #-}

-- | Output type-specific settings.
--
-- /Note:/ Consider using 'outputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOutputSettings :: Lens.Lens' Output OutputSettings
oOutputSettings = Lens.lens (outputSettings :: Output -> OutputSettings) (\s a -> s {outputSettings = a} :: Output)
{-# DEPRECATED oOutputSettings "Use generic-lens or generic-optics with 'outputSettings' instead." #-}

instance Lude.FromJSON Output where
  parseJSON =
    Lude.withObject
      "Output"
      ( \x ->
          Output'
            Lude.<$> (x Lude..:? "captionDescriptionNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "videoDescriptionName")
            Lude.<*> (x Lude..:? "outputName")
            Lude.<*> (x Lude..:? "audioDescriptionNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "outputSettings")
      )

instance Lude.ToJSON Output where
  toJSON Output' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("captionDescriptionNames" Lude..=)
              Lude.<$> captionDescriptionNames,
            ("videoDescriptionName" Lude..=) Lude.<$> videoDescriptionName,
            ("outputName" Lude..=) Lude.<$> outputName,
            ("audioDescriptionNames" Lude..=) Lude.<$> audioDescriptionNames,
            Lude.Just ("outputSettings" Lude..= outputSettings)
          ]
      )
