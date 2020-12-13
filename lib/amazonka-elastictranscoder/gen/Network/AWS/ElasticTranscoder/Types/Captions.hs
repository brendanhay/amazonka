{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Captions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Captions
  ( Captions (..),

    -- * Smart constructor
    mkCaptions,

    -- * Lenses
    cMergePolicy,
    cCaptionSources,
    cCaptionFormats,
  )
where

import Network.AWS.ElasticTranscoder.Types.CaptionFormat
import Network.AWS.ElasticTranscoder.Types.CaptionSource
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The captions to be created, if any.
--
-- /See:/ 'mkCaptions' smart constructor.
data Captions = Captions'
  { -- | A policy that determines how Elastic Transcoder handles the existence of multiple captions.
    --
    --
    --     * __MergeOverride:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the sidecar captions and ignores the embedded captions for that language.
    --
    --
    --     * __MergeRetain:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the embedded captions and ignores the sidecar captions for that language. If @CaptionSources@ is empty, Elastic Transcoder omits all sidecar captions from the output files.
    --
    --
    --     * __Override:__ Elastic Transcoder transcodes only the sidecar captions that you specify in @CaptionSources@ .
    --
    --
    -- @MergePolicy@ cannot be null.
    mergePolicy :: Lude.Maybe Lude.Text,
    -- | Source files for the input sidecar captions used during the transcoding process. To omit all sidecar captions, leave @CaptionSources@ blank.
    captionSources :: Lude.Maybe [CaptionSource],
    -- | The array of file formats for the output captions. If you leave this value blank, Elastic Transcoder returns an error.
    captionFormats :: Lude.Maybe [CaptionFormat]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Captions' with the minimum fields required to make a request.
--
-- * 'mergePolicy' - A policy that determines how Elastic Transcoder handles the existence of multiple captions.
--
--
--     * __MergeOverride:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the sidecar captions and ignores the embedded captions for that language.
--
--
--     * __MergeRetain:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the embedded captions and ignores the sidecar captions for that language. If @CaptionSources@ is empty, Elastic Transcoder omits all sidecar captions from the output files.
--
--
--     * __Override:__ Elastic Transcoder transcodes only the sidecar captions that you specify in @CaptionSources@ .
--
--
-- @MergePolicy@ cannot be null.
-- * 'captionSources' - Source files for the input sidecar captions used during the transcoding process. To omit all sidecar captions, leave @CaptionSources@ blank.
-- * 'captionFormats' - The array of file formats for the output captions. If you leave this value blank, Elastic Transcoder returns an error.
mkCaptions ::
  Captions
mkCaptions =
  Captions'
    { mergePolicy = Lude.Nothing,
      captionSources = Lude.Nothing,
      captionFormats = Lude.Nothing
    }

-- | A policy that determines how Elastic Transcoder handles the existence of multiple captions.
--
--
--     * __MergeOverride:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the sidecar captions and ignores the embedded captions for that language.
--
--
--     * __MergeRetain:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the embedded captions and ignores the sidecar captions for that language. If @CaptionSources@ is empty, Elastic Transcoder omits all sidecar captions from the output files.
--
--
--     * __Override:__ Elastic Transcoder transcodes only the sidecar captions that you specify in @CaptionSources@ .
--
--
-- @MergePolicy@ cannot be null.
--
-- /Note:/ Consider using 'mergePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMergePolicy :: Lens.Lens' Captions (Lude.Maybe Lude.Text)
cMergePolicy = Lens.lens (mergePolicy :: Captions -> Lude.Maybe Lude.Text) (\s a -> s {mergePolicy = a} :: Captions)
{-# DEPRECATED cMergePolicy "Use generic-lens or generic-optics with 'mergePolicy' instead." #-}

-- | Source files for the input sidecar captions used during the transcoding process. To omit all sidecar captions, leave @CaptionSources@ blank.
--
-- /Note:/ Consider using 'captionSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCaptionSources :: Lens.Lens' Captions (Lude.Maybe [CaptionSource])
cCaptionSources = Lens.lens (captionSources :: Captions -> Lude.Maybe [CaptionSource]) (\s a -> s {captionSources = a} :: Captions)
{-# DEPRECATED cCaptionSources "Use generic-lens or generic-optics with 'captionSources' instead." #-}

-- | The array of file formats for the output captions. If you leave this value blank, Elastic Transcoder returns an error.
--
-- /Note:/ Consider using 'captionFormats' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCaptionFormats :: Lens.Lens' Captions (Lude.Maybe [CaptionFormat])
cCaptionFormats = Lens.lens (captionFormats :: Captions -> Lude.Maybe [CaptionFormat]) (\s a -> s {captionFormats = a} :: Captions)
{-# DEPRECATED cCaptionFormats "Use generic-lens or generic-optics with 'captionFormats' instead." #-}

instance Lude.FromJSON Captions where
  parseJSON =
    Lude.withObject
      "Captions"
      ( \x ->
          Captions'
            Lude.<$> (x Lude..:? "MergePolicy")
            Lude.<*> (x Lude..:? "CaptionSources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CaptionFormats" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON Captions where
  toJSON Captions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MergePolicy" Lude..=) Lude.<$> mergePolicy,
            ("CaptionSources" Lude..=) Lude.<$> captionSources,
            ("CaptionFormats" Lude..=) Lude.<$> captionFormats
          ]
      )
