{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.InputCaptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.InputCaptions
  ( InputCaptions (..),

    -- * Smart constructor
    mkInputCaptions,

    -- * Lenses
    icMergePolicy,
    icCaptionSources,
  )
where

import Network.AWS.ElasticTranscoder.Types.CaptionSource
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The captions to be created, if any.
--
-- /See:/ 'mkInputCaptions' smart constructor.
data InputCaptions = InputCaptions'
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
    captionSources :: Lude.Maybe [CaptionSource]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputCaptions' with the minimum fields required to make a request.
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
mkInputCaptions ::
  InputCaptions
mkInputCaptions =
  InputCaptions'
    { mergePolicy = Lude.Nothing,
      captionSources = Lude.Nothing
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
icMergePolicy :: Lens.Lens' InputCaptions (Lude.Maybe Lude.Text)
icMergePolicy = Lens.lens (mergePolicy :: InputCaptions -> Lude.Maybe Lude.Text) (\s a -> s {mergePolicy = a} :: InputCaptions)
{-# DEPRECATED icMergePolicy "Use generic-lens or generic-optics with 'mergePolicy' instead." #-}

-- | Source files for the input sidecar captions used during the transcoding process. To omit all sidecar captions, leave @CaptionSources@ blank.
--
-- /Note:/ Consider using 'captionSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCaptionSources :: Lens.Lens' InputCaptions (Lude.Maybe [CaptionSource])
icCaptionSources = Lens.lens (captionSources :: InputCaptions -> Lude.Maybe [CaptionSource]) (\s a -> s {captionSources = a} :: InputCaptions)
{-# DEPRECATED icCaptionSources "Use generic-lens or generic-optics with 'captionSources' instead." #-}

instance Lude.FromJSON InputCaptions where
  parseJSON =
    Lude.withObject
      "InputCaptions"
      ( \x ->
          InputCaptions'
            Lude.<$> (x Lude..:? "MergePolicy")
            Lude.<*> (x Lude..:? "CaptionSources" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON InputCaptions where
  toJSON InputCaptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MergePolicy" Lude..=) Lude.<$> mergePolicy,
            ("CaptionSources" Lude..=) Lude.<$> captionSources
          ]
      )
