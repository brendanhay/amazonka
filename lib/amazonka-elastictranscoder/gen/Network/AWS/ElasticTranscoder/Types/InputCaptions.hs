{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.InputCaptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.InputCaptions
  ( InputCaptions (..)
  -- * Smart constructor
  , mkInputCaptions
  -- * Lenses
  , icCaptionSources
  , icMergePolicy
  ) where

import qualified Network.AWS.ElasticTranscoder.Types.CaptionMergePolicy as Types
import qualified Network.AWS.ElasticTranscoder.Types.CaptionSource as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The captions to be created, if any.
--
-- /See:/ 'mkInputCaptions' smart constructor.
data InputCaptions = InputCaptions'
  { captionSources :: Core.Maybe [Types.CaptionSource]
    -- ^ Source files for the input sidecar captions used during the transcoding process. To omit all sidecar captions, leave @CaptionSources@ blank.
  , mergePolicy :: Core.Maybe Types.CaptionMergePolicy
    -- ^ A policy that determines how Elastic Transcoder handles the existence of multiple captions.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputCaptions' value with any optional fields omitted.
mkInputCaptions
    :: InputCaptions
mkInputCaptions
  = InputCaptions'{captionSources = Core.Nothing,
                   mergePolicy = Core.Nothing}

-- | Source files for the input sidecar captions used during the transcoding process. To omit all sidecar captions, leave @CaptionSources@ blank.
--
-- /Note:/ Consider using 'captionSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCaptionSources :: Lens.Lens' InputCaptions (Core.Maybe [Types.CaptionSource])
icCaptionSources = Lens.field @"captionSources"
{-# INLINEABLE icCaptionSources #-}
{-# DEPRECATED captionSources "Use generic-lens or generic-optics with 'captionSources' instead"  #-}

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
icMergePolicy :: Lens.Lens' InputCaptions (Core.Maybe Types.CaptionMergePolicy)
icMergePolicy = Lens.field @"mergePolicy"
{-# INLINEABLE icMergePolicy #-}
{-# DEPRECATED mergePolicy "Use generic-lens or generic-optics with 'mergePolicy' instead"  #-}

instance Core.FromJSON InputCaptions where
        toJSON InputCaptions{..}
          = Core.object
              (Core.catMaybes
                 [("CaptionSources" Core..=) Core.<$> captionSources,
                  ("MergePolicy" Core..=) Core.<$> mergePolicy])

instance Core.FromJSON InputCaptions where
        parseJSON
          = Core.withObject "InputCaptions" Core.$
              \ x ->
                InputCaptions' Core.<$>
                  (x Core..:? "CaptionSources") Core.<*> x Core..:? "MergePolicy"
