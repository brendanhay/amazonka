{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.CaptionSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.CaptionSource
  ( CaptionSource (..)
  -- * Smart constructor
  , mkCaptionSource
  -- * Lenses
  , csEncryption
  , csKey
  , csLabel
  , csLanguage
  , csTimeOffset
  ) where

import qualified Network.AWS.ElasticTranscoder.Types.Encryption as Types
import qualified Network.AWS.ElasticTranscoder.Types.Label as Types
import qualified Network.AWS.ElasticTranscoder.Types.Language as Types
import qualified Network.AWS.ElasticTranscoder.Types.LongKey as Types
import qualified Network.AWS.ElasticTranscoder.Types.TimeOffset as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A source file for the input sidecar captions used during the transcoding process.
--
-- /See:/ 'mkCaptionSource' smart constructor.
data CaptionSource = CaptionSource'
  { encryption :: Core.Maybe Types.Encryption
    -- ^ The encryption settings, if any, that Elastic Transcoder needs to decyrpt your caption sources, or that you want Elastic Transcoder to apply to your caption sources.
  , key :: Core.Maybe Types.LongKey
    -- ^ The name of the sidecar caption file that you want Elastic Transcoder to include in the output file.
  , label :: Core.Maybe Types.Label
    -- ^ The label of the caption shown in the player when choosing a language. We recommend that you put the caption language name here, in the language of the captions.
  , language :: Core.Maybe Types.Language
    -- ^ A string that specifies the language of the caption. If you specified multiple inputs with captions, the caption language must match in order to be included in the output. Specify this as one of:
--
--
--     * 2-character ISO 639-1 code
--
--
--     * 3-character ISO 639-2 code
--
--
-- For more information on ISO language codes and language names, see the List of ISO 639-1 codes.
  , timeOffset :: Core.Maybe Types.TimeOffset
    -- ^ For clip generation or captions that do not start at the same time as the associated video file, the @TimeOffset@ tells Elastic Transcoder how much of the video to encode before including captions.
--
-- Specify the TimeOffset in the form [+-]SS.sss or [+-]HH:mm:SS.ss.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CaptionSource' value with any optional fields omitted.
mkCaptionSource
    :: CaptionSource
mkCaptionSource
  = CaptionSource'{encryption = Core.Nothing, key = Core.Nothing,
                   label = Core.Nothing, language = Core.Nothing,
                   timeOffset = Core.Nothing}

-- | The encryption settings, if any, that Elastic Transcoder needs to decyrpt your caption sources, or that you want Elastic Transcoder to apply to your caption sources.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEncryption :: Lens.Lens' CaptionSource (Core.Maybe Types.Encryption)
csEncryption = Lens.field @"encryption"
{-# INLINEABLE csEncryption #-}
{-# DEPRECATED encryption "Use generic-lens or generic-optics with 'encryption' instead"  #-}

-- | The name of the sidecar caption file that you want Elastic Transcoder to include in the output file.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csKey :: Lens.Lens' CaptionSource (Core.Maybe Types.LongKey)
csKey = Lens.field @"key"
{-# INLINEABLE csKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The label of the caption shown in the player when choosing a language. We recommend that you put the caption language name here, in the language of the captions.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLabel :: Lens.Lens' CaptionSource (Core.Maybe Types.Label)
csLabel = Lens.field @"label"
{-# INLINEABLE csLabel #-}
{-# DEPRECATED label "Use generic-lens or generic-optics with 'label' instead"  #-}

-- | A string that specifies the language of the caption. If you specified multiple inputs with captions, the caption language must match in order to be included in the output. Specify this as one of:
--
--
--     * 2-character ISO 639-1 code
--
--
--     * 3-character ISO 639-2 code
--
--
-- For more information on ISO language codes and language names, see the List of ISO 639-1 codes.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLanguage :: Lens.Lens' CaptionSource (Core.Maybe Types.Language)
csLanguage = Lens.field @"language"
{-# INLINEABLE csLanguage #-}
{-# DEPRECATED language "Use generic-lens or generic-optics with 'language' instead"  #-}

-- | For clip generation or captions that do not start at the same time as the associated video file, the @TimeOffset@ tells Elastic Transcoder how much of the video to encode before including captions.
--
-- Specify the TimeOffset in the form [+-]SS.sss or [+-]HH:mm:SS.ss.
--
-- /Note:/ Consider using 'timeOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTimeOffset :: Lens.Lens' CaptionSource (Core.Maybe Types.TimeOffset)
csTimeOffset = Lens.field @"timeOffset"
{-# INLINEABLE csTimeOffset #-}
{-# DEPRECATED timeOffset "Use generic-lens or generic-optics with 'timeOffset' instead"  #-}

instance Core.FromJSON CaptionSource where
        toJSON CaptionSource{..}
          = Core.object
              (Core.catMaybes
                 [("Encryption" Core..=) Core.<$> encryption,
                  ("Key" Core..=) Core.<$> key, ("Label" Core..=) Core.<$> label,
                  ("Language" Core..=) Core.<$> language,
                  ("TimeOffset" Core..=) Core.<$> timeOffset])

instance Core.FromJSON CaptionSource where
        parseJSON
          = Core.withObject "CaptionSource" Core.$
              \ x ->
                CaptionSource' Core.<$>
                  (x Core..:? "Encryption") Core.<*> x Core..:? "Key" Core.<*>
                    x Core..:? "Label"
                    Core.<*> x Core..:? "Language"
                    Core.<*> x Core..:? "TimeOffset"
