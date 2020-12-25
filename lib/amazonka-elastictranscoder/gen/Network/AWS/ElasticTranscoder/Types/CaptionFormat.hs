{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.CaptionFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.CaptionFormat
  ( CaptionFormat (..),

    -- * Smart constructor
    mkCaptionFormat,

    -- * Lenses
    cfEncryption,
    cfFormat,
    cfPattern,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types.CaptionFormatFormat as Types
import qualified Network.AWS.ElasticTranscoder.Types.CaptionFormatPattern as Types
import qualified Network.AWS.ElasticTranscoder.Types.Encryption as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The file format of the output captions. If you leave this value blank, Elastic Transcoder returns an error.
--
-- /See:/ 'mkCaptionFormat' smart constructor.
data CaptionFormat = CaptionFormat'
  { -- | The encryption settings, if any, that you want Elastic Transcoder to apply to your caption formats.
    encryption :: Core.Maybe Types.Encryption,
    -- | The format you specify determines whether Elastic Transcoder generates an embedded or sidecar caption for this output.
    --
    --
    --     * __Valid Embedded Caption Formats:__
    --
    --     * __for FLAC__ : None
    --
    --
    --     * __For MP3__ : None
    --
    --
    --     * __For MP4__ : mov-text
    --
    --
    --     * __For MPEG-TS__ : None
    --
    --
    --     * __For ogg__ : None
    --
    --
    --     * __For webm__ : None
    --
    --
    --
    --
    --     * __Valid Sidecar Caption Formats:__ Elastic Transcoder supports dfxp (first div element only), scc, srt, and webvtt. If you want ttml or smpte-tt compatible captions, specify dfxp as your output format.
    --
    --     * __For FMP4__ : dfxp
    --
    --
    --     * __Non-FMP4 outputs__ : All sidecar types
    --
    --
    -- @fmp4@ captions have an extension of @.ismt@
    format :: Core.Maybe Types.CaptionFormatFormat,
    -- | The prefix for caption filenames, in the form /description/ -@{language}@ , where:
    --
    --
    --     * /description/ is a description of the video.
    --
    --
    --     * @{language}@ is a literal value that Elastic Transcoder replaces with the two- or three-letter code for the language of the caption in the output file names.
    --
    --
    -- If you don't include @{language}@ in the file name pattern, Elastic Transcoder automatically appends "@{language}@ " to the value that you specify for the description. In addition, Elastic Transcoder automatically appends the count to the end of the segment files.
    -- For example, suppose you're transcoding into srt format. When you enter "Sydney-{language}-sunrise", and the language of the captions is English (en), the name of the first caption file is be Sydney-en-sunrise00000.srt.
    pattern' :: Core.Maybe Types.CaptionFormatPattern
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CaptionFormat' value with any optional fields omitted.
mkCaptionFormat ::
  CaptionFormat
mkCaptionFormat =
  CaptionFormat'
    { encryption = Core.Nothing,
      format = Core.Nothing,
      pattern' = Core.Nothing
    }

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your caption formats.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEncryption :: Lens.Lens' CaptionFormat (Core.Maybe Types.Encryption)
cfEncryption = Lens.field @"encryption"
{-# DEPRECATED cfEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | The format you specify determines whether Elastic Transcoder generates an embedded or sidecar caption for this output.
--
--
--     * __Valid Embedded Caption Formats:__
--
--     * __for FLAC__ : None
--
--
--     * __For MP3__ : None
--
--
--     * __For MP4__ : mov-text
--
--
--     * __For MPEG-TS__ : None
--
--
--     * __For ogg__ : None
--
--
--     * __For webm__ : None
--
--
--
--
--     * __Valid Sidecar Caption Formats:__ Elastic Transcoder supports dfxp (first div element only), scc, srt, and webvtt. If you want ttml or smpte-tt compatible captions, specify dfxp as your output format.
--
--     * __For FMP4__ : dfxp
--
--
--     * __Non-FMP4 outputs__ : All sidecar types
--
--
-- @fmp4@ captions have an extension of @.ismt@
--
--
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfFormat :: Lens.Lens' CaptionFormat (Core.Maybe Types.CaptionFormatFormat)
cfFormat = Lens.field @"format"
{-# DEPRECATED cfFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The prefix for caption filenames, in the form /description/ -@{language}@ , where:
--
--
--     * /description/ is a description of the video.
--
--
--     * @{language}@ is a literal value that Elastic Transcoder replaces with the two- or three-letter code for the language of the caption in the output file names.
--
--
-- If you don't include @{language}@ in the file name pattern, Elastic Transcoder automatically appends "@{language}@ " to the value that you specify for the description. In addition, Elastic Transcoder automatically appends the count to the end of the segment files.
-- For example, suppose you're transcoding into srt format. When you enter "Sydney-{language}-sunrise", and the language of the captions is English (en), the name of the first caption file is be Sydney-en-sunrise00000.srt.
--
-- /Note:/ Consider using 'pattern'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfPattern :: Lens.Lens' CaptionFormat (Core.Maybe Types.CaptionFormatPattern)
cfPattern = Lens.field @"pattern'"
{-# DEPRECATED cfPattern "Use generic-lens or generic-optics with 'pattern'' instead." #-}

instance Core.FromJSON CaptionFormat where
  toJSON CaptionFormat {..} =
    Core.object
      ( Core.catMaybes
          [ ("Encryption" Core..=) Core.<$> encryption,
            ("Format" Core..=) Core.<$> format,
            ("Pattern" Core..=) Core.<$> pattern'
          ]
      )

instance Core.FromJSON CaptionFormat where
  parseJSON =
    Core.withObject "CaptionFormat" Core.$
      \x ->
        CaptionFormat'
          Core.<$> (x Core..:? "Encryption")
          Core.<*> (x Core..:? "Format")
          Core.<*> (x Core..:? "Pattern")
