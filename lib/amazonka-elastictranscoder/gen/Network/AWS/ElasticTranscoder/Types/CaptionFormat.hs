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
    cfPattern,
    cfFormat,
    cfEncryption,
  )
where

import Network.AWS.ElasticTranscoder.Types.Encryption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The file format of the output captions. If you leave this value blank, Elastic Transcoder returns an error.
--
-- /See:/ 'mkCaptionFormat' smart constructor.
data CaptionFormat = CaptionFormat'
  { pattern' ::
      Lude.Maybe Lude.Text,
    format :: Lude.Maybe Lude.Text,
    encryption :: Lude.Maybe Encryption
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CaptionFormat' with the minimum fields required to make a request.
--
-- * 'encryption' - The encryption settings, if any, that you want Elastic Transcoder to apply to your caption formats.
-- * 'format' - The format you specify determines whether Elastic Transcoder generates an embedded or sidecar caption for this output.
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
-- * 'pattern'' - The prefix for caption filenames, in the form /description/ -@{language}@ , where:
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
mkCaptionFormat ::
  CaptionFormat
mkCaptionFormat =
  CaptionFormat'
    { pattern' = Lude.Nothing,
      format = Lude.Nothing,
      encryption = Lude.Nothing
    }

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
cfPattern :: Lens.Lens' CaptionFormat (Lude.Maybe Lude.Text)
cfPattern = Lens.lens (pattern' :: CaptionFormat -> Lude.Maybe Lude.Text) (\s a -> s {pattern' = a} :: CaptionFormat)
{-# DEPRECATED cfPattern "Use generic-lens or generic-optics with 'pattern'' instead." #-}

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
cfFormat :: Lens.Lens' CaptionFormat (Lude.Maybe Lude.Text)
cfFormat = Lens.lens (format :: CaptionFormat -> Lude.Maybe Lude.Text) (\s a -> s {format = a} :: CaptionFormat)
{-# DEPRECATED cfFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your caption formats.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEncryption :: Lens.Lens' CaptionFormat (Lude.Maybe Encryption)
cfEncryption = Lens.lens (encryption :: CaptionFormat -> Lude.Maybe Encryption) (\s a -> s {encryption = a} :: CaptionFormat)
{-# DEPRECATED cfEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

instance Lude.FromJSON CaptionFormat where
  parseJSON =
    Lude.withObject
      "CaptionFormat"
      ( \x ->
          CaptionFormat'
            Lude.<$> (x Lude..:? "Pattern")
            Lude.<*> (x Lude..:? "Format")
            Lude.<*> (x Lude..:? "Encryption")
      )

instance Lude.ToJSON CaptionFormat where
  toJSON CaptionFormat' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Pattern" Lude..=) Lude.<$> pattern',
            ("Format" Lude..=) Lude.<$> format,
            ("Encryption" Lude..=) Lude.<$> encryption
          ]
      )
