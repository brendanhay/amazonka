{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.CaptionFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.CaptionFormat where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticTranscoder.Types.Encryption
import qualified Network.AWS.Lens as Lens

-- | The file format of the output captions. If you leave this value blank,
-- Elastic Transcoder returns an error.
--
-- /See:/ 'newCaptionFormat' smart constructor.
data CaptionFormat = CaptionFormat'
  { -- | The format you specify determines whether Elastic Transcoder generates
    -- an embedded or sidecar caption for this output.
    --
    -- -   __Valid Embedded Caption Formats:__
    --
    --     -   __for FLAC__: None
    --
    --     -   __For MP3__: None
    --
    --     -   __For MP4__: mov-text
    --
    --     -   __For MPEG-TS__: None
    --
    --     -   __For ogg__: None
    --
    --     -   __For webm__: None
    --
    -- -   __Valid Sidecar Caption Formats:__ Elastic Transcoder supports dfxp
    --     (first div element only), scc, srt, and webvtt. If you want ttml or
    --     smpte-tt compatible captions, specify dfxp as your output format.
    --
    --     -   __For FMP4__: dfxp
    --
    --     -   __Non-FMP4 outputs__: All sidecar types
    --
    --     @fmp4@ captions have an extension of @.ismt@
    format :: Core.Maybe Core.Text,
    -- | The encryption settings, if any, that you want Elastic Transcoder to
    -- apply to your caption formats.
    encryption :: Core.Maybe Encryption,
    -- | The prefix for caption filenames, in the form
    -- /description/-@{language}@, where:
    --
    -- -   /description/ is a description of the video.
    --
    -- -   @{language}@ is a literal value that Elastic Transcoder replaces
    --     with the two- or three-letter code for the language of the caption
    --     in the output file names.
    --
    -- If you don\'t include @{language}@ in the file name pattern, Elastic
    -- Transcoder automatically appends \"@{language}@\" to the value that you
    -- specify for the description. In addition, Elastic Transcoder
    -- automatically appends the count to the end of the segment files.
    --
    -- For example, suppose you\'re transcoding into srt format. When you enter
    -- \"Sydney-{language}-sunrise\", and the language of the captions is
    -- English (en), the name of the first caption file is be
    -- Sydney-en-sunrise00000.srt.
    pattern' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CaptionFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'captionFormat_format' - The format you specify determines whether Elastic Transcoder generates
-- an embedded or sidecar caption for this output.
--
-- -   __Valid Embedded Caption Formats:__
--
--     -   __for FLAC__: None
--
--     -   __For MP3__: None
--
--     -   __For MP4__: mov-text
--
--     -   __For MPEG-TS__: None
--
--     -   __For ogg__: None
--
--     -   __For webm__: None
--
-- -   __Valid Sidecar Caption Formats:__ Elastic Transcoder supports dfxp
--     (first div element only), scc, srt, and webvtt. If you want ttml or
--     smpte-tt compatible captions, specify dfxp as your output format.
--
--     -   __For FMP4__: dfxp
--
--     -   __Non-FMP4 outputs__: All sidecar types
--
--     @fmp4@ captions have an extension of @.ismt@
--
-- 'encryption', 'captionFormat_encryption' - The encryption settings, if any, that you want Elastic Transcoder to
-- apply to your caption formats.
--
-- 'pattern'', 'captionFormat_pattern' - The prefix for caption filenames, in the form
-- /description/-@{language}@, where:
--
-- -   /description/ is a description of the video.
--
-- -   @{language}@ is a literal value that Elastic Transcoder replaces
--     with the two- or three-letter code for the language of the caption
--     in the output file names.
--
-- If you don\'t include @{language}@ in the file name pattern, Elastic
-- Transcoder automatically appends \"@{language}@\" to the value that you
-- specify for the description. In addition, Elastic Transcoder
-- automatically appends the count to the end of the segment files.
--
-- For example, suppose you\'re transcoding into srt format. When you enter
-- \"Sydney-{language}-sunrise\", and the language of the captions is
-- English (en), the name of the first caption file is be
-- Sydney-en-sunrise00000.srt.
newCaptionFormat ::
  CaptionFormat
newCaptionFormat =
  CaptionFormat'
    { format = Core.Nothing,
      encryption = Core.Nothing,
      pattern' = Core.Nothing
    }

-- | The format you specify determines whether Elastic Transcoder generates
-- an embedded or sidecar caption for this output.
--
-- -   __Valid Embedded Caption Formats:__
--
--     -   __for FLAC__: None
--
--     -   __For MP3__: None
--
--     -   __For MP4__: mov-text
--
--     -   __For MPEG-TS__: None
--
--     -   __For ogg__: None
--
--     -   __For webm__: None
--
-- -   __Valid Sidecar Caption Formats:__ Elastic Transcoder supports dfxp
--     (first div element only), scc, srt, and webvtt. If you want ttml or
--     smpte-tt compatible captions, specify dfxp as your output format.
--
--     -   __For FMP4__: dfxp
--
--     -   __Non-FMP4 outputs__: All sidecar types
--
--     @fmp4@ captions have an extension of @.ismt@
captionFormat_format :: Lens.Lens' CaptionFormat (Core.Maybe Core.Text)
captionFormat_format = Lens.lens (\CaptionFormat' {format} -> format) (\s@CaptionFormat' {} a -> s {format = a} :: CaptionFormat)

-- | The encryption settings, if any, that you want Elastic Transcoder to
-- apply to your caption formats.
captionFormat_encryption :: Lens.Lens' CaptionFormat (Core.Maybe Encryption)
captionFormat_encryption = Lens.lens (\CaptionFormat' {encryption} -> encryption) (\s@CaptionFormat' {} a -> s {encryption = a} :: CaptionFormat)

-- | The prefix for caption filenames, in the form
-- /description/-@{language}@, where:
--
-- -   /description/ is a description of the video.
--
-- -   @{language}@ is a literal value that Elastic Transcoder replaces
--     with the two- or three-letter code for the language of the caption
--     in the output file names.
--
-- If you don\'t include @{language}@ in the file name pattern, Elastic
-- Transcoder automatically appends \"@{language}@\" to the value that you
-- specify for the description. In addition, Elastic Transcoder
-- automatically appends the count to the end of the segment files.
--
-- For example, suppose you\'re transcoding into srt format. When you enter
-- \"Sydney-{language}-sunrise\", and the language of the captions is
-- English (en), the name of the first caption file is be
-- Sydney-en-sunrise00000.srt.
captionFormat_pattern :: Lens.Lens' CaptionFormat (Core.Maybe Core.Text)
captionFormat_pattern = Lens.lens (\CaptionFormat' {pattern'} -> pattern') (\s@CaptionFormat' {} a -> s {pattern' = a} :: CaptionFormat)

instance Core.FromJSON CaptionFormat where
  parseJSON =
    Core.withObject
      "CaptionFormat"
      ( \x ->
          CaptionFormat'
            Core.<$> (x Core..:? "Format")
            Core.<*> (x Core..:? "Encryption")
            Core.<*> (x Core..:? "Pattern")
      )

instance Core.Hashable CaptionFormat

instance Core.NFData CaptionFormat

instance Core.ToJSON CaptionFormat where
  toJSON CaptionFormat' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Format" Core..=) Core.<$> format,
            ("Encryption" Core..=) Core.<$> encryption,
            ("Pattern" Core..=) Core.<$> pattern'
          ]
      )
