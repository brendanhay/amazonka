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
-- Module      : Amazonka.ElasticTranscoder.Types.CaptionFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.CaptionFormat where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types.Encryption
import qualified Amazonka.Prelude as Prelude

-- | The file format of the output captions. If you leave this value blank,
-- Elastic Transcoder returns an error.
--
-- /See:/ 'newCaptionFormat' smart constructor.
data CaptionFormat = CaptionFormat'
  { -- | The encryption settings, if any, that you want Elastic Transcoder to
    -- apply to your caption formats.
    encryption :: Prelude.Maybe Encryption,
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
    format :: Prelude.Maybe Prelude.Text,
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
    pattern' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptionFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryption', 'captionFormat_encryption' - The encryption settings, if any, that you want Elastic Transcoder to
-- apply to your caption formats.
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
    { encryption = Prelude.Nothing,
      format = Prelude.Nothing,
      pattern' = Prelude.Nothing
    }

-- | The encryption settings, if any, that you want Elastic Transcoder to
-- apply to your caption formats.
captionFormat_encryption :: Lens.Lens' CaptionFormat (Prelude.Maybe Encryption)
captionFormat_encryption = Lens.lens (\CaptionFormat' {encryption} -> encryption) (\s@CaptionFormat' {} a -> s {encryption = a} :: CaptionFormat)

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
captionFormat_format :: Lens.Lens' CaptionFormat (Prelude.Maybe Prelude.Text)
captionFormat_format = Lens.lens (\CaptionFormat' {format} -> format) (\s@CaptionFormat' {} a -> s {format = a} :: CaptionFormat)

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
captionFormat_pattern :: Lens.Lens' CaptionFormat (Prelude.Maybe Prelude.Text)
captionFormat_pattern = Lens.lens (\CaptionFormat' {pattern'} -> pattern') (\s@CaptionFormat' {} a -> s {pattern' = a} :: CaptionFormat)

instance Data.FromJSON CaptionFormat where
  parseJSON =
    Data.withObject
      "CaptionFormat"
      ( \x ->
          CaptionFormat'
            Prelude.<$> (x Data..:? "Encryption")
            Prelude.<*> (x Data..:? "Format")
            Prelude.<*> (x Data..:? "Pattern")
      )

instance Prelude.Hashable CaptionFormat where
  hashWithSalt _salt CaptionFormat' {..} =
    _salt
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` pattern'

instance Prelude.NFData CaptionFormat where
  rnf CaptionFormat' {..} =
    Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf pattern'

instance Data.ToJSON CaptionFormat where
  toJSON CaptionFormat' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Encryption" Data..=) Prelude.<$> encryption,
            ("Format" Data..=) Prelude.<$> format,
            ("Pattern" Data..=) Prelude.<$> pattern'
          ]
      )
