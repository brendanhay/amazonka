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
-- Module      : Amazonka.Transcribe.Types.Media
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.Media where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the Amazon S3 location of the media file you want to use in
-- your request.
--
-- For information on supported media formats, refer to the
-- <https://docs.aws.amazon.com/APIReference/API_StartTranscriptionJob.html#transcribe-StartTranscriptionJob-request-MediaFormat MediaFormat>
-- parameter or the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/how-input.html#how-input-audio Media formats>
-- section in the Amazon S3 Developer Guide.
--
-- /See:/ 'newMedia' smart constructor.
data Media = Media'
  { -- | The Amazon S3 location of the media file you want to transcribe. For
    -- example:
    --
    -- -   @s3:\/\/DOC-EXAMPLE-BUCKET\/my-media-file.flac@
    --
    -- -   @s3:\/\/DOC-EXAMPLE-BUCKET\/media-files\/my-media-file.flac@
    --
    -- Note that the Amazon S3 bucket that contains your input media must be
    -- located in the same Amazon Web Services Region where you\'re making your
    -- transcription request.
    mediaFileUri :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location of the media file you want to redact. For
    -- example:
    --
    -- -   @s3:\/\/DOC-EXAMPLE-BUCKET\/my-media-file.flac@
    --
    -- -   @s3:\/\/DOC-EXAMPLE-BUCKET\/media-files\/my-media-file.flac@
    --
    -- Note that the Amazon S3 bucket that contains your input media must be
    -- located in the same Amazon Web Services Region where you\'re making your
    -- transcription request.
    --
    -- @RedactedMediaFileUri@ produces a redacted audio file in addition to a
    -- redacted transcript. It is only supported for Call Analytics
    -- (@StartCallAnalyticsJob@) transcription requests.
    redactedMediaFileUri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Media' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaFileUri', 'media_mediaFileUri' - The Amazon S3 location of the media file you want to transcribe. For
-- example:
--
-- -   @s3:\/\/DOC-EXAMPLE-BUCKET\/my-media-file.flac@
--
-- -   @s3:\/\/DOC-EXAMPLE-BUCKET\/media-files\/my-media-file.flac@
--
-- Note that the Amazon S3 bucket that contains your input media must be
-- located in the same Amazon Web Services Region where you\'re making your
-- transcription request.
--
-- 'redactedMediaFileUri', 'media_redactedMediaFileUri' - The Amazon S3 location of the media file you want to redact. For
-- example:
--
-- -   @s3:\/\/DOC-EXAMPLE-BUCKET\/my-media-file.flac@
--
-- -   @s3:\/\/DOC-EXAMPLE-BUCKET\/media-files\/my-media-file.flac@
--
-- Note that the Amazon S3 bucket that contains your input media must be
-- located in the same Amazon Web Services Region where you\'re making your
-- transcription request.
--
-- @RedactedMediaFileUri@ produces a redacted audio file in addition to a
-- redacted transcript. It is only supported for Call Analytics
-- (@StartCallAnalyticsJob@) transcription requests.
newMedia ::
  Media
newMedia =
  Media'
    { mediaFileUri = Prelude.Nothing,
      redactedMediaFileUri = Prelude.Nothing
    }

-- | The Amazon S3 location of the media file you want to transcribe. For
-- example:
--
-- -   @s3:\/\/DOC-EXAMPLE-BUCKET\/my-media-file.flac@
--
-- -   @s3:\/\/DOC-EXAMPLE-BUCKET\/media-files\/my-media-file.flac@
--
-- Note that the Amazon S3 bucket that contains your input media must be
-- located in the same Amazon Web Services Region where you\'re making your
-- transcription request.
media_mediaFileUri :: Lens.Lens' Media (Prelude.Maybe Prelude.Text)
media_mediaFileUri = Lens.lens (\Media' {mediaFileUri} -> mediaFileUri) (\s@Media' {} a -> s {mediaFileUri = a} :: Media)

-- | The Amazon S3 location of the media file you want to redact. For
-- example:
--
-- -   @s3:\/\/DOC-EXAMPLE-BUCKET\/my-media-file.flac@
--
-- -   @s3:\/\/DOC-EXAMPLE-BUCKET\/media-files\/my-media-file.flac@
--
-- Note that the Amazon S3 bucket that contains your input media must be
-- located in the same Amazon Web Services Region where you\'re making your
-- transcription request.
--
-- @RedactedMediaFileUri@ produces a redacted audio file in addition to a
-- redacted transcript. It is only supported for Call Analytics
-- (@StartCallAnalyticsJob@) transcription requests.
media_redactedMediaFileUri :: Lens.Lens' Media (Prelude.Maybe Prelude.Text)
media_redactedMediaFileUri = Lens.lens (\Media' {redactedMediaFileUri} -> redactedMediaFileUri) (\s@Media' {} a -> s {redactedMediaFileUri = a} :: Media)

instance Data.FromJSON Media where
  parseJSON =
    Data.withObject
      "Media"
      ( \x ->
          Media'
            Prelude.<$> (x Data..:? "MediaFileUri")
            Prelude.<*> (x Data..:? "RedactedMediaFileUri")
      )

instance Prelude.Hashable Media where
  hashWithSalt _salt Media' {..} =
    _salt
      `Prelude.hashWithSalt` mediaFileUri
      `Prelude.hashWithSalt` redactedMediaFileUri

instance Prelude.NFData Media where
  rnf Media' {..} =
    Prelude.rnf mediaFileUri
      `Prelude.seq` Prelude.rnf redactedMediaFileUri

instance Data.ToJSON Media where
  toJSON Media' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MediaFileUri" Data..=) Prelude.<$> mediaFileUri,
            ("RedactedMediaFileUri" Data..=)
              Prelude.<$> redactedMediaFileUri
          ]
      )
