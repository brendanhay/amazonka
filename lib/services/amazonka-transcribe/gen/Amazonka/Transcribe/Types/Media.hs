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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
-- /See:/ 'newMedia' smart constructor.
data Media = Media'
  { -- | The Amazon S3 location of the media file you want to redact. For
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
    -- @RedactedMediaFileUri@ is only supported for Call Analytics
    -- (@StartCallAnalyticsJob@) transcription requests.
    redactedMediaFileUri :: Prelude.Maybe Prelude.Text,
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
    mediaFileUri :: Prelude.Maybe Prelude.Text
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
-- @RedactedMediaFileUri@ is only supported for Call Analytics
-- (@StartCallAnalyticsJob@) transcription requests.
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
newMedia ::
  Media
newMedia =
  Media'
    { redactedMediaFileUri = Prelude.Nothing,
      mediaFileUri = Prelude.Nothing
    }

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
-- @RedactedMediaFileUri@ is only supported for Call Analytics
-- (@StartCallAnalyticsJob@) transcription requests.
media_redactedMediaFileUri :: Lens.Lens' Media (Prelude.Maybe Prelude.Text)
media_redactedMediaFileUri = Lens.lens (\Media' {redactedMediaFileUri} -> redactedMediaFileUri) (\s@Media' {} a -> s {redactedMediaFileUri = a} :: Media)

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

instance Data.FromJSON Media where
  parseJSON =
    Data.withObject
      "Media"
      ( \x ->
          Media'
            Prelude.<$> (x Data..:? "RedactedMediaFileUri")
            Prelude.<*> (x Data..:? "MediaFileUri")
      )

instance Prelude.Hashable Media where
  hashWithSalt _salt Media' {..} =
    _salt `Prelude.hashWithSalt` redactedMediaFileUri
      `Prelude.hashWithSalt` mediaFileUri

instance Prelude.NFData Media where
  rnf Media' {..} =
    Prelude.rnf redactedMediaFileUri
      `Prelude.seq` Prelude.rnf mediaFileUri

instance Data.ToJSON Media where
  toJSON Media' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RedactedMediaFileUri" Data..=)
              Prelude.<$> redactedMediaFileUri,
            ("MediaFileUri" Data..=) Prelude.<$> mediaFileUri
          ]
      )
