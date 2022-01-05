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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.Media where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the input media file in a transcription request.
--
-- /See:/ 'newMedia' smart constructor.
data Media = Media'
  { -- | The S3 object location of the input media file. The URI must be in the
    -- same region as the API endpoint that you are calling. The general form
    -- is:
    --
    -- For example:
    --
    -- For more information about S3 object names, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
    -- in the /Amazon S3 Developer Guide/.
    mediaFileUri :: Prelude.Maybe Prelude.Text,
    -- | The S3 object location for your redacted output media file. This is only
    -- supported for call analytics jobs.
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
-- 'mediaFileUri', 'media_mediaFileUri' - The S3 object location of the input media file. The URI must be in the
-- same region as the API endpoint that you are calling. The general form
-- is:
--
-- For example:
--
-- For more information about S3 object names, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
-- in the /Amazon S3 Developer Guide/.
--
-- 'redactedMediaFileUri', 'media_redactedMediaFileUri' - The S3 object location for your redacted output media file. This is only
-- supported for call analytics jobs.
newMedia ::
  Media
newMedia =
  Media'
    { mediaFileUri = Prelude.Nothing,
      redactedMediaFileUri = Prelude.Nothing
    }

-- | The S3 object location of the input media file. The URI must be in the
-- same region as the API endpoint that you are calling. The general form
-- is:
--
-- For example:
--
-- For more information about S3 object names, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
-- in the /Amazon S3 Developer Guide/.
media_mediaFileUri :: Lens.Lens' Media (Prelude.Maybe Prelude.Text)
media_mediaFileUri = Lens.lens (\Media' {mediaFileUri} -> mediaFileUri) (\s@Media' {} a -> s {mediaFileUri = a} :: Media)

-- | The S3 object location for your redacted output media file. This is only
-- supported for call analytics jobs.
media_redactedMediaFileUri :: Lens.Lens' Media (Prelude.Maybe Prelude.Text)
media_redactedMediaFileUri = Lens.lens (\Media' {redactedMediaFileUri} -> redactedMediaFileUri) (\s@Media' {} a -> s {redactedMediaFileUri = a} :: Media)

instance Core.FromJSON Media where
  parseJSON =
    Core.withObject
      "Media"
      ( \x ->
          Media'
            Prelude.<$> (x Core..:? "MediaFileUri")
            Prelude.<*> (x Core..:? "RedactedMediaFileUri")
      )

instance Prelude.Hashable Media where
  hashWithSalt _salt Media' {..} =
    _salt `Prelude.hashWithSalt` mediaFileUri
      `Prelude.hashWithSalt` redactedMediaFileUri

instance Prelude.NFData Media where
  rnf Media' {..} =
    Prelude.rnf mediaFileUri
      `Prelude.seq` Prelude.rnf redactedMediaFileUri

instance Core.ToJSON Media where
  toJSON Media' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MediaFileUri" Core..=) Prelude.<$> mediaFileUri,
            ("RedactedMediaFileUri" Core..=)
              Prelude.<$> redactedMediaFileUri
          ]
      )
