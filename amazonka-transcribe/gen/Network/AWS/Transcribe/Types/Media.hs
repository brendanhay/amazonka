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
-- Module      : Network.AWS.Transcribe.Types.Media
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.Media where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    -- <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
    -- in the /Amazon S3 Developer Guide/.
    mediaFileUri :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
-- in the /Amazon S3 Developer Guide/.
newMedia ::
  Media
newMedia = Media' {mediaFileUri = Core.Nothing}

-- | The S3 object location of the input media file. The URI must be in the
-- same region as the API endpoint that you are calling. The general form
-- is:
--
-- For example:
--
-- For more information about S3 object names, see
-- <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys>
-- in the /Amazon S3 Developer Guide/.
media_mediaFileUri :: Lens.Lens' Media (Core.Maybe Core.Text)
media_mediaFileUri = Lens.lens (\Media' {mediaFileUri} -> mediaFileUri) (\s@Media' {} a -> s {mediaFileUri = a} :: Media)

instance Core.FromJSON Media where
  parseJSON =
    Core.withObject
      "Media"
      (\x -> Media' Core.<$> (x Core..:? "MediaFileUri"))

instance Core.Hashable Media

instance Core.NFData Media

instance Core.ToJSON Media where
  toJSON Media' {..} =
    Core.object
      ( Core.catMaybes
          [("MediaFileUri" Core..=) Core.<$> mediaFileUri]
      )
