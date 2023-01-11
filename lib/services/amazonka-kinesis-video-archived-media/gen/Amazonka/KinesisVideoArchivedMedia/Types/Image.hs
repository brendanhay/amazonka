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
-- Module      : Amazonka.KinesisVideoArchivedMedia.Types.Image
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoArchivedMedia.Types.Image where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideoArchivedMedia.Types.ImageError
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the @Timestamp@, @Error@, and @ImageContent@.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | The error message shown when the image for the provided timestamp was
    -- not extracted due to a non-tryable error. An error will be returned if:
    --
    -- -   There is no media that exists for the specified @Timestamp@.
    --
    -- -   The media for the specified time does not allow an image to be
    --     extracted. In this case the media is audio only, or the incorrect
    --     media has been ingested.
    error :: Prelude.Maybe ImageError,
    -- | An attribute of the @Image@ object that is Base64 encoded.
    imageContent :: Prelude.Maybe Prelude.Text,
    -- | An attribute of the @Image@ object that is used to extract an image from
    -- the video stream. This field is used to manage gaps on images or to
    -- better understand the pagination window.
    timeStamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Image' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'image_error' - The error message shown when the image for the provided timestamp was
-- not extracted due to a non-tryable error. An error will be returned if:
--
-- -   There is no media that exists for the specified @Timestamp@.
--
-- -   The media for the specified time does not allow an image to be
--     extracted. In this case the media is audio only, or the incorrect
--     media has been ingested.
--
-- 'imageContent', 'image_imageContent' - An attribute of the @Image@ object that is Base64 encoded.
--
-- 'timeStamp', 'image_timeStamp' - An attribute of the @Image@ object that is used to extract an image from
-- the video stream. This field is used to manage gaps on images or to
-- better understand the pagination window.
newImage ::
  Image
newImage =
  Image'
    { error = Prelude.Nothing,
      imageContent = Prelude.Nothing,
      timeStamp = Prelude.Nothing
    }

-- | The error message shown when the image for the provided timestamp was
-- not extracted due to a non-tryable error. An error will be returned if:
--
-- -   There is no media that exists for the specified @Timestamp@.
--
-- -   The media for the specified time does not allow an image to be
--     extracted. In this case the media is audio only, or the incorrect
--     media has been ingested.
image_error :: Lens.Lens' Image (Prelude.Maybe ImageError)
image_error = Lens.lens (\Image' {error} -> error) (\s@Image' {} a -> s {error = a} :: Image)

-- | An attribute of the @Image@ object that is Base64 encoded.
image_imageContent :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_imageContent = Lens.lens (\Image' {imageContent} -> imageContent) (\s@Image' {} a -> s {imageContent = a} :: Image)

-- | An attribute of the @Image@ object that is used to extract an image from
-- the video stream. This field is used to manage gaps on images or to
-- better understand the pagination window.
image_timeStamp :: Lens.Lens' Image (Prelude.Maybe Prelude.UTCTime)
image_timeStamp = Lens.lens (\Image' {timeStamp} -> timeStamp) (\s@Image' {} a -> s {timeStamp = a} :: Image) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Image where
  parseJSON =
    Data.withObject
      "Image"
      ( \x ->
          Image'
            Prelude.<$> (x Data..:? "Error")
            Prelude.<*> (x Data..:? "ImageContent")
            Prelude.<*> (x Data..:? "TimeStamp")
      )

instance Prelude.Hashable Image where
  hashWithSalt _salt Image' {..} =
    _salt `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` imageContent
      `Prelude.hashWithSalt` timeStamp

instance Prelude.NFData Image where
  rnf Image' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf imageContent
      `Prelude.seq` Prelude.rnf timeStamp
