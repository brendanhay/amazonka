{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KinesisVideoArchivedMedia.GetImages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of Images corresponding to each timestamp for a given
-- time range, sampling interval, and image format configuration.
--
-- This operation returns paginated results.
module Amazonka.KinesisVideoArchivedMedia.GetImages
  ( -- * Creating a Request
    GetImages (..),
    newGetImages,

    -- * Request Lenses
    getImages_nextToken,
    getImages_formatConfig,
    getImages_maxResults,
    getImages_heightPixels,
    getImages_streamARN,
    getImages_widthPixels,
    getImages_streamName,
    getImages_imageSelectorType,
    getImages_startTimestamp,
    getImages_endTimestamp,
    getImages_samplingInterval,
    getImages_format,

    -- * Destructuring the Response
    GetImagesResponse (..),
    newGetImagesResponse,

    -- * Response Lenses
    getImagesResponse_nextToken,
    getImagesResponse_images,
    getImagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisVideoArchivedMedia.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetImages' smart constructor.
data GetImages = GetImages'
  { -- | A token that specifies where to start paginating the next set of Images.
    -- This is the @GetImages:NextToken@ from a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of a key-value pair structure that contains extra parameters
    -- that can be applied when the image is generated. The @FormatConfig@ key
    -- is the @JPEGQuality@, which indicates the JPEG quality key to be used to
    -- generate the image. The @FormatConfig@ value accepts ints from 1 to 100.
    -- If the value is 1, the image will be generated with less quality and the
    -- best compression. If the value is 100, the image will be generated with
    -- the best quality and less compression. If no value is provided, the
    -- default value of the @JPEGQuality@ key will be set to 80.
    formatConfig :: Prelude.Maybe (Prelude.HashMap FormatConfigKey Prelude.Text),
    -- | The maximum number of images to be returned by the API.
    --
    -- The default limit is 100 images per API response. The additional results
    -- will be paginated.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The height of the output image that is used in conjunction with the
    -- @WidthPixels@ parameter. When both @HeightPixels@ and @WidthPixels@
    -- parameters are provided, the image will be stretched to fit the
    -- specified aspect ratio. If only the @HeightPixels@ parameter is
    -- provided, its original aspect ratio will be used to calculate the
    -- @WidthPixels@ ratio. If neither parameter is provided, the original
    -- image size will be returned.
    heightPixels :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the stream from which to retrieve the
    -- images. You must specify either the @StreamName@ or the @StreamARN@.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The width of the output image that is used in conjunction with the
    -- @HeightPixels@ parameter. When both @WidthPixels@ and @HeightPixels@
    -- parameters are provided, the image will be stretched to fit the
    -- specified aspect ratio. If only the @WidthPixels@ parameter is provided
    -- or if only the @HeightPixels@ is provided, a @ValidationException@ will
    -- be thrown. If neither parameter is provided, the original image size
    -- from the stream will be returned.
    widthPixels :: Prelude.Maybe Prelude.Natural,
    -- | The name of the stream from which to retrieve the images. You must
    -- specify either the @StreamName@ or the @StreamARN@.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | The origin of the Server or Producer timestamps to use to generate the
    -- images.
    imageSelectorType :: ImageSelectorType,
    -- | The starting point from which the images should be generated. This
    -- @StartTimestamp@ must be within an inclusive range of timestamps for an
    -- image to be returned.
    startTimestamp :: Core.POSIX,
    -- | The end timestamp for the range of images to be generated.
    endTimestamp :: Core.POSIX,
    -- | The time interval in milliseconds (ms) at which the images need to be
    -- generated from the stream. The minimum value that can be provided is
    -- 3000 ms. If the timestamp range is less than the sampling interval, the
    -- Image from the @startTimestamp@ will be returned if available.
    --
    -- The minimum value of 3000 ms is a soft limit. If needed, a lower
    -- sampling frequency can be requested.
    samplingInterval :: Prelude.Natural,
    -- | The format that will be used to encode the image.
    format :: Format
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getImages_nextToken' - A token that specifies where to start paginating the next set of Images.
-- This is the @GetImages:NextToken@ from a previously truncated response.
--
-- 'formatConfig', 'getImages_formatConfig' - The list of a key-value pair structure that contains extra parameters
-- that can be applied when the image is generated. The @FormatConfig@ key
-- is the @JPEGQuality@, which indicates the JPEG quality key to be used to
-- generate the image. The @FormatConfig@ value accepts ints from 1 to 100.
-- If the value is 1, the image will be generated with less quality and the
-- best compression. If the value is 100, the image will be generated with
-- the best quality and less compression. If no value is provided, the
-- default value of the @JPEGQuality@ key will be set to 80.
--
-- 'maxResults', 'getImages_maxResults' - The maximum number of images to be returned by the API.
--
-- The default limit is 100 images per API response. The additional results
-- will be paginated.
--
-- 'heightPixels', 'getImages_heightPixels' - The height of the output image that is used in conjunction with the
-- @WidthPixels@ parameter. When both @HeightPixels@ and @WidthPixels@
-- parameters are provided, the image will be stretched to fit the
-- specified aspect ratio. If only the @HeightPixels@ parameter is
-- provided, its original aspect ratio will be used to calculate the
-- @WidthPixels@ ratio. If neither parameter is provided, the original
-- image size will be returned.
--
-- 'streamARN', 'getImages_streamARN' - The Amazon Resource Name (ARN) of the stream from which to retrieve the
-- images. You must specify either the @StreamName@ or the @StreamARN@.
--
-- 'widthPixels', 'getImages_widthPixels' - The width of the output image that is used in conjunction with the
-- @HeightPixels@ parameter. When both @WidthPixels@ and @HeightPixels@
-- parameters are provided, the image will be stretched to fit the
-- specified aspect ratio. If only the @WidthPixels@ parameter is provided
-- or if only the @HeightPixels@ is provided, a @ValidationException@ will
-- be thrown. If neither parameter is provided, the original image size
-- from the stream will be returned.
--
-- 'streamName', 'getImages_streamName' - The name of the stream from which to retrieve the images. You must
-- specify either the @StreamName@ or the @StreamARN@.
--
-- 'imageSelectorType', 'getImages_imageSelectorType' - The origin of the Server or Producer timestamps to use to generate the
-- images.
--
-- 'startTimestamp', 'getImages_startTimestamp' - The starting point from which the images should be generated. This
-- @StartTimestamp@ must be within an inclusive range of timestamps for an
-- image to be returned.
--
-- 'endTimestamp', 'getImages_endTimestamp' - The end timestamp for the range of images to be generated.
--
-- 'samplingInterval', 'getImages_samplingInterval' - The time interval in milliseconds (ms) at which the images need to be
-- generated from the stream. The minimum value that can be provided is
-- 3000 ms. If the timestamp range is less than the sampling interval, the
-- Image from the @startTimestamp@ will be returned if available.
--
-- The minimum value of 3000 ms is a soft limit. If needed, a lower
-- sampling frequency can be requested.
--
-- 'format', 'getImages_format' - The format that will be used to encode the image.
newGetImages ::
  -- | 'imageSelectorType'
  ImageSelectorType ->
  -- | 'startTimestamp'
  Prelude.UTCTime ->
  -- | 'endTimestamp'
  Prelude.UTCTime ->
  -- | 'samplingInterval'
  Prelude.Natural ->
  -- | 'format'
  Format ->
  GetImages
newGetImages
  pImageSelectorType_
  pStartTimestamp_
  pEndTimestamp_
  pSamplingInterval_
  pFormat_ =
    GetImages'
      { nextToken = Prelude.Nothing,
        formatConfig = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        heightPixels = Prelude.Nothing,
        streamARN = Prelude.Nothing,
        widthPixels = Prelude.Nothing,
        streamName = Prelude.Nothing,
        imageSelectorType = pImageSelectorType_,
        startTimestamp = Core._Time Lens.# pStartTimestamp_,
        endTimestamp = Core._Time Lens.# pEndTimestamp_,
        samplingInterval = pSamplingInterval_,
        format = pFormat_
      }

-- | A token that specifies where to start paginating the next set of Images.
-- This is the @GetImages:NextToken@ from a previously truncated response.
getImages_nextToken :: Lens.Lens' GetImages (Prelude.Maybe Prelude.Text)
getImages_nextToken = Lens.lens (\GetImages' {nextToken} -> nextToken) (\s@GetImages' {} a -> s {nextToken = a} :: GetImages)

-- | The list of a key-value pair structure that contains extra parameters
-- that can be applied when the image is generated. The @FormatConfig@ key
-- is the @JPEGQuality@, which indicates the JPEG quality key to be used to
-- generate the image. The @FormatConfig@ value accepts ints from 1 to 100.
-- If the value is 1, the image will be generated with less quality and the
-- best compression. If the value is 100, the image will be generated with
-- the best quality and less compression. If no value is provided, the
-- default value of the @JPEGQuality@ key will be set to 80.
getImages_formatConfig :: Lens.Lens' GetImages (Prelude.Maybe (Prelude.HashMap FormatConfigKey Prelude.Text))
getImages_formatConfig = Lens.lens (\GetImages' {formatConfig} -> formatConfig) (\s@GetImages' {} a -> s {formatConfig = a} :: GetImages) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of images to be returned by the API.
--
-- The default limit is 100 images per API response. The additional results
-- will be paginated.
getImages_maxResults :: Lens.Lens' GetImages (Prelude.Maybe Prelude.Natural)
getImages_maxResults = Lens.lens (\GetImages' {maxResults} -> maxResults) (\s@GetImages' {} a -> s {maxResults = a} :: GetImages)

-- | The height of the output image that is used in conjunction with the
-- @WidthPixels@ parameter. When both @HeightPixels@ and @WidthPixels@
-- parameters are provided, the image will be stretched to fit the
-- specified aspect ratio. If only the @HeightPixels@ parameter is
-- provided, its original aspect ratio will be used to calculate the
-- @WidthPixels@ ratio. If neither parameter is provided, the original
-- image size will be returned.
getImages_heightPixels :: Lens.Lens' GetImages (Prelude.Maybe Prelude.Natural)
getImages_heightPixels = Lens.lens (\GetImages' {heightPixels} -> heightPixels) (\s@GetImages' {} a -> s {heightPixels = a} :: GetImages)

-- | The Amazon Resource Name (ARN) of the stream from which to retrieve the
-- images. You must specify either the @StreamName@ or the @StreamARN@.
getImages_streamARN :: Lens.Lens' GetImages (Prelude.Maybe Prelude.Text)
getImages_streamARN = Lens.lens (\GetImages' {streamARN} -> streamARN) (\s@GetImages' {} a -> s {streamARN = a} :: GetImages)

-- | The width of the output image that is used in conjunction with the
-- @HeightPixels@ parameter. When both @WidthPixels@ and @HeightPixels@
-- parameters are provided, the image will be stretched to fit the
-- specified aspect ratio. If only the @WidthPixels@ parameter is provided
-- or if only the @HeightPixels@ is provided, a @ValidationException@ will
-- be thrown. If neither parameter is provided, the original image size
-- from the stream will be returned.
getImages_widthPixels :: Lens.Lens' GetImages (Prelude.Maybe Prelude.Natural)
getImages_widthPixels = Lens.lens (\GetImages' {widthPixels} -> widthPixels) (\s@GetImages' {} a -> s {widthPixels = a} :: GetImages)

-- | The name of the stream from which to retrieve the images. You must
-- specify either the @StreamName@ or the @StreamARN@.
getImages_streamName :: Lens.Lens' GetImages (Prelude.Maybe Prelude.Text)
getImages_streamName = Lens.lens (\GetImages' {streamName} -> streamName) (\s@GetImages' {} a -> s {streamName = a} :: GetImages)

-- | The origin of the Server or Producer timestamps to use to generate the
-- images.
getImages_imageSelectorType :: Lens.Lens' GetImages ImageSelectorType
getImages_imageSelectorType = Lens.lens (\GetImages' {imageSelectorType} -> imageSelectorType) (\s@GetImages' {} a -> s {imageSelectorType = a} :: GetImages)

-- | The starting point from which the images should be generated. This
-- @StartTimestamp@ must be within an inclusive range of timestamps for an
-- image to be returned.
getImages_startTimestamp :: Lens.Lens' GetImages Prelude.UTCTime
getImages_startTimestamp = Lens.lens (\GetImages' {startTimestamp} -> startTimestamp) (\s@GetImages' {} a -> s {startTimestamp = a} :: GetImages) Prelude.. Core._Time

-- | The end timestamp for the range of images to be generated.
getImages_endTimestamp :: Lens.Lens' GetImages Prelude.UTCTime
getImages_endTimestamp = Lens.lens (\GetImages' {endTimestamp} -> endTimestamp) (\s@GetImages' {} a -> s {endTimestamp = a} :: GetImages) Prelude.. Core._Time

-- | The time interval in milliseconds (ms) at which the images need to be
-- generated from the stream. The minimum value that can be provided is
-- 3000 ms. If the timestamp range is less than the sampling interval, the
-- Image from the @startTimestamp@ will be returned if available.
--
-- The minimum value of 3000 ms is a soft limit. If needed, a lower
-- sampling frequency can be requested.
getImages_samplingInterval :: Lens.Lens' GetImages Prelude.Natural
getImages_samplingInterval = Lens.lens (\GetImages' {samplingInterval} -> samplingInterval) (\s@GetImages' {} a -> s {samplingInterval = a} :: GetImages)

-- | The format that will be used to encode the image.
getImages_format :: Lens.Lens' GetImages Format
getImages_format = Lens.lens (\GetImages' {format} -> format) (\s@GetImages' {} a -> s {format = a} :: GetImages)

instance Core.AWSPager GetImages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getImagesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getImagesResponse_images Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getImages_nextToken
          Lens..~ rs
          Lens.^? getImagesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetImages where
  type AWSResponse GetImages = GetImagesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetImagesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Images" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetImages where
  hashWithSalt _salt GetImages' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` formatConfig
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` heightPixels
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` widthPixels
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` imageSelectorType
      `Prelude.hashWithSalt` startTimestamp
      `Prelude.hashWithSalt` endTimestamp
      `Prelude.hashWithSalt` samplingInterval
      `Prelude.hashWithSalt` format

instance Prelude.NFData GetImages where
  rnf GetImages' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf formatConfig
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf heightPixels
      `Prelude.seq` Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf widthPixels
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf imageSelectorType
      `Prelude.seq` Prelude.rnf startTimestamp
      `Prelude.seq` Prelude.rnf endTimestamp
      `Prelude.seq` Prelude.rnf samplingInterval
      `Prelude.seq` Prelude.rnf format

instance Core.ToHeaders GetImages where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON GetImages where
  toJSON GetImages' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("FormatConfig" Core..=) Prelude.<$> formatConfig,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("HeightPixels" Core..=) Prelude.<$> heightPixels,
            ("StreamARN" Core..=) Prelude.<$> streamARN,
            ("WidthPixels" Core..=) Prelude.<$> widthPixels,
            ("StreamName" Core..=) Prelude.<$> streamName,
            Prelude.Just
              ("ImageSelectorType" Core..= imageSelectorType),
            Prelude.Just
              ("StartTimestamp" Core..= startTimestamp),
            Prelude.Just ("EndTimestamp" Core..= endTimestamp),
            Prelude.Just
              ("SamplingInterval" Core..= samplingInterval),
            Prelude.Just ("Format" Core..= format)
          ]
      )

instance Core.ToPath GetImages where
  toPath = Prelude.const "/getImages"

instance Core.ToQuery GetImages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetImagesResponse' smart constructor.
data GetImagesResponse = GetImagesResponse'
  { -- | The encrypted token that was used in the request to get more images.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of images generated from the video stream. If there is no media
    -- available for the given timestamp, the @NO_MEDIA@ error will be listed
    -- in the output. If an error occurs while the image is being generated,
    -- the @MEDIA_ERROR@ will be listed in the output as the cause of the
    -- missing image.
    images :: Prelude.Maybe [Image],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getImagesResponse_nextToken' - The encrypted token that was used in the request to get more images.
--
-- 'images', 'getImagesResponse_images' - The list of images generated from the video stream. If there is no media
-- available for the given timestamp, the @NO_MEDIA@ error will be listed
-- in the output. If an error occurs while the image is being generated,
-- the @MEDIA_ERROR@ will be listed in the output as the cause of the
-- missing image.
--
-- 'httpStatus', 'getImagesResponse_httpStatus' - The response's http status code.
newGetImagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetImagesResponse
newGetImagesResponse pHttpStatus_ =
  GetImagesResponse'
    { nextToken = Prelude.Nothing,
      images = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The encrypted token that was used in the request to get more images.
getImagesResponse_nextToken :: Lens.Lens' GetImagesResponse (Prelude.Maybe Prelude.Text)
getImagesResponse_nextToken = Lens.lens (\GetImagesResponse' {nextToken} -> nextToken) (\s@GetImagesResponse' {} a -> s {nextToken = a} :: GetImagesResponse)

-- | The list of images generated from the video stream. If there is no media
-- available for the given timestamp, the @NO_MEDIA@ error will be listed
-- in the output. If an error occurs while the image is being generated,
-- the @MEDIA_ERROR@ will be listed in the output as the cause of the
-- missing image.
getImagesResponse_images :: Lens.Lens' GetImagesResponse (Prelude.Maybe [Image])
getImagesResponse_images = Lens.lens (\GetImagesResponse' {images} -> images) (\s@GetImagesResponse' {} a -> s {images = a} :: GetImagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getImagesResponse_httpStatus :: Lens.Lens' GetImagesResponse Prelude.Int
getImagesResponse_httpStatus = Lens.lens (\GetImagesResponse' {httpStatus} -> httpStatus) (\s@GetImagesResponse' {} a -> s {httpStatus = a} :: GetImagesResponse)

instance Prelude.NFData GetImagesResponse where
  rnf GetImagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf images
      `Prelude.seq` Prelude.rnf httpStatus
