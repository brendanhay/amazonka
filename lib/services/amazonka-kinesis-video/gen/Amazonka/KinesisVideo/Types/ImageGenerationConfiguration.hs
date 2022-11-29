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
-- Module      : Amazonka.KinesisVideo.Types.ImageGenerationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.ImageGenerationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisVideo.Types.ConfigurationStatus
import Amazonka.KinesisVideo.Types.Format
import Amazonka.KinesisVideo.Types.FormatConfigKey
import Amazonka.KinesisVideo.Types.ImageGenerationDestinationConfig
import Amazonka.KinesisVideo.Types.ImageSelectorType
import qualified Amazonka.Prelude as Prelude

-- | The structure that contains the information required for the KVS images
-- delivery. If null, the configuration will be deleted from the stream.
--
-- /See:/ 'newImageGenerationConfiguration' smart constructor.
data ImageGenerationConfiguration = ImageGenerationConfiguration'
  { -- | The list of a key-value pair structure that contains extra parameters
    -- that can be applied when the image is generated. The @FormatConfig@ key
    -- is the @JPEGQuality@, which indicates the JPEG quality key to be used to
    -- generate the image. The @FormatConfig@ value accepts ints from 1 to 100.
    -- If the value is 1, the image will be generated with less quality and the
    -- best compression. If the value is 100, the image will be generated with
    -- the best quality and less compression. If no value is provided, the
    -- default value of the @JPEGQuality@ key will be set to 80.
    formatConfig :: Prelude.Maybe (Prelude.HashMap FormatConfigKey Prelude.Text),
    -- | The height of the output image that is used in conjunction with the
    -- @WidthPixels@ parameter. When both @HeightPixels@ and @WidthPixels@
    -- parameters are provided, the image will be stretched to fit the
    -- specified aspect ratio. If only the @HeightPixels@ parameter is
    -- provided, its original aspect ratio will be used to calculate the
    -- @WidthPixels@ ratio. If neither parameter is provided, the original
    -- image size will be returned.
    heightPixels :: Prelude.Maybe Prelude.Natural,
    -- | The width of the output image that is used in conjunction with the
    -- @HeightPixels@ parameter. When both @WidthPixels@ and @HeightPixels@
    -- parameters are provided, the image will be stretched to fit the
    -- specified aspect ratio. If only the @WidthPixels@ parameter is provided,
    -- its original aspect ratio will be used to calculate the @HeightPixels@
    -- ratio. If neither parameter is provided, the original image size will be
    -- returned.
    widthPixels :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether the @ContinuousImageGenerationConfigurations@ API is
    -- enabled or disabled.
    status :: ConfigurationStatus,
    -- | The origin of the Server or Producer timestamps to use to generate the
    -- images.
    imageSelectorType :: ImageSelectorType,
    -- | The structure that contains the information required to deliver images
    -- to a customer.
    destinationConfig :: ImageGenerationDestinationConfig,
    -- | The time interval in milliseconds (ms) at which the images need to be
    -- generated from the stream. The minimum value that can be provided is 33
    -- ms, because a camera that generates content at 30 FPS would create a
    -- frame every 33.3 ms. If the timestamp range is less than the sampling
    -- interval, the Image from the @StartTimestamp@ will be returned if
    -- available.
    samplingInterval :: Prelude.Natural,
    -- | The accepted image format.
    format :: Format
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageGenerationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formatConfig', 'imageGenerationConfiguration_formatConfig' - The list of a key-value pair structure that contains extra parameters
-- that can be applied when the image is generated. The @FormatConfig@ key
-- is the @JPEGQuality@, which indicates the JPEG quality key to be used to
-- generate the image. The @FormatConfig@ value accepts ints from 1 to 100.
-- If the value is 1, the image will be generated with less quality and the
-- best compression. If the value is 100, the image will be generated with
-- the best quality and less compression. If no value is provided, the
-- default value of the @JPEGQuality@ key will be set to 80.
--
-- 'heightPixels', 'imageGenerationConfiguration_heightPixels' - The height of the output image that is used in conjunction with the
-- @WidthPixels@ parameter. When both @HeightPixels@ and @WidthPixels@
-- parameters are provided, the image will be stretched to fit the
-- specified aspect ratio. If only the @HeightPixels@ parameter is
-- provided, its original aspect ratio will be used to calculate the
-- @WidthPixels@ ratio. If neither parameter is provided, the original
-- image size will be returned.
--
-- 'widthPixels', 'imageGenerationConfiguration_widthPixels' - The width of the output image that is used in conjunction with the
-- @HeightPixels@ parameter. When both @WidthPixels@ and @HeightPixels@
-- parameters are provided, the image will be stretched to fit the
-- specified aspect ratio. If only the @WidthPixels@ parameter is provided,
-- its original aspect ratio will be used to calculate the @HeightPixels@
-- ratio. If neither parameter is provided, the original image size will be
-- returned.
--
-- 'status', 'imageGenerationConfiguration_status' - Indicates whether the @ContinuousImageGenerationConfigurations@ API is
-- enabled or disabled.
--
-- 'imageSelectorType', 'imageGenerationConfiguration_imageSelectorType' - The origin of the Server or Producer timestamps to use to generate the
-- images.
--
-- 'destinationConfig', 'imageGenerationConfiguration_destinationConfig' - The structure that contains the information required to deliver images
-- to a customer.
--
-- 'samplingInterval', 'imageGenerationConfiguration_samplingInterval' - The time interval in milliseconds (ms) at which the images need to be
-- generated from the stream. The minimum value that can be provided is 33
-- ms, because a camera that generates content at 30 FPS would create a
-- frame every 33.3 ms. If the timestamp range is less than the sampling
-- interval, the Image from the @StartTimestamp@ will be returned if
-- available.
--
-- 'format', 'imageGenerationConfiguration_format' - The accepted image format.
newImageGenerationConfiguration ::
  -- | 'status'
  ConfigurationStatus ->
  -- | 'imageSelectorType'
  ImageSelectorType ->
  -- | 'destinationConfig'
  ImageGenerationDestinationConfig ->
  -- | 'samplingInterval'
  Prelude.Natural ->
  -- | 'format'
  Format ->
  ImageGenerationConfiguration
newImageGenerationConfiguration
  pStatus_
  pImageSelectorType_
  pDestinationConfig_
  pSamplingInterval_
  pFormat_ =
    ImageGenerationConfiguration'
      { formatConfig =
          Prelude.Nothing,
        heightPixels = Prelude.Nothing,
        widthPixels = Prelude.Nothing,
        status = pStatus_,
        imageSelectorType = pImageSelectorType_,
        destinationConfig = pDestinationConfig_,
        samplingInterval = pSamplingInterval_,
        format = pFormat_
      }

-- | The list of a key-value pair structure that contains extra parameters
-- that can be applied when the image is generated. The @FormatConfig@ key
-- is the @JPEGQuality@, which indicates the JPEG quality key to be used to
-- generate the image. The @FormatConfig@ value accepts ints from 1 to 100.
-- If the value is 1, the image will be generated with less quality and the
-- best compression. If the value is 100, the image will be generated with
-- the best quality and less compression. If no value is provided, the
-- default value of the @JPEGQuality@ key will be set to 80.
imageGenerationConfiguration_formatConfig :: Lens.Lens' ImageGenerationConfiguration (Prelude.Maybe (Prelude.HashMap FormatConfigKey Prelude.Text))
imageGenerationConfiguration_formatConfig = Lens.lens (\ImageGenerationConfiguration' {formatConfig} -> formatConfig) (\s@ImageGenerationConfiguration' {} a -> s {formatConfig = a} :: ImageGenerationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The height of the output image that is used in conjunction with the
-- @WidthPixels@ parameter. When both @HeightPixels@ and @WidthPixels@
-- parameters are provided, the image will be stretched to fit the
-- specified aspect ratio. If only the @HeightPixels@ parameter is
-- provided, its original aspect ratio will be used to calculate the
-- @WidthPixels@ ratio. If neither parameter is provided, the original
-- image size will be returned.
imageGenerationConfiguration_heightPixels :: Lens.Lens' ImageGenerationConfiguration (Prelude.Maybe Prelude.Natural)
imageGenerationConfiguration_heightPixels = Lens.lens (\ImageGenerationConfiguration' {heightPixels} -> heightPixels) (\s@ImageGenerationConfiguration' {} a -> s {heightPixels = a} :: ImageGenerationConfiguration)

-- | The width of the output image that is used in conjunction with the
-- @HeightPixels@ parameter. When both @WidthPixels@ and @HeightPixels@
-- parameters are provided, the image will be stretched to fit the
-- specified aspect ratio. If only the @WidthPixels@ parameter is provided,
-- its original aspect ratio will be used to calculate the @HeightPixels@
-- ratio. If neither parameter is provided, the original image size will be
-- returned.
imageGenerationConfiguration_widthPixels :: Lens.Lens' ImageGenerationConfiguration (Prelude.Maybe Prelude.Natural)
imageGenerationConfiguration_widthPixels = Lens.lens (\ImageGenerationConfiguration' {widthPixels} -> widthPixels) (\s@ImageGenerationConfiguration' {} a -> s {widthPixels = a} :: ImageGenerationConfiguration)

-- | Indicates whether the @ContinuousImageGenerationConfigurations@ API is
-- enabled or disabled.
imageGenerationConfiguration_status :: Lens.Lens' ImageGenerationConfiguration ConfigurationStatus
imageGenerationConfiguration_status = Lens.lens (\ImageGenerationConfiguration' {status} -> status) (\s@ImageGenerationConfiguration' {} a -> s {status = a} :: ImageGenerationConfiguration)

-- | The origin of the Server or Producer timestamps to use to generate the
-- images.
imageGenerationConfiguration_imageSelectorType :: Lens.Lens' ImageGenerationConfiguration ImageSelectorType
imageGenerationConfiguration_imageSelectorType = Lens.lens (\ImageGenerationConfiguration' {imageSelectorType} -> imageSelectorType) (\s@ImageGenerationConfiguration' {} a -> s {imageSelectorType = a} :: ImageGenerationConfiguration)

-- | The structure that contains the information required to deliver images
-- to a customer.
imageGenerationConfiguration_destinationConfig :: Lens.Lens' ImageGenerationConfiguration ImageGenerationDestinationConfig
imageGenerationConfiguration_destinationConfig = Lens.lens (\ImageGenerationConfiguration' {destinationConfig} -> destinationConfig) (\s@ImageGenerationConfiguration' {} a -> s {destinationConfig = a} :: ImageGenerationConfiguration)

-- | The time interval in milliseconds (ms) at which the images need to be
-- generated from the stream. The minimum value that can be provided is 33
-- ms, because a camera that generates content at 30 FPS would create a
-- frame every 33.3 ms. If the timestamp range is less than the sampling
-- interval, the Image from the @StartTimestamp@ will be returned if
-- available.
imageGenerationConfiguration_samplingInterval :: Lens.Lens' ImageGenerationConfiguration Prelude.Natural
imageGenerationConfiguration_samplingInterval = Lens.lens (\ImageGenerationConfiguration' {samplingInterval} -> samplingInterval) (\s@ImageGenerationConfiguration' {} a -> s {samplingInterval = a} :: ImageGenerationConfiguration)

-- | The accepted image format.
imageGenerationConfiguration_format :: Lens.Lens' ImageGenerationConfiguration Format
imageGenerationConfiguration_format = Lens.lens (\ImageGenerationConfiguration' {format} -> format) (\s@ImageGenerationConfiguration' {} a -> s {format = a} :: ImageGenerationConfiguration)

instance Core.FromJSON ImageGenerationConfiguration where
  parseJSON =
    Core.withObject
      "ImageGenerationConfiguration"
      ( \x ->
          ImageGenerationConfiguration'
            Prelude.<$> (x Core..:? "FormatConfig" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "HeightPixels")
            Prelude.<*> (x Core..:? "WidthPixels")
            Prelude.<*> (x Core..: "Status")
            Prelude.<*> (x Core..: "ImageSelectorType")
            Prelude.<*> (x Core..: "DestinationConfig")
            Prelude.<*> (x Core..: "SamplingInterval")
            Prelude.<*> (x Core..: "Format")
      )

instance
  Prelude.Hashable
    ImageGenerationConfiguration
  where
  hashWithSalt _salt ImageGenerationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` formatConfig
      `Prelude.hashWithSalt` heightPixels
      `Prelude.hashWithSalt` widthPixels
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` imageSelectorType
      `Prelude.hashWithSalt` destinationConfig
      `Prelude.hashWithSalt` samplingInterval
      `Prelude.hashWithSalt` format

instance Prelude.NFData ImageGenerationConfiguration where
  rnf ImageGenerationConfiguration' {..} =
    Prelude.rnf formatConfig
      `Prelude.seq` Prelude.rnf heightPixels
      `Prelude.seq` Prelude.rnf widthPixels
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf imageSelectorType
      `Prelude.seq` Prelude.rnf destinationConfig
      `Prelude.seq` Prelude.rnf samplingInterval
      `Prelude.seq` Prelude.rnf format

instance Core.ToJSON ImageGenerationConfiguration where
  toJSON ImageGenerationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FormatConfig" Core..=) Prelude.<$> formatConfig,
            ("HeightPixels" Core..=) Prelude.<$> heightPixels,
            ("WidthPixels" Core..=) Prelude.<$> widthPixels,
            Prelude.Just ("Status" Core..= status),
            Prelude.Just
              ("ImageSelectorType" Core..= imageSelectorType),
            Prelude.Just
              ("DestinationConfig" Core..= destinationConfig),
            Prelude.Just
              ("SamplingInterval" Core..= samplingInterval),
            Prelude.Just ("Format" Core..= format)
          ]
      )
