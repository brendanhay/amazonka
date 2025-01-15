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
-- Module      : Amazonka.KinesisVideo.Types.ImageGenerationDestinationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.ImageGenerationDestinationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The structure that contains the information required to deliver images
-- to a customer.
--
-- /See:/ 'newImageGenerationDestinationConfig' smart constructor.
data ImageGenerationDestinationConfig = ImageGenerationDestinationConfig'
  { -- | The Uniform Resource Identifier (URI) that identifies where the images
    -- will be delivered.
    uri :: Prelude.Text,
    -- | The AWS Region of the S3 bucket where images will be delivered. This
    -- @DestinationRegion@ must match the Region where the stream is located.
    destinationRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageGenerationDestinationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uri', 'imageGenerationDestinationConfig_uri' - The Uniform Resource Identifier (URI) that identifies where the images
-- will be delivered.
--
-- 'destinationRegion', 'imageGenerationDestinationConfig_destinationRegion' - The AWS Region of the S3 bucket where images will be delivered. This
-- @DestinationRegion@ must match the Region where the stream is located.
newImageGenerationDestinationConfig ::
  -- | 'uri'
  Prelude.Text ->
  -- | 'destinationRegion'
  Prelude.Text ->
  ImageGenerationDestinationConfig
newImageGenerationDestinationConfig
  pUri_
  pDestinationRegion_ =
    ImageGenerationDestinationConfig'
      { uri = pUri_,
        destinationRegion = pDestinationRegion_
      }

-- | The Uniform Resource Identifier (URI) that identifies where the images
-- will be delivered.
imageGenerationDestinationConfig_uri :: Lens.Lens' ImageGenerationDestinationConfig Prelude.Text
imageGenerationDestinationConfig_uri = Lens.lens (\ImageGenerationDestinationConfig' {uri} -> uri) (\s@ImageGenerationDestinationConfig' {} a -> s {uri = a} :: ImageGenerationDestinationConfig)

-- | The AWS Region of the S3 bucket where images will be delivered. This
-- @DestinationRegion@ must match the Region where the stream is located.
imageGenerationDestinationConfig_destinationRegion :: Lens.Lens' ImageGenerationDestinationConfig Prelude.Text
imageGenerationDestinationConfig_destinationRegion = Lens.lens (\ImageGenerationDestinationConfig' {destinationRegion} -> destinationRegion) (\s@ImageGenerationDestinationConfig' {} a -> s {destinationRegion = a} :: ImageGenerationDestinationConfig)

instance
  Data.FromJSON
    ImageGenerationDestinationConfig
  where
  parseJSON =
    Data.withObject
      "ImageGenerationDestinationConfig"
      ( \x ->
          ImageGenerationDestinationConfig'
            Prelude.<$> (x Data..: "Uri")
            Prelude.<*> (x Data..: "DestinationRegion")
      )

instance
  Prelude.Hashable
    ImageGenerationDestinationConfig
  where
  hashWithSalt
    _salt
    ImageGenerationDestinationConfig' {..} =
      _salt
        `Prelude.hashWithSalt` uri
        `Prelude.hashWithSalt` destinationRegion

instance
  Prelude.NFData
    ImageGenerationDestinationConfig
  where
  rnf ImageGenerationDestinationConfig' {..} =
    Prelude.rnf uri `Prelude.seq`
      Prelude.rnf destinationRegion

instance Data.ToJSON ImageGenerationDestinationConfig where
  toJSON ImageGenerationDestinationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Uri" Data..= uri),
            Prelude.Just
              ("DestinationRegion" Data..= destinationRegion)
          ]
      )
