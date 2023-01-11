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
-- Module      : Amazonka.KinesisVideo.UpdateImageGenerationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the @StreamInfo@ and @ImageProcessingConfiguration@ fields.
module Amazonka.KinesisVideo.UpdateImageGenerationConfiguration
  ( -- * Creating a Request
    UpdateImageGenerationConfiguration (..),
    newUpdateImageGenerationConfiguration,

    -- * Request Lenses
    updateImageGenerationConfiguration_imageGenerationConfiguration,
    updateImageGenerationConfiguration_streamARN,
    updateImageGenerationConfiguration_streamName,

    -- * Destructuring the Response
    UpdateImageGenerationConfigurationResponse (..),
    newUpdateImageGenerationConfigurationResponse,

    -- * Response Lenses
    updateImageGenerationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateImageGenerationConfiguration' smart constructor.
data UpdateImageGenerationConfiguration = UpdateImageGenerationConfiguration'
  { -- | The structure that contains the information required for the KVS images
    -- delivery. If the structure is null, the configuration will be deleted
    -- from the stream.
    imageGenerationConfiguration :: Prelude.Maybe ImageGenerationConfiguration,
    -- | The Amazon Resource Name (ARN) of the Kinesis video stream from where
    -- you want to update the image generation configuration. You must specify
    -- either the @StreamName@ or the @StreamARN@.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream from which to update the image generation
    -- configuration. You must specify either the @StreamName@ or the
    -- @StreamARN@.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateImageGenerationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageGenerationConfiguration', 'updateImageGenerationConfiguration_imageGenerationConfiguration' - The structure that contains the information required for the KVS images
-- delivery. If the structure is null, the configuration will be deleted
-- from the stream.
--
-- 'streamARN', 'updateImageGenerationConfiguration_streamARN' - The Amazon Resource Name (ARN) of the Kinesis video stream from where
-- you want to update the image generation configuration. You must specify
-- either the @StreamName@ or the @StreamARN@.
--
-- 'streamName', 'updateImageGenerationConfiguration_streamName' - The name of the stream from which to update the image generation
-- configuration. You must specify either the @StreamName@ or the
-- @StreamARN@.
newUpdateImageGenerationConfiguration ::
  UpdateImageGenerationConfiguration
newUpdateImageGenerationConfiguration =
  UpdateImageGenerationConfiguration'
    { imageGenerationConfiguration =
        Prelude.Nothing,
      streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | The structure that contains the information required for the KVS images
-- delivery. If the structure is null, the configuration will be deleted
-- from the stream.
updateImageGenerationConfiguration_imageGenerationConfiguration :: Lens.Lens' UpdateImageGenerationConfiguration (Prelude.Maybe ImageGenerationConfiguration)
updateImageGenerationConfiguration_imageGenerationConfiguration = Lens.lens (\UpdateImageGenerationConfiguration' {imageGenerationConfiguration} -> imageGenerationConfiguration) (\s@UpdateImageGenerationConfiguration' {} a -> s {imageGenerationConfiguration = a} :: UpdateImageGenerationConfiguration)

-- | The Amazon Resource Name (ARN) of the Kinesis video stream from where
-- you want to update the image generation configuration. You must specify
-- either the @StreamName@ or the @StreamARN@.
updateImageGenerationConfiguration_streamARN :: Lens.Lens' UpdateImageGenerationConfiguration (Prelude.Maybe Prelude.Text)
updateImageGenerationConfiguration_streamARN = Lens.lens (\UpdateImageGenerationConfiguration' {streamARN} -> streamARN) (\s@UpdateImageGenerationConfiguration' {} a -> s {streamARN = a} :: UpdateImageGenerationConfiguration)

-- | The name of the stream from which to update the image generation
-- configuration. You must specify either the @StreamName@ or the
-- @StreamARN@.
updateImageGenerationConfiguration_streamName :: Lens.Lens' UpdateImageGenerationConfiguration (Prelude.Maybe Prelude.Text)
updateImageGenerationConfiguration_streamName = Lens.lens (\UpdateImageGenerationConfiguration' {streamName} -> streamName) (\s@UpdateImageGenerationConfiguration' {} a -> s {streamName = a} :: UpdateImageGenerationConfiguration)

instance
  Core.AWSRequest
    UpdateImageGenerationConfiguration
  where
  type
    AWSResponse UpdateImageGenerationConfiguration =
      UpdateImageGenerationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateImageGenerationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateImageGenerationConfiguration
  where
  hashWithSalt
    _salt
    UpdateImageGenerationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` imageGenerationConfiguration
        `Prelude.hashWithSalt` streamARN
        `Prelude.hashWithSalt` streamName

instance
  Prelude.NFData
    UpdateImageGenerationConfiguration
  where
  rnf UpdateImageGenerationConfiguration' {..} =
    Prelude.rnf imageGenerationConfiguration
      `Prelude.seq` Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName

instance
  Data.ToHeaders
    UpdateImageGenerationConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    UpdateImageGenerationConfiguration
  where
  toJSON UpdateImageGenerationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ImageGenerationConfiguration" Data..=)
              Prelude.<$> imageGenerationConfiguration,
            ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName
          ]
      )

instance
  Data.ToPath
    UpdateImageGenerationConfiguration
  where
  toPath =
    Prelude.const "/updateImageGenerationConfiguration"

instance
  Data.ToQuery
    UpdateImageGenerationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateImageGenerationConfigurationResponse' smart constructor.
data UpdateImageGenerationConfigurationResponse = UpdateImageGenerationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateImageGenerationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateImageGenerationConfigurationResponse_httpStatus' - The response's http status code.
newUpdateImageGenerationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateImageGenerationConfigurationResponse
newUpdateImageGenerationConfigurationResponse
  pHttpStatus_ =
    UpdateImageGenerationConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateImageGenerationConfigurationResponse_httpStatus :: Lens.Lens' UpdateImageGenerationConfigurationResponse Prelude.Int
updateImageGenerationConfigurationResponse_httpStatus = Lens.lens (\UpdateImageGenerationConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateImageGenerationConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateImageGenerationConfigurationResponse)

instance
  Prelude.NFData
    UpdateImageGenerationConfigurationResponse
  where
  rnf UpdateImageGenerationConfigurationResponse' {..} =
    Prelude.rnf httpStatus
