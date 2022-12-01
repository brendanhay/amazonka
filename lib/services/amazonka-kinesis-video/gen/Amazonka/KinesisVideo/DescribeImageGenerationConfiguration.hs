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
-- Module      : Amazonka.KinesisVideo.DescribeImageGenerationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the @ImageGenerationConfiguration@ for a given Kinesis video
-- stream.
module Amazonka.KinesisVideo.DescribeImageGenerationConfiguration
  ( -- * Creating a Request
    DescribeImageGenerationConfiguration (..),
    newDescribeImageGenerationConfiguration,

    -- * Request Lenses
    describeImageGenerationConfiguration_streamARN,
    describeImageGenerationConfiguration_streamName,

    -- * Destructuring the Response
    DescribeImageGenerationConfigurationResponse (..),
    newDescribeImageGenerationConfigurationResponse,

    -- * Response Lenses
    describeImageGenerationConfigurationResponse_imageGenerationConfiguration,
    describeImageGenerationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeImageGenerationConfiguration' smart constructor.
data DescribeImageGenerationConfiguration = DescribeImageGenerationConfiguration'
  { -- | The Amazon Resource Name (ARN) of the Kinesis video stream from which to
    -- retrieve the image generation configuration. You must specify either the
    -- @StreamName@ or the @StreamARN@.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream from which to retrieve the image generation
    -- configuration. You must specify either the @StreamName@ or the
    -- @StreamARN@.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageGenerationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'describeImageGenerationConfiguration_streamARN' - The Amazon Resource Name (ARN) of the Kinesis video stream from which to
-- retrieve the image generation configuration. You must specify either the
-- @StreamName@ or the @StreamARN@.
--
-- 'streamName', 'describeImageGenerationConfiguration_streamName' - The name of the stream from which to retrieve the image generation
-- configuration. You must specify either the @StreamName@ or the
-- @StreamARN@.
newDescribeImageGenerationConfiguration ::
  DescribeImageGenerationConfiguration
newDescribeImageGenerationConfiguration =
  DescribeImageGenerationConfiguration'
    { streamARN =
        Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Kinesis video stream from which to
-- retrieve the image generation configuration. You must specify either the
-- @StreamName@ or the @StreamARN@.
describeImageGenerationConfiguration_streamARN :: Lens.Lens' DescribeImageGenerationConfiguration (Prelude.Maybe Prelude.Text)
describeImageGenerationConfiguration_streamARN = Lens.lens (\DescribeImageGenerationConfiguration' {streamARN} -> streamARN) (\s@DescribeImageGenerationConfiguration' {} a -> s {streamARN = a} :: DescribeImageGenerationConfiguration)

-- | The name of the stream from which to retrieve the image generation
-- configuration. You must specify either the @StreamName@ or the
-- @StreamARN@.
describeImageGenerationConfiguration_streamName :: Lens.Lens' DescribeImageGenerationConfiguration (Prelude.Maybe Prelude.Text)
describeImageGenerationConfiguration_streamName = Lens.lens (\DescribeImageGenerationConfiguration' {streamName} -> streamName) (\s@DescribeImageGenerationConfiguration' {} a -> s {streamName = a} :: DescribeImageGenerationConfiguration)

instance
  Core.AWSRequest
    DescribeImageGenerationConfiguration
  where
  type
    AWSResponse DescribeImageGenerationConfiguration =
      DescribeImageGenerationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImageGenerationConfigurationResponse'
            Prelude.<$> (x Core..?> "ImageGenerationConfiguration")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeImageGenerationConfiguration
  where
  hashWithSalt
    _salt
    DescribeImageGenerationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` streamARN
        `Prelude.hashWithSalt` streamName

instance
  Prelude.NFData
    DescribeImageGenerationConfiguration
  where
  rnf DescribeImageGenerationConfiguration' {..} =
    Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName

instance
  Core.ToHeaders
    DescribeImageGenerationConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToJSON
    DescribeImageGenerationConfiguration
  where
  toJSON DescribeImageGenerationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StreamARN" Core..=) Prelude.<$> streamARN,
            ("StreamName" Core..=) Prelude.<$> streamName
          ]
      )

instance
  Core.ToPath
    DescribeImageGenerationConfiguration
  where
  toPath =
    Prelude.const
      "/describeImageGenerationConfiguration"

instance
  Core.ToQuery
    DescribeImageGenerationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeImageGenerationConfigurationResponse' smart constructor.
data DescribeImageGenerationConfigurationResponse = DescribeImageGenerationConfigurationResponse'
  { -- | The structure that contains the information required for the Kinesis
    -- video stream (KVS) images delivery. If this structure is null, the
    -- configuration will be deleted from the stream.
    imageGenerationConfiguration :: Prelude.Maybe ImageGenerationConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageGenerationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageGenerationConfiguration', 'describeImageGenerationConfigurationResponse_imageGenerationConfiguration' - The structure that contains the information required for the Kinesis
-- video stream (KVS) images delivery. If this structure is null, the
-- configuration will be deleted from the stream.
--
-- 'httpStatus', 'describeImageGenerationConfigurationResponse_httpStatus' - The response's http status code.
newDescribeImageGenerationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeImageGenerationConfigurationResponse
newDescribeImageGenerationConfigurationResponse
  pHttpStatus_ =
    DescribeImageGenerationConfigurationResponse'
      { imageGenerationConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The structure that contains the information required for the Kinesis
-- video stream (KVS) images delivery. If this structure is null, the
-- configuration will be deleted from the stream.
describeImageGenerationConfigurationResponse_imageGenerationConfiguration :: Lens.Lens' DescribeImageGenerationConfigurationResponse (Prelude.Maybe ImageGenerationConfiguration)
describeImageGenerationConfigurationResponse_imageGenerationConfiguration = Lens.lens (\DescribeImageGenerationConfigurationResponse' {imageGenerationConfiguration} -> imageGenerationConfiguration) (\s@DescribeImageGenerationConfigurationResponse' {} a -> s {imageGenerationConfiguration = a} :: DescribeImageGenerationConfigurationResponse)

-- | The response's http status code.
describeImageGenerationConfigurationResponse_httpStatus :: Lens.Lens' DescribeImageGenerationConfigurationResponse Prelude.Int
describeImageGenerationConfigurationResponse_httpStatus = Lens.lens (\DescribeImageGenerationConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeImageGenerationConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeImageGenerationConfigurationResponse)

instance
  Prelude.NFData
    DescribeImageGenerationConfigurationResponse
  where
  rnf DescribeImageGenerationConfigurationResponse' {..} =
    Prelude.rnf imageGenerationConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
