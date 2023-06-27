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
-- Module      : Amazonka.ChimeSdkMediaPipelines.CreateMediaInsightsPipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a media insights pipeline.
module Amazonka.ChimeSdkMediaPipelines.CreateMediaInsightsPipeline
  ( -- * Creating a Request
    CreateMediaInsightsPipeline (..),
    newCreateMediaInsightsPipeline,

    -- * Request Lenses
    createMediaInsightsPipeline_clientRequestToken,
    createMediaInsightsPipeline_kinesisVideoStreamRecordingSourceRuntimeConfiguration,
    createMediaInsightsPipeline_kinesisVideoStreamSourceRuntimeConfiguration,
    createMediaInsightsPipeline_mediaInsightsRuntimeMetadata,
    createMediaInsightsPipeline_s3RecordingSinkRuntimeConfiguration,
    createMediaInsightsPipeline_tags,
    createMediaInsightsPipeline_mediaInsightsPipelineConfigurationArn,

    -- * Destructuring the Response
    CreateMediaInsightsPipelineResponse (..),
    newCreateMediaInsightsPipelineResponse,

    -- * Response Lenses
    createMediaInsightsPipelineResponse_httpStatus,
    createMediaInsightsPipelineResponse_mediaInsightsPipeline,
  )
where

import Amazonka.ChimeSdkMediaPipelines.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMediaInsightsPipeline' smart constructor.
data CreateMediaInsightsPipeline = CreateMediaInsightsPipeline'
  { -- | The unique identifier for the media insights pipeline request.
    clientRequestToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The runtime configuration for the Kinesis video recording stream source.
    kinesisVideoStreamRecordingSourceRuntimeConfiguration :: Prelude.Maybe KinesisVideoStreamRecordingSourceRuntimeConfiguration,
    -- | The runtime configuration for the Kinesis video stream source of the
    -- media insights pipeline.
    kinesisVideoStreamSourceRuntimeConfiguration :: Prelude.Maybe KinesisVideoStreamSourceRuntimeConfiguration,
    -- | The runtime metadata for the media insights pipeline. Consists of a
    -- key-value map of strings.
    mediaInsightsRuntimeMetadata :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The runtime configuration for the S3 recording sink.
    s3RecordingSinkRuntimeConfiguration :: Prelude.Maybe S3RecordingSinkRuntimeConfiguration,
    -- | The tags assigned to the media insights pipeline.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The ARN of the pipeline\'s configuration.
    mediaInsightsPipelineConfigurationArn :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMediaInsightsPipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createMediaInsightsPipeline_clientRequestToken' - The unique identifier for the media insights pipeline request.
--
-- 'kinesisVideoStreamRecordingSourceRuntimeConfiguration', 'createMediaInsightsPipeline_kinesisVideoStreamRecordingSourceRuntimeConfiguration' - The runtime configuration for the Kinesis video recording stream source.
--
-- 'kinesisVideoStreamSourceRuntimeConfiguration', 'createMediaInsightsPipeline_kinesisVideoStreamSourceRuntimeConfiguration' - The runtime configuration for the Kinesis video stream source of the
-- media insights pipeline.
--
-- 'mediaInsightsRuntimeMetadata', 'createMediaInsightsPipeline_mediaInsightsRuntimeMetadata' - The runtime metadata for the media insights pipeline. Consists of a
-- key-value map of strings.
--
-- 's3RecordingSinkRuntimeConfiguration', 'createMediaInsightsPipeline_s3RecordingSinkRuntimeConfiguration' - The runtime configuration for the S3 recording sink.
--
-- 'tags', 'createMediaInsightsPipeline_tags' - The tags assigned to the media insights pipeline.
--
-- 'mediaInsightsPipelineConfigurationArn', 'createMediaInsightsPipeline_mediaInsightsPipelineConfigurationArn' - The ARN of the pipeline\'s configuration.
newCreateMediaInsightsPipeline ::
  -- | 'mediaInsightsPipelineConfigurationArn'
  Prelude.Text ->
  CreateMediaInsightsPipeline
newCreateMediaInsightsPipeline
  pMediaInsightsPipelineConfigurationArn_ =
    CreateMediaInsightsPipeline'
      { clientRequestToken =
          Prelude.Nothing,
        kinesisVideoStreamRecordingSourceRuntimeConfiguration =
          Prelude.Nothing,
        kinesisVideoStreamSourceRuntimeConfiguration =
          Prelude.Nothing,
        mediaInsightsRuntimeMetadata = Prelude.Nothing,
        s3RecordingSinkRuntimeConfiguration =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        mediaInsightsPipelineConfigurationArn =
          Data._Sensitive
            Lens.# pMediaInsightsPipelineConfigurationArn_
      }

-- | The unique identifier for the media insights pipeline request.
createMediaInsightsPipeline_clientRequestToken :: Lens.Lens' CreateMediaInsightsPipeline (Prelude.Maybe Prelude.Text)
createMediaInsightsPipeline_clientRequestToken = Lens.lens (\CreateMediaInsightsPipeline' {clientRequestToken} -> clientRequestToken) (\s@CreateMediaInsightsPipeline' {} a -> s {clientRequestToken = a} :: CreateMediaInsightsPipeline) Prelude.. Lens.mapping Data._Sensitive

-- | The runtime configuration for the Kinesis video recording stream source.
createMediaInsightsPipeline_kinesisVideoStreamRecordingSourceRuntimeConfiguration :: Lens.Lens' CreateMediaInsightsPipeline (Prelude.Maybe KinesisVideoStreamRecordingSourceRuntimeConfiguration)
createMediaInsightsPipeline_kinesisVideoStreamRecordingSourceRuntimeConfiguration = Lens.lens (\CreateMediaInsightsPipeline' {kinesisVideoStreamRecordingSourceRuntimeConfiguration} -> kinesisVideoStreamRecordingSourceRuntimeConfiguration) (\s@CreateMediaInsightsPipeline' {} a -> s {kinesisVideoStreamRecordingSourceRuntimeConfiguration = a} :: CreateMediaInsightsPipeline)

-- | The runtime configuration for the Kinesis video stream source of the
-- media insights pipeline.
createMediaInsightsPipeline_kinesisVideoStreamSourceRuntimeConfiguration :: Lens.Lens' CreateMediaInsightsPipeline (Prelude.Maybe KinesisVideoStreamSourceRuntimeConfiguration)
createMediaInsightsPipeline_kinesisVideoStreamSourceRuntimeConfiguration = Lens.lens (\CreateMediaInsightsPipeline' {kinesisVideoStreamSourceRuntimeConfiguration} -> kinesisVideoStreamSourceRuntimeConfiguration) (\s@CreateMediaInsightsPipeline' {} a -> s {kinesisVideoStreamSourceRuntimeConfiguration = a} :: CreateMediaInsightsPipeline)

-- | The runtime metadata for the media insights pipeline. Consists of a
-- key-value map of strings.
createMediaInsightsPipeline_mediaInsightsRuntimeMetadata :: Lens.Lens' CreateMediaInsightsPipeline (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createMediaInsightsPipeline_mediaInsightsRuntimeMetadata = Lens.lens (\CreateMediaInsightsPipeline' {mediaInsightsRuntimeMetadata} -> mediaInsightsRuntimeMetadata) (\s@CreateMediaInsightsPipeline' {} a -> s {mediaInsightsRuntimeMetadata = a} :: CreateMediaInsightsPipeline) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The runtime configuration for the S3 recording sink.
createMediaInsightsPipeline_s3RecordingSinkRuntimeConfiguration :: Lens.Lens' CreateMediaInsightsPipeline (Prelude.Maybe S3RecordingSinkRuntimeConfiguration)
createMediaInsightsPipeline_s3RecordingSinkRuntimeConfiguration = Lens.lens (\CreateMediaInsightsPipeline' {s3RecordingSinkRuntimeConfiguration} -> s3RecordingSinkRuntimeConfiguration) (\s@CreateMediaInsightsPipeline' {} a -> s {s3RecordingSinkRuntimeConfiguration = a} :: CreateMediaInsightsPipeline)

-- | The tags assigned to the media insights pipeline.
createMediaInsightsPipeline_tags :: Lens.Lens' CreateMediaInsightsPipeline (Prelude.Maybe (Prelude.NonEmpty Tag))
createMediaInsightsPipeline_tags = Lens.lens (\CreateMediaInsightsPipeline' {tags} -> tags) (\s@CreateMediaInsightsPipeline' {} a -> s {tags = a} :: CreateMediaInsightsPipeline) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the pipeline\'s configuration.
createMediaInsightsPipeline_mediaInsightsPipelineConfigurationArn :: Lens.Lens' CreateMediaInsightsPipeline Prelude.Text
createMediaInsightsPipeline_mediaInsightsPipelineConfigurationArn = Lens.lens (\CreateMediaInsightsPipeline' {mediaInsightsPipelineConfigurationArn} -> mediaInsightsPipelineConfigurationArn) (\s@CreateMediaInsightsPipeline' {} a -> s {mediaInsightsPipelineConfigurationArn = a} :: CreateMediaInsightsPipeline) Prelude.. Data._Sensitive

instance Core.AWSRequest CreateMediaInsightsPipeline where
  type
    AWSResponse CreateMediaInsightsPipeline =
      CreateMediaInsightsPipelineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMediaInsightsPipelineResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "MediaInsightsPipeline")
      )

instance Prelude.Hashable CreateMediaInsightsPipeline where
  hashWithSalt _salt CreateMediaInsightsPipeline' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` kinesisVideoStreamRecordingSourceRuntimeConfiguration
      `Prelude.hashWithSalt` kinesisVideoStreamSourceRuntimeConfiguration
      `Prelude.hashWithSalt` mediaInsightsRuntimeMetadata
      `Prelude.hashWithSalt` s3RecordingSinkRuntimeConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` mediaInsightsPipelineConfigurationArn

instance Prelude.NFData CreateMediaInsightsPipeline where
  rnf CreateMediaInsightsPipeline' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf
        kinesisVideoStreamRecordingSourceRuntimeConfiguration
      `Prelude.seq` Prelude.rnf
        kinesisVideoStreamSourceRuntimeConfiguration
      `Prelude.seq` Prelude.rnf mediaInsightsRuntimeMetadata
      `Prelude.seq` Prelude.rnf s3RecordingSinkRuntimeConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf mediaInsightsPipelineConfigurationArn

instance Data.ToHeaders CreateMediaInsightsPipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateMediaInsightsPipeline where
  toJSON CreateMediaInsightsPipeline' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ( "KinesisVideoStreamRecordingSourceRuntimeConfiguration"
                Data..=
            )
              Prelude.<$> kinesisVideoStreamRecordingSourceRuntimeConfiguration,
            ( "KinesisVideoStreamSourceRuntimeConfiguration"
                Data..=
            )
              Prelude.<$> kinesisVideoStreamSourceRuntimeConfiguration,
            ("MediaInsightsRuntimeMetadata" Data..=)
              Prelude.<$> mediaInsightsRuntimeMetadata,
            ("S3RecordingSinkRuntimeConfiguration" Data..=)
              Prelude.<$> s3RecordingSinkRuntimeConfiguration,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "MediaInsightsPipelineConfigurationArn"
                  Data..= mediaInsightsPipelineConfigurationArn
              )
          ]
      )

instance Data.ToPath CreateMediaInsightsPipeline where
  toPath = Prelude.const "/media-insights-pipelines"

instance Data.ToQuery CreateMediaInsightsPipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMediaInsightsPipelineResponse' smart constructor.
data CreateMediaInsightsPipelineResponse = CreateMediaInsightsPipelineResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The media insights pipeline object.
    mediaInsightsPipeline :: MediaInsightsPipeline
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMediaInsightsPipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createMediaInsightsPipelineResponse_httpStatus' - The response's http status code.
--
-- 'mediaInsightsPipeline', 'createMediaInsightsPipelineResponse_mediaInsightsPipeline' - The media insights pipeline object.
newCreateMediaInsightsPipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'mediaInsightsPipeline'
  MediaInsightsPipeline ->
  CreateMediaInsightsPipelineResponse
newCreateMediaInsightsPipelineResponse
  pHttpStatus_
  pMediaInsightsPipeline_ =
    CreateMediaInsightsPipelineResponse'
      { httpStatus =
          pHttpStatus_,
        mediaInsightsPipeline =
          pMediaInsightsPipeline_
      }

-- | The response's http status code.
createMediaInsightsPipelineResponse_httpStatus :: Lens.Lens' CreateMediaInsightsPipelineResponse Prelude.Int
createMediaInsightsPipelineResponse_httpStatus = Lens.lens (\CreateMediaInsightsPipelineResponse' {httpStatus} -> httpStatus) (\s@CreateMediaInsightsPipelineResponse' {} a -> s {httpStatus = a} :: CreateMediaInsightsPipelineResponse)

-- | The media insights pipeline object.
createMediaInsightsPipelineResponse_mediaInsightsPipeline :: Lens.Lens' CreateMediaInsightsPipelineResponse MediaInsightsPipeline
createMediaInsightsPipelineResponse_mediaInsightsPipeline = Lens.lens (\CreateMediaInsightsPipelineResponse' {mediaInsightsPipeline} -> mediaInsightsPipeline) (\s@CreateMediaInsightsPipelineResponse' {} a -> s {mediaInsightsPipeline = a} :: CreateMediaInsightsPipelineResponse)

instance
  Prelude.NFData
    CreateMediaInsightsPipelineResponse
  where
  rnf CreateMediaInsightsPipelineResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf mediaInsightsPipeline
