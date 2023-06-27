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
-- Module      : Amazonka.ChimeSdkMediaPipelines.CreateMediaInsightsPipelineConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A structure that contains the static configurations for a media insights
-- pipeline.
module Amazonka.ChimeSdkMediaPipelines.CreateMediaInsightsPipelineConfiguration
  ( -- * Creating a Request
    CreateMediaInsightsPipelineConfiguration (..),
    newCreateMediaInsightsPipelineConfiguration,

    -- * Request Lenses
    createMediaInsightsPipelineConfiguration_clientRequestToken,
    createMediaInsightsPipelineConfiguration_realTimeAlertConfiguration,
    createMediaInsightsPipelineConfiguration_tags,
    createMediaInsightsPipelineConfiguration_mediaInsightsPipelineConfigurationName,
    createMediaInsightsPipelineConfiguration_resourceAccessRoleArn,
    createMediaInsightsPipelineConfiguration_elements,

    -- * Destructuring the Response
    CreateMediaInsightsPipelineConfigurationResponse (..),
    newCreateMediaInsightsPipelineConfigurationResponse,

    -- * Response Lenses
    createMediaInsightsPipelineConfigurationResponse_mediaInsightsPipelineConfiguration,
    createMediaInsightsPipelineConfigurationResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkMediaPipelines.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMediaInsightsPipelineConfiguration' smart constructor.
data CreateMediaInsightsPipelineConfiguration = CreateMediaInsightsPipelineConfiguration'
  { -- | The unique identifier for the media insights pipeline configuration
    -- request.
    clientRequestToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The configuration settings for the real-time alerts in a media insights
    -- pipeline configuration.
    realTimeAlertConfiguration :: Prelude.Maybe RealTimeAlertConfiguration,
    -- | The tags assigned to the media insights pipeline configuration.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The name of the media insights pipeline configuration.
    mediaInsightsPipelineConfigurationName :: Prelude.Text,
    -- | The ARN of the role used by the service to access Amazon Web Services
    -- resources, including @Transcribe@ and @Transcribe Call Analytics@, on
    -- the caller’s behalf.
    resourceAccessRoleArn :: Data.Sensitive Prelude.Text,
    -- | The elements in the request, such as a processor for Amazon Transcribe
    -- or a sink for a Kinesis Data Stream.
    elements :: [MediaInsightsPipelineConfigurationElement]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMediaInsightsPipelineConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createMediaInsightsPipelineConfiguration_clientRequestToken' - The unique identifier for the media insights pipeline configuration
-- request.
--
-- 'realTimeAlertConfiguration', 'createMediaInsightsPipelineConfiguration_realTimeAlertConfiguration' - The configuration settings for the real-time alerts in a media insights
-- pipeline configuration.
--
-- 'tags', 'createMediaInsightsPipelineConfiguration_tags' - The tags assigned to the media insights pipeline configuration.
--
-- 'mediaInsightsPipelineConfigurationName', 'createMediaInsightsPipelineConfiguration_mediaInsightsPipelineConfigurationName' - The name of the media insights pipeline configuration.
--
-- 'resourceAccessRoleArn', 'createMediaInsightsPipelineConfiguration_resourceAccessRoleArn' - The ARN of the role used by the service to access Amazon Web Services
-- resources, including @Transcribe@ and @Transcribe Call Analytics@, on
-- the caller’s behalf.
--
-- 'elements', 'createMediaInsightsPipelineConfiguration_elements' - The elements in the request, such as a processor for Amazon Transcribe
-- or a sink for a Kinesis Data Stream.
newCreateMediaInsightsPipelineConfiguration ::
  -- | 'mediaInsightsPipelineConfigurationName'
  Prelude.Text ->
  -- | 'resourceAccessRoleArn'
  Prelude.Text ->
  CreateMediaInsightsPipelineConfiguration
newCreateMediaInsightsPipelineConfiguration
  pMediaInsightsPipelineConfigurationName_
  pResourceAccessRoleArn_ =
    CreateMediaInsightsPipelineConfiguration'
      { clientRequestToken =
          Prelude.Nothing,
        realTimeAlertConfiguration =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        mediaInsightsPipelineConfigurationName =
          pMediaInsightsPipelineConfigurationName_,
        resourceAccessRoleArn =
          Data._Sensitive
            Lens.# pResourceAccessRoleArn_,
        elements = Prelude.mempty
      }

-- | The unique identifier for the media insights pipeline configuration
-- request.
createMediaInsightsPipelineConfiguration_clientRequestToken :: Lens.Lens' CreateMediaInsightsPipelineConfiguration (Prelude.Maybe Prelude.Text)
createMediaInsightsPipelineConfiguration_clientRequestToken = Lens.lens (\CreateMediaInsightsPipelineConfiguration' {clientRequestToken} -> clientRequestToken) (\s@CreateMediaInsightsPipelineConfiguration' {} a -> s {clientRequestToken = a} :: CreateMediaInsightsPipelineConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The configuration settings for the real-time alerts in a media insights
-- pipeline configuration.
createMediaInsightsPipelineConfiguration_realTimeAlertConfiguration :: Lens.Lens' CreateMediaInsightsPipelineConfiguration (Prelude.Maybe RealTimeAlertConfiguration)
createMediaInsightsPipelineConfiguration_realTimeAlertConfiguration = Lens.lens (\CreateMediaInsightsPipelineConfiguration' {realTimeAlertConfiguration} -> realTimeAlertConfiguration) (\s@CreateMediaInsightsPipelineConfiguration' {} a -> s {realTimeAlertConfiguration = a} :: CreateMediaInsightsPipelineConfiguration)

-- | The tags assigned to the media insights pipeline configuration.
createMediaInsightsPipelineConfiguration_tags :: Lens.Lens' CreateMediaInsightsPipelineConfiguration (Prelude.Maybe (Prelude.NonEmpty Tag))
createMediaInsightsPipelineConfiguration_tags = Lens.lens (\CreateMediaInsightsPipelineConfiguration' {tags} -> tags) (\s@CreateMediaInsightsPipelineConfiguration' {} a -> s {tags = a} :: CreateMediaInsightsPipelineConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the media insights pipeline configuration.
createMediaInsightsPipelineConfiguration_mediaInsightsPipelineConfigurationName :: Lens.Lens' CreateMediaInsightsPipelineConfiguration Prelude.Text
createMediaInsightsPipelineConfiguration_mediaInsightsPipelineConfigurationName = Lens.lens (\CreateMediaInsightsPipelineConfiguration' {mediaInsightsPipelineConfigurationName} -> mediaInsightsPipelineConfigurationName) (\s@CreateMediaInsightsPipelineConfiguration' {} a -> s {mediaInsightsPipelineConfigurationName = a} :: CreateMediaInsightsPipelineConfiguration)

-- | The ARN of the role used by the service to access Amazon Web Services
-- resources, including @Transcribe@ and @Transcribe Call Analytics@, on
-- the caller’s behalf.
createMediaInsightsPipelineConfiguration_resourceAccessRoleArn :: Lens.Lens' CreateMediaInsightsPipelineConfiguration Prelude.Text
createMediaInsightsPipelineConfiguration_resourceAccessRoleArn = Lens.lens (\CreateMediaInsightsPipelineConfiguration' {resourceAccessRoleArn} -> resourceAccessRoleArn) (\s@CreateMediaInsightsPipelineConfiguration' {} a -> s {resourceAccessRoleArn = a} :: CreateMediaInsightsPipelineConfiguration) Prelude.. Data._Sensitive

-- | The elements in the request, such as a processor for Amazon Transcribe
-- or a sink for a Kinesis Data Stream.
createMediaInsightsPipelineConfiguration_elements :: Lens.Lens' CreateMediaInsightsPipelineConfiguration [MediaInsightsPipelineConfigurationElement]
createMediaInsightsPipelineConfiguration_elements = Lens.lens (\CreateMediaInsightsPipelineConfiguration' {elements} -> elements) (\s@CreateMediaInsightsPipelineConfiguration' {} a -> s {elements = a} :: CreateMediaInsightsPipelineConfiguration) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    CreateMediaInsightsPipelineConfiguration
  where
  type
    AWSResponse
      CreateMediaInsightsPipelineConfiguration =
      CreateMediaInsightsPipelineConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMediaInsightsPipelineConfigurationResponse'
            Prelude.<$> (x Data..?> "MediaInsightsPipelineConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateMediaInsightsPipelineConfiguration
  where
  hashWithSalt
    _salt
    CreateMediaInsightsPipelineConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` clientRequestToken
        `Prelude.hashWithSalt` realTimeAlertConfiguration
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` mediaInsightsPipelineConfigurationName
        `Prelude.hashWithSalt` resourceAccessRoleArn
        `Prelude.hashWithSalt` elements

instance
  Prelude.NFData
    CreateMediaInsightsPipelineConfiguration
  where
  rnf CreateMediaInsightsPipelineConfiguration' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf realTimeAlertConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf mediaInsightsPipelineConfigurationName
      `Prelude.seq` Prelude.rnf resourceAccessRoleArn
      `Prelude.seq` Prelude.rnf elements

instance
  Data.ToHeaders
    CreateMediaInsightsPipelineConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    CreateMediaInsightsPipelineConfiguration
  where
  toJSON CreateMediaInsightsPipelineConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("RealTimeAlertConfiguration" Data..=)
              Prelude.<$> realTimeAlertConfiguration,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "MediaInsightsPipelineConfigurationName"
                  Data..= mediaInsightsPipelineConfigurationName
              ),
            Prelude.Just
              ( "ResourceAccessRoleArn"
                  Data..= resourceAccessRoleArn
              ),
            Prelude.Just ("Elements" Data..= elements)
          ]
      )

instance
  Data.ToPath
    CreateMediaInsightsPipelineConfiguration
  where
  toPath =
    Prelude.const
      "/media-insights-pipeline-configurations"

instance
  Data.ToQuery
    CreateMediaInsightsPipelineConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMediaInsightsPipelineConfigurationResponse' smart constructor.
data CreateMediaInsightsPipelineConfigurationResponse = CreateMediaInsightsPipelineConfigurationResponse'
  { -- | The configuration settings for the media insights pipeline.
    mediaInsightsPipelineConfiguration :: Prelude.Maybe MediaInsightsPipelineConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMediaInsightsPipelineConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaInsightsPipelineConfiguration', 'createMediaInsightsPipelineConfigurationResponse_mediaInsightsPipelineConfiguration' - The configuration settings for the media insights pipeline.
--
-- 'httpStatus', 'createMediaInsightsPipelineConfigurationResponse_httpStatus' - The response's http status code.
newCreateMediaInsightsPipelineConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMediaInsightsPipelineConfigurationResponse
newCreateMediaInsightsPipelineConfigurationResponse
  pHttpStatus_ =
    CreateMediaInsightsPipelineConfigurationResponse'
      { mediaInsightsPipelineConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The configuration settings for the media insights pipeline.
createMediaInsightsPipelineConfigurationResponse_mediaInsightsPipelineConfiguration :: Lens.Lens' CreateMediaInsightsPipelineConfigurationResponse (Prelude.Maybe MediaInsightsPipelineConfiguration)
createMediaInsightsPipelineConfigurationResponse_mediaInsightsPipelineConfiguration = Lens.lens (\CreateMediaInsightsPipelineConfigurationResponse' {mediaInsightsPipelineConfiguration} -> mediaInsightsPipelineConfiguration) (\s@CreateMediaInsightsPipelineConfigurationResponse' {} a -> s {mediaInsightsPipelineConfiguration = a} :: CreateMediaInsightsPipelineConfigurationResponse)

-- | The response's http status code.
createMediaInsightsPipelineConfigurationResponse_httpStatus :: Lens.Lens' CreateMediaInsightsPipelineConfigurationResponse Prelude.Int
createMediaInsightsPipelineConfigurationResponse_httpStatus = Lens.lens (\CreateMediaInsightsPipelineConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateMediaInsightsPipelineConfigurationResponse' {} a -> s {httpStatus = a} :: CreateMediaInsightsPipelineConfigurationResponse)

instance
  Prelude.NFData
    CreateMediaInsightsPipelineConfigurationResponse
  where
  rnf
    CreateMediaInsightsPipelineConfigurationResponse' {..} =
      Prelude.rnf mediaInsightsPipelineConfiguration
        `Prelude.seq` Prelude.rnf httpStatus
