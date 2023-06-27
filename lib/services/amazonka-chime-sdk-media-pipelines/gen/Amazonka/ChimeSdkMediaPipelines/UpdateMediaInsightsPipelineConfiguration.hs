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
-- Module      : Amazonka.ChimeSdkMediaPipelines.UpdateMediaInsightsPipelineConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the media insights pipeline\'s configuration settings.
module Amazonka.ChimeSdkMediaPipelines.UpdateMediaInsightsPipelineConfiguration
  ( -- * Creating a Request
    UpdateMediaInsightsPipelineConfiguration (..),
    newUpdateMediaInsightsPipelineConfiguration,

    -- * Request Lenses
    updateMediaInsightsPipelineConfiguration_realTimeAlertConfiguration,
    updateMediaInsightsPipelineConfiguration_identifier,
    updateMediaInsightsPipelineConfiguration_resourceAccessRoleArn,
    updateMediaInsightsPipelineConfiguration_elements,

    -- * Destructuring the Response
    UpdateMediaInsightsPipelineConfigurationResponse (..),
    newUpdateMediaInsightsPipelineConfigurationResponse,

    -- * Response Lenses
    updateMediaInsightsPipelineConfigurationResponse_mediaInsightsPipelineConfiguration,
    updateMediaInsightsPipelineConfigurationResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkMediaPipelines.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMediaInsightsPipelineConfiguration' smart constructor.
data UpdateMediaInsightsPipelineConfiguration = UpdateMediaInsightsPipelineConfiguration'
  { -- | The configuration settings for real-time alerts for the media insights
    -- pipeline.
    realTimeAlertConfiguration :: Prelude.Maybe RealTimeAlertConfiguration,
    -- | The unique identifier for the resource to be updated. Valid values
    -- include the name and ARN of the media insights pipeline configuration.
    identifier :: Prelude.Text,
    -- | The ARN of the role used by the service to access Amazon Web Services
    -- resources.
    resourceAccessRoleArn :: Data.Sensitive Prelude.Text,
    -- | The elements in the request, such as a processor for Amazon Transcribe
    -- or a sink for a Kinesis Data Stream..
    elements :: [MediaInsightsPipelineConfigurationElement]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMediaInsightsPipelineConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'realTimeAlertConfiguration', 'updateMediaInsightsPipelineConfiguration_realTimeAlertConfiguration' - The configuration settings for real-time alerts for the media insights
-- pipeline.
--
-- 'identifier', 'updateMediaInsightsPipelineConfiguration_identifier' - The unique identifier for the resource to be updated. Valid values
-- include the name and ARN of the media insights pipeline configuration.
--
-- 'resourceAccessRoleArn', 'updateMediaInsightsPipelineConfiguration_resourceAccessRoleArn' - The ARN of the role used by the service to access Amazon Web Services
-- resources.
--
-- 'elements', 'updateMediaInsightsPipelineConfiguration_elements' - The elements in the request, such as a processor for Amazon Transcribe
-- or a sink for a Kinesis Data Stream..
newUpdateMediaInsightsPipelineConfiguration ::
  -- | 'identifier'
  Prelude.Text ->
  -- | 'resourceAccessRoleArn'
  Prelude.Text ->
  UpdateMediaInsightsPipelineConfiguration
newUpdateMediaInsightsPipelineConfiguration
  pIdentifier_
  pResourceAccessRoleArn_ =
    UpdateMediaInsightsPipelineConfiguration'
      { realTimeAlertConfiguration =
          Prelude.Nothing,
        identifier = pIdentifier_,
        resourceAccessRoleArn =
          Data._Sensitive
            Lens.# pResourceAccessRoleArn_,
        elements = Prelude.mempty
      }

-- | The configuration settings for real-time alerts for the media insights
-- pipeline.
updateMediaInsightsPipelineConfiguration_realTimeAlertConfiguration :: Lens.Lens' UpdateMediaInsightsPipelineConfiguration (Prelude.Maybe RealTimeAlertConfiguration)
updateMediaInsightsPipelineConfiguration_realTimeAlertConfiguration = Lens.lens (\UpdateMediaInsightsPipelineConfiguration' {realTimeAlertConfiguration} -> realTimeAlertConfiguration) (\s@UpdateMediaInsightsPipelineConfiguration' {} a -> s {realTimeAlertConfiguration = a} :: UpdateMediaInsightsPipelineConfiguration)

-- | The unique identifier for the resource to be updated. Valid values
-- include the name and ARN of the media insights pipeline configuration.
updateMediaInsightsPipelineConfiguration_identifier :: Lens.Lens' UpdateMediaInsightsPipelineConfiguration Prelude.Text
updateMediaInsightsPipelineConfiguration_identifier = Lens.lens (\UpdateMediaInsightsPipelineConfiguration' {identifier} -> identifier) (\s@UpdateMediaInsightsPipelineConfiguration' {} a -> s {identifier = a} :: UpdateMediaInsightsPipelineConfiguration)

-- | The ARN of the role used by the service to access Amazon Web Services
-- resources.
updateMediaInsightsPipelineConfiguration_resourceAccessRoleArn :: Lens.Lens' UpdateMediaInsightsPipelineConfiguration Prelude.Text
updateMediaInsightsPipelineConfiguration_resourceAccessRoleArn = Lens.lens (\UpdateMediaInsightsPipelineConfiguration' {resourceAccessRoleArn} -> resourceAccessRoleArn) (\s@UpdateMediaInsightsPipelineConfiguration' {} a -> s {resourceAccessRoleArn = a} :: UpdateMediaInsightsPipelineConfiguration) Prelude.. Data._Sensitive

-- | The elements in the request, such as a processor for Amazon Transcribe
-- or a sink for a Kinesis Data Stream..
updateMediaInsightsPipelineConfiguration_elements :: Lens.Lens' UpdateMediaInsightsPipelineConfiguration [MediaInsightsPipelineConfigurationElement]
updateMediaInsightsPipelineConfiguration_elements = Lens.lens (\UpdateMediaInsightsPipelineConfiguration' {elements} -> elements) (\s@UpdateMediaInsightsPipelineConfiguration' {} a -> s {elements = a} :: UpdateMediaInsightsPipelineConfiguration) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    UpdateMediaInsightsPipelineConfiguration
  where
  type
    AWSResponse
      UpdateMediaInsightsPipelineConfiguration =
      UpdateMediaInsightsPipelineConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMediaInsightsPipelineConfigurationResponse'
            Prelude.<$> (x Data..?> "MediaInsightsPipelineConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateMediaInsightsPipelineConfiguration
  where
  hashWithSalt
    _salt
    UpdateMediaInsightsPipelineConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` realTimeAlertConfiguration
        `Prelude.hashWithSalt` identifier
        `Prelude.hashWithSalt` resourceAccessRoleArn
        `Prelude.hashWithSalt` elements

instance
  Prelude.NFData
    UpdateMediaInsightsPipelineConfiguration
  where
  rnf UpdateMediaInsightsPipelineConfiguration' {..} =
    Prelude.rnf realTimeAlertConfiguration
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf resourceAccessRoleArn
      `Prelude.seq` Prelude.rnf elements

instance
  Data.ToHeaders
    UpdateMediaInsightsPipelineConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    UpdateMediaInsightsPipelineConfiguration
  where
  toJSON UpdateMediaInsightsPipelineConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RealTimeAlertConfiguration" Data..=)
              Prelude.<$> realTimeAlertConfiguration,
            Prelude.Just
              ( "ResourceAccessRoleArn"
                  Data..= resourceAccessRoleArn
              ),
            Prelude.Just ("Elements" Data..= elements)
          ]
      )

instance
  Data.ToPath
    UpdateMediaInsightsPipelineConfiguration
  where
  toPath UpdateMediaInsightsPipelineConfiguration' {..} =
    Prelude.mconcat
      [ "/media-insights-pipeline-configurations/",
        Data.toBS identifier
      ]

instance
  Data.ToQuery
    UpdateMediaInsightsPipelineConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMediaInsightsPipelineConfigurationResponse' smart constructor.
data UpdateMediaInsightsPipelineConfigurationResponse = UpdateMediaInsightsPipelineConfigurationResponse'
  { -- | The updated configuration settings.
    mediaInsightsPipelineConfiguration :: Prelude.Maybe MediaInsightsPipelineConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMediaInsightsPipelineConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaInsightsPipelineConfiguration', 'updateMediaInsightsPipelineConfigurationResponse_mediaInsightsPipelineConfiguration' - The updated configuration settings.
--
-- 'httpStatus', 'updateMediaInsightsPipelineConfigurationResponse_httpStatus' - The response's http status code.
newUpdateMediaInsightsPipelineConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMediaInsightsPipelineConfigurationResponse
newUpdateMediaInsightsPipelineConfigurationResponse
  pHttpStatus_ =
    UpdateMediaInsightsPipelineConfigurationResponse'
      { mediaInsightsPipelineConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The updated configuration settings.
updateMediaInsightsPipelineConfigurationResponse_mediaInsightsPipelineConfiguration :: Lens.Lens' UpdateMediaInsightsPipelineConfigurationResponse (Prelude.Maybe MediaInsightsPipelineConfiguration)
updateMediaInsightsPipelineConfigurationResponse_mediaInsightsPipelineConfiguration = Lens.lens (\UpdateMediaInsightsPipelineConfigurationResponse' {mediaInsightsPipelineConfiguration} -> mediaInsightsPipelineConfiguration) (\s@UpdateMediaInsightsPipelineConfigurationResponse' {} a -> s {mediaInsightsPipelineConfiguration = a} :: UpdateMediaInsightsPipelineConfigurationResponse)

-- | The response's http status code.
updateMediaInsightsPipelineConfigurationResponse_httpStatus :: Lens.Lens' UpdateMediaInsightsPipelineConfigurationResponse Prelude.Int
updateMediaInsightsPipelineConfigurationResponse_httpStatus = Lens.lens (\UpdateMediaInsightsPipelineConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateMediaInsightsPipelineConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateMediaInsightsPipelineConfigurationResponse)

instance
  Prelude.NFData
    UpdateMediaInsightsPipelineConfigurationResponse
  where
  rnf
    UpdateMediaInsightsPipelineConfigurationResponse' {..} =
      Prelude.rnf mediaInsightsPipelineConfiguration
        `Prelude.seq` Prelude.rnf httpStatus
