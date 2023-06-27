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
-- Module      : Amazonka.ChimeSdkMediaPipelines.GetMediaInsightsPipelineConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the configuration settings for a media insights pipeline.
module Amazonka.ChimeSdkMediaPipelines.GetMediaInsightsPipelineConfiguration
  ( -- * Creating a Request
    GetMediaInsightsPipelineConfiguration (..),
    newGetMediaInsightsPipelineConfiguration,

    -- * Request Lenses
    getMediaInsightsPipelineConfiguration_identifier,

    -- * Destructuring the Response
    GetMediaInsightsPipelineConfigurationResponse (..),
    newGetMediaInsightsPipelineConfigurationResponse,

    -- * Response Lenses
    getMediaInsightsPipelineConfigurationResponse_mediaInsightsPipelineConfiguration,
    getMediaInsightsPipelineConfigurationResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkMediaPipelines.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMediaInsightsPipelineConfiguration' smart constructor.
data GetMediaInsightsPipelineConfiguration = GetMediaInsightsPipelineConfiguration'
  { -- | The unique identifier of the requested resource. Valid values include
    -- the name and ARN of the media insights pipeline configuration.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMediaInsightsPipelineConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'getMediaInsightsPipelineConfiguration_identifier' - The unique identifier of the requested resource. Valid values include
-- the name and ARN of the media insights pipeline configuration.
newGetMediaInsightsPipelineConfiguration ::
  -- | 'identifier'
  Prelude.Text ->
  GetMediaInsightsPipelineConfiguration
newGetMediaInsightsPipelineConfiguration pIdentifier_ =
  GetMediaInsightsPipelineConfiguration'
    { identifier =
        pIdentifier_
    }

-- | The unique identifier of the requested resource. Valid values include
-- the name and ARN of the media insights pipeline configuration.
getMediaInsightsPipelineConfiguration_identifier :: Lens.Lens' GetMediaInsightsPipelineConfiguration Prelude.Text
getMediaInsightsPipelineConfiguration_identifier = Lens.lens (\GetMediaInsightsPipelineConfiguration' {identifier} -> identifier) (\s@GetMediaInsightsPipelineConfiguration' {} a -> s {identifier = a} :: GetMediaInsightsPipelineConfiguration)

instance
  Core.AWSRequest
    GetMediaInsightsPipelineConfiguration
  where
  type
    AWSResponse
      GetMediaInsightsPipelineConfiguration =
      GetMediaInsightsPipelineConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMediaInsightsPipelineConfigurationResponse'
            Prelude.<$> (x Data..?> "MediaInsightsPipelineConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetMediaInsightsPipelineConfiguration
  where
  hashWithSalt
    _salt
    GetMediaInsightsPipelineConfiguration' {..} =
      _salt `Prelude.hashWithSalt` identifier

instance
  Prelude.NFData
    GetMediaInsightsPipelineConfiguration
  where
  rnf GetMediaInsightsPipelineConfiguration' {..} =
    Prelude.rnf identifier

instance
  Data.ToHeaders
    GetMediaInsightsPipelineConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetMediaInsightsPipelineConfiguration
  where
  toPath GetMediaInsightsPipelineConfiguration' {..} =
    Prelude.mconcat
      [ "/media-insights-pipeline-configurations/",
        Data.toBS identifier
      ]

instance
  Data.ToQuery
    GetMediaInsightsPipelineConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMediaInsightsPipelineConfigurationResponse' smart constructor.
data GetMediaInsightsPipelineConfigurationResponse = GetMediaInsightsPipelineConfigurationResponse'
  { -- | The requested media insights pipeline configuration.
    mediaInsightsPipelineConfiguration :: Prelude.Maybe MediaInsightsPipelineConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMediaInsightsPipelineConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaInsightsPipelineConfiguration', 'getMediaInsightsPipelineConfigurationResponse_mediaInsightsPipelineConfiguration' - The requested media insights pipeline configuration.
--
-- 'httpStatus', 'getMediaInsightsPipelineConfigurationResponse_httpStatus' - The response's http status code.
newGetMediaInsightsPipelineConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMediaInsightsPipelineConfigurationResponse
newGetMediaInsightsPipelineConfigurationResponse
  pHttpStatus_ =
    GetMediaInsightsPipelineConfigurationResponse'
      { mediaInsightsPipelineConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The requested media insights pipeline configuration.
getMediaInsightsPipelineConfigurationResponse_mediaInsightsPipelineConfiguration :: Lens.Lens' GetMediaInsightsPipelineConfigurationResponse (Prelude.Maybe MediaInsightsPipelineConfiguration)
getMediaInsightsPipelineConfigurationResponse_mediaInsightsPipelineConfiguration = Lens.lens (\GetMediaInsightsPipelineConfigurationResponse' {mediaInsightsPipelineConfiguration} -> mediaInsightsPipelineConfiguration) (\s@GetMediaInsightsPipelineConfigurationResponse' {} a -> s {mediaInsightsPipelineConfiguration = a} :: GetMediaInsightsPipelineConfigurationResponse)

-- | The response's http status code.
getMediaInsightsPipelineConfigurationResponse_httpStatus :: Lens.Lens' GetMediaInsightsPipelineConfigurationResponse Prelude.Int
getMediaInsightsPipelineConfigurationResponse_httpStatus = Lens.lens (\GetMediaInsightsPipelineConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetMediaInsightsPipelineConfigurationResponse' {} a -> s {httpStatus = a} :: GetMediaInsightsPipelineConfigurationResponse)

instance
  Prelude.NFData
    GetMediaInsightsPipelineConfigurationResponse
  where
  rnf
    GetMediaInsightsPipelineConfigurationResponse' {..} =
      Prelude.rnf mediaInsightsPipelineConfiguration
        `Prelude.seq` Prelude.rnf httpStatus
