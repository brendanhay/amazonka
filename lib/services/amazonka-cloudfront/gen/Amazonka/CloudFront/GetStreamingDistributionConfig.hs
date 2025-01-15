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
-- Module      : Amazonka.CloudFront.GetStreamingDistributionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the configuration information about a streaming distribution.
module Amazonka.CloudFront.GetStreamingDistributionConfig
  ( -- * Creating a Request
    GetStreamingDistributionConfig (..),
    newGetStreamingDistributionConfig,

    -- * Request Lenses
    getStreamingDistributionConfig_id,

    -- * Destructuring the Response
    GetStreamingDistributionConfigResponse (..),
    newGetStreamingDistributionConfigResponse,

    -- * Response Lenses
    getStreamingDistributionConfigResponse_eTag,
    getStreamingDistributionConfigResponse_streamingDistributionConfig,
    getStreamingDistributionConfigResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | To request to get a streaming distribution configuration.
--
-- /See:/ 'newGetStreamingDistributionConfig' smart constructor.
data GetStreamingDistributionConfig = GetStreamingDistributionConfig'
  { -- | The streaming distribution\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamingDistributionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getStreamingDistributionConfig_id' - The streaming distribution\'s ID.
newGetStreamingDistributionConfig ::
  -- | 'id'
  Prelude.Text ->
  GetStreamingDistributionConfig
newGetStreamingDistributionConfig pId_ =
  GetStreamingDistributionConfig' {id = pId_}

-- | The streaming distribution\'s ID.
getStreamingDistributionConfig_id :: Lens.Lens' GetStreamingDistributionConfig Prelude.Text
getStreamingDistributionConfig_id = Lens.lens (\GetStreamingDistributionConfig' {id} -> id) (\s@GetStreamingDistributionConfig' {} a -> s {id = a} :: GetStreamingDistributionConfig)

instance
  Core.AWSRequest
    GetStreamingDistributionConfig
  where
  type
    AWSResponse GetStreamingDistributionConfig =
      GetStreamingDistributionConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetStreamingDistributionConfigResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetStreamingDistributionConfig
  where
  hashWithSalt
    _salt
    GetStreamingDistributionConfig' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    GetStreamingDistributionConfig
  where
  rnf GetStreamingDistributionConfig' {..} =
    Prelude.rnf id

instance
  Data.ToHeaders
    GetStreamingDistributionConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetStreamingDistributionConfig where
  toPath GetStreamingDistributionConfig' {..} =
    Prelude.mconcat
      [ "/2020-05-31/streaming-distribution/",
        Data.toBS id,
        "/config"
      ]

instance Data.ToQuery GetStreamingDistributionConfig where
  toQuery = Prelude.const Prelude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newGetStreamingDistributionConfigResponse' smart constructor.
data GetStreamingDistributionConfigResponse = GetStreamingDistributionConfigResponse'
  { -- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The streaming distribution\'s configuration information.
    streamingDistributionConfig :: Prelude.Maybe StreamingDistributionConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamingDistributionConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getStreamingDistributionConfigResponse_eTag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
--
-- 'streamingDistributionConfig', 'getStreamingDistributionConfigResponse_streamingDistributionConfig' - The streaming distribution\'s configuration information.
--
-- 'httpStatus', 'getStreamingDistributionConfigResponse_httpStatus' - The response's http status code.
newGetStreamingDistributionConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStreamingDistributionConfigResponse
newGetStreamingDistributionConfigResponse
  pHttpStatus_ =
    GetStreamingDistributionConfigResponse'
      { eTag =
          Prelude.Nothing,
        streamingDistributionConfig =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
getStreamingDistributionConfigResponse_eTag :: Lens.Lens' GetStreamingDistributionConfigResponse (Prelude.Maybe Prelude.Text)
getStreamingDistributionConfigResponse_eTag = Lens.lens (\GetStreamingDistributionConfigResponse' {eTag} -> eTag) (\s@GetStreamingDistributionConfigResponse' {} a -> s {eTag = a} :: GetStreamingDistributionConfigResponse)

-- | The streaming distribution\'s configuration information.
getStreamingDistributionConfigResponse_streamingDistributionConfig :: Lens.Lens' GetStreamingDistributionConfigResponse (Prelude.Maybe StreamingDistributionConfig)
getStreamingDistributionConfigResponse_streamingDistributionConfig = Lens.lens (\GetStreamingDistributionConfigResponse' {streamingDistributionConfig} -> streamingDistributionConfig) (\s@GetStreamingDistributionConfigResponse' {} a -> s {streamingDistributionConfig = a} :: GetStreamingDistributionConfigResponse)

-- | The response's http status code.
getStreamingDistributionConfigResponse_httpStatus :: Lens.Lens' GetStreamingDistributionConfigResponse Prelude.Int
getStreamingDistributionConfigResponse_httpStatus = Lens.lens (\GetStreamingDistributionConfigResponse' {httpStatus} -> httpStatus) (\s@GetStreamingDistributionConfigResponse' {} a -> s {httpStatus = a} :: GetStreamingDistributionConfigResponse)

instance
  Prelude.NFData
    GetStreamingDistributionConfigResponse
  where
  rnf GetStreamingDistributionConfigResponse' {..} =
    Prelude.rnf eTag `Prelude.seq`
      Prelude.rnf streamingDistributionConfig `Prelude.seq`
        Prelude.rnf httpStatus
