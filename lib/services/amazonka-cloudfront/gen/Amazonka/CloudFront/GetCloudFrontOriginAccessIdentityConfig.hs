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
-- Module      : Amazonka.CloudFront.GetCloudFrontOriginAccessIdentityConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the configuration information about an origin access identity.
module Amazonka.CloudFront.GetCloudFrontOriginAccessIdentityConfig
  ( -- * Creating a Request
    GetCloudFrontOriginAccessIdentityConfig (..),
    newGetCloudFrontOriginAccessIdentityConfig,

    -- * Request Lenses
    getCloudFrontOriginAccessIdentityConfig_id,

    -- * Destructuring the Response
    GetCloudFrontOriginAccessIdentityConfigResponse (..),
    newGetCloudFrontOriginAccessIdentityConfigResponse,

    -- * Response Lenses
    getCloudFrontOriginAccessIdentityConfigResponse_cloudFrontOriginAccessIdentityConfig,
    getCloudFrontOriginAccessIdentityConfigResponse_eTag,
    getCloudFrontOriginAccessIdentityConfigResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The origin access identity\'s configuration information. For more
-- information, see
-- <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_CloudFrontOriginAccessIdentityConfig.html CloudFrontOriginAccessIdentityConfig>.
--
-- /See:/ 'newGetCloudFrontOriginAccessIdentityConfig' smart constructor.
data GetCloudFrontOriginAccessIdentityConfig = GetCloudFrontOriginAccessIdentityConfig'
  { -- | The identity\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCloudFrontOriginAccessIdentityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getCloudFrontOriginAccessIdentityConfig_id' - The identity\'s ID.
newGetCloudFrontOriginAccessIdentityConfig ::
  -- | 'id'
  Prelude.Text ->
  GetCloudFrontOriginAccessIdentityConfig
newGetCloudFrontOriginAccessIdentityConfig pId_ =
  GetCloudFrontOriginAccessIdentityConfig' {id = pId_}

-- | The identity\'s ID.
getCloudFrontOriginAccessIdentityConfig_id :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfig Prelude.Text
getCloudFrontOriginAccessIdentityConfig_id = Lens.lens (\GetCloudFrontOriginAccessIdentityConfig' {id} -> id) (\s@GetCloudFrontOriginAccessIdentityConfig' {} a -> s {id = a} :: GetCloudFrontOriginAccessIdentityConfig)

instance
  Core.AWSRequest
    GetCloudFrontOriginAccessIdentityConfig
  where
  type
    AWSResponse
      GetCloudFrontOriginAccessIdentityConfig =
      GetCloudFrontOriginAccessIdentityConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetCloudFrontOriginAccessIdentityConfigResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetCloudFrontOriginAccessIdentityConfig
  where
  hashWithSalt
    _salt
    GetCloudFrontOriginAccessIdentityConfig' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    GetCloudFrontOriginAccessIdentityConfig
  where
  rnf GetCloudFrontOriginAccessIdentityConfig' {..} =
    Prelude.rnf id

instance
  Data.ToHeaders
    GetCloudFrontOriginAccessIdentityConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetCloudFrontOriginAccessIdentityConfig
  where
  toPath GetCloudFrontOriginAccessIdentityConfig' {..} =
    Prelude.mconcat
      [ "/2020-05-31/origin-access-identity/cloudfront/",
        Data.toBS id,
        "/config"
      ]

instance
  Data.ToQuery
    GetCloudFrontOriginAccessIdentityConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newGetCloudFrontOriginAccessIdentityConfigResponse' smart constructor.
data GetCloudFrontOriginAccessIdentityConfigResponse = GetCloudFrontOriginAccessIdentityConfigResponse'
  { -- | The origin access identity\'s configuration information.
    cloudFrontOriginAccessIdentityConfig :: Prelude.Maybe CloudFrontOriginAccessIdentityConfig,
    -- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCloudFrontOriginAccessIdentityConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudFrontOriginAccessIdentityConfig', 'getCloudFrontOriginAccessIdentityConfigResponse_cloudFrontOriginAccessIdentityConfig' - The origin access identity\'s configuration information.
--
-- 'eTag', 'getCloudFrontOriginAccessIdentityConfigResponse_eTag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
--
-- 'httpStatus', 'getCloudFrontOriginAccessIdentityConfigResponse_httpStatus' - The response's http status code.
newGetCloudFrontOriginAccessIdentityConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCloudFrontOriginAccessIdentityConfigResponse
newGetCloudFrontOriginAccessIdentityConfigResponse
  pHttpStatus_ =
    GetCloudFrontOriginAccessIdentityConfigResponse'
      { cloudFrontOriginAccessIdentityConfig =
          Prelude.Nothing,
        eTag = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The origin access identity\'s configuration information.
getCloudFrontOriginAccessIdentityConfigResponse_cloudFrontOriginAccessIdentityConfig :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Prelude.Maybe CloudFrontOriginAccessIdentityConfig)
getCloudFrontOriginAccessIdentityConfigResponse_cloudFrontOriginAccessIdentityConfig = Lens.lens (\GetCloudFrontOriginAccessIdentityConfigResponse' {cloudFrontOriginAccessIdentityConfig} -> cloudFrontOriginAccessIdentityConfig) (\s@GetCloudFrontOriginAccessIdentityConfigResponse' {} a -> s {cloudFrontOriginAccessIdentityConfig = a} :: GetCloudFrontOriginAccessIdentityConfigResponse)

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
getCloudFrontOriginAccessIdentityConfigResponse_eTag :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Prelude.Maybe Prelude.Text)
getCloudFrontOriginAccessIdentityConfigResponse_eTag = Lens.lens (\GetCloudFrontOriginAccessIdentityConfigResponse' {eTag} -> eTag) (\s@GetCloudFrontOriginAccessIdentityConfigResponse' {} a -> s {eTag = a} :: GetCloudFrontOriginAccessIdentityConfigResponse)

-- | The response's http status code.
getCloudFrontOriginAccessIdentityConfigResponse_httpStatus :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfigResponse Prelude.Int
getCloudFrontOriginAccessIdentityConfigResponse_httpStatus = Lens.lens (\GetCloudFrontOriginAccessIdentityConfigResponse' {httpStatus} -> httpStatus) (\s@GetCloudFrontOriginAccessIdentityConfigResponse' {} a -> s {httpStatus = a} :: GetCloudFrontOriginAccessIdentityConfigResponse)

instance
  Prelude.NFData
    GetCloudFrontOriginAccessIdentityConfigResponse
  where
  rnf
    GetCloudFrontOriginAccessIdentityConfigResponse' {..} =
      Prelude.rnf cloudFrontOriginAccessIdentityConfig `Prelude.seq`
        Prelude.rnf eTag `Prelude.seq`
          Prelude.rnf httpStatus
