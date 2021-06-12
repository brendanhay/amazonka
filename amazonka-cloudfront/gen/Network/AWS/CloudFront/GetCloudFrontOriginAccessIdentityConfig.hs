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
-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the configuration information about an origin access identity.
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
  ( -- * Creating a Request
    GetCloudFrontOriginAccessIdentityConfig (..),
    newGetCloudFrontOriginAccessIdentityConfig,

    -- * Request Lenses
    getCloudFrontOriginAccessIdentityConfig_id,

    -- * Destructuring the Response
    GetCloudFrontOriginAccessIdentityConfigResponse (..),
    newGetCloudFrontOriginAccessIdentityConfigResponse,

    -- * Response Lenses
    getCloudFrontOriginAccessIdentityConfigResponse_eTag,
    getCloudFrontOriginAccessIdentityConfigResponse_cloudFrontOriginAccessIdentityConfig,
    getCloudFrontOriginAccessIdentityConfigResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The origin access identity\'s configuration information. For more
-- information, see
-- <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_CloudFrontOriginAccessIdentityConfig.html CloudFrontOriginAccessIdentityConfig>.
--
-- /See:/ 'newGetCloudFrontOriginAccessIdentityConfig' smart constructor.
data GetCloudFrontOriginAccessIdentityConfig = GetCloudFrontOriginAccessIdentityConfig'
  { -- | The identity\'s ID.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetCloudFrontOriginAccessIdentityConfig
newGetCloudFrontOriginAccessIdentityConfig pId_ =
  GetCloudFrontOriginAccessIdentityConfig' {id = pId_}

-- | The identity\'s ID.
getCloudFrontOriginAccessIdentityConfig_id :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfig Core.Text
getCloudFrontOriginAccessIdentityConfig_id = Lens.lens (\GetCloudFrontOriginAccessIdentityConfig' {id} -> id) (\s@GetCloudFrontOriginAccessIdentityConfig' {} a -> s {id = a} :: GetCloudFrontOriginAccessIdentityConfig)

instance
  Core.AWSRequest
    GetCloudFrontOriginAccessIdentityConfig
  where
  type
    AWSResponse
      GetCloudFrontOriginAccessIdentityConfig =
      GetCloudFrontOriginAccessIdentityConfigResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetCloudFrontOriginAccessIdentityConfigResponse'
            Core.<$> (h Core..#? "ETag") Core.<*> (Core.parseXML x)
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetCloudFrontOriginAccessIdentityConfig

instance
  Core.NFData
    GetCloudFrontOriginAccessIdentityConfig

instance
  Core.ToHeaders
    GetCloudFrontOriginAccessIdentityConfig
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    GetCloudFrontOriginAccessIdentityConfig
  where
  toPath GetCloudFrontOriginAccessIdentityConfig' {..} =
    Core.mconcat
      [ "/2020-05-31/origin-access-identity/cloudfront/",
        Core.toBS id,
        "/config"
      ]

instance
  Core.ToQuery
    GetCloudFrontOriginAccessIdentityConfig
  where
  toQuery = Core.const Core.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newGetCloudFrontOriginAccessIdentityConfigResponse' smart constructor.
data GetCloudFrontOriginAccessIdentityConfigResponse = GetCloudFrontOriginAccessIdentityConfigResponse'
  { -- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
    eTag :: Core.Maybe Core.Text,
    -- | The origin access identity\'s configuration information.
    cloudFrontOriginAccessIdentityConfig :: Core.Maybe CloudFrontOriginAccessIdentityConfig,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCloudFrontOriginAccessIdentityConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getCloudFrontOriginAccessIdentityConfigResponse_eTag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
--
-- 'cloudFrontOriginAccessIdentityConfig', 'getCloudFrontOriginAccessIdentityConfigResponse_cloudFrontOriginAccessIdentityConfig' - The origin access identity\'s configuration information.
--
-- 'httpStatus', 'getCloudFrontOriginAccessIdentityConfigResponse_httpStatus' - The response's http status code.
newGetCloudFrontOriginAccessIdentityConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetCloudFrontOriginAccessIdentityConfigResponse
newGetCloudFrontOriginAccessIdentityConfigResponse
  pHttpStatus_ =
    GetCloudFrontOriginAccessIdentityConfigResponse'
      { eTag =
          Core.Nothing,
        cloudFrontOriginAccessIdentityConfig =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
getCloudFrontOriginAccessIdentityConfigResponse_eTag :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Core.Maybe Core.Text)
getCloudFrontOriginAccessIdentityConfigResponse_eTag = Lens.lens (\GetCloudFrontOriginAccessIdentityConfigResponse' {eTag} -> eTag) (\s@GetCloudFrontOriginAccessIdentityConfigResponse' {} a -> s {eTag = a} :: GetCloudFrontOriginAccessIdentityConfigResponse)

-- | The origin access identity\'s configuration information.
getCloudFrontOriginAccessIdentityConfigResponse_cloudFrontOriginAccessIdentityConfig :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Core.Maybe CloudFrontOriginAccessIdentityConfig)
getCloudFrontOriginAccessIdentityConfigResponse_cloudFrontOriginAccessIdentityConfig = Lens.lens (\GetCloudFrontOriginAccessIdentityConfigResponse' {cloudFrontOriginAccessIdentityConfig} -> cloudFrontOriginAccessIdentityConfig) (\s@GetCloudFrontOriginAccessIdentityConfigResponse' {} a -> s {cloudFrontOriginAccessIdentityConfig = a} :: GetCloudFrontOriginAccessIdentityConfigResponse)

-- | The response's http status code.
getCloudFrontOriginAccessIdentityConfigResponse_httpStatus :: Lens.Lens' GetCloudFrontOriginAccessIdentityConfigResponse Core.Int
getCloudFrontOriginAccessIdentityConfigResponse_httpStatus = Lens.lens (\GetCloudFrontOriginAccessIdentityConfigResponse' {httpStatus} -> httpStatus) (\s@GetCloudFrontOriginAccessIdentityConfigResponse' {} a -> s {httpStatus = a} :: GetCloudFrontOriginAccessIdentityConfigResponse)

instance
  Core.NFData
    GetCloudFrontOriginAccessIdentityConfigResponse
