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
-- Module      : Network.AWS.CloudFront.GetDistributionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the configuration information about a distribution.
module Network.AWS.CloudFront.GetDistributionConfig
  ( -- * Creating a Request
    GetDistributionConfig (..),
    newGetDistributionConfig,

    -- * Request Lenses
    getDistributionConfig_id,

    -- * Destructuring the Response
    GetDistributionConfigResponse (..),
    newGetDistributionConfigResponse,

    -- * Response Lenses
    getDistributionConfigResponse_eTag,
    getDistributionConfigResponse_distributionConfig,
    getDistributionConfigResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to get a distribution configuration.
--
-- /See:/ 'newGetDistributionConfig' smart constructor.
data GetDistributionConfig = GetDistributionConfig'
  { -- | The distribution\'s ID. If the ID is empty, an empty distribution
    -- configuration is returned.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDistributionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getDistributionConfig_id' - The distribution\'s ID. If the ID is empty, an empty distribution
-- configuration is returned.
newGetDistributionConfig ::
  -- | 'id'
  Core.Text ->
  GetDistributionConfig
newGetDistributionConfig pId_ =
  GetDistributionConfig' {id = pId_}

-- | The distribution\'s ID. If the ID is empty, an empty distribution
-- configuration is returned.
getDistributionConfig_id :: Lens.Lens' GetDistributionConfig Core.Text
getDistributionConfig_id = Lens.lens (\GetDistributionConfig' {id} -> id) (\s@GetDistributionConfig' {} a -> s {id = a} :: GetDistributionConfig)

instance Core.AWSRequest GetDistributionConfig where
  type
    AWSResponse GetDistributionConfig =
      GetDistributionConfigResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetDistributionConfigResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDistributionConfig

instance Core.NFData GetDistributionConfig

instance Core.ToHeaders GetDistributionConfig where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetDistributionConfig where
  toPath GetDistributionConfig' {..} =
    Core.mconcat
      [ "/2020-05-31/distribution/",
        Core.toBS id,
        "/config"
      ]

instance Core.ToQuery GetDistributionConfig where
  toQuery = Core.const Core.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newGetDistributionConfigResponse' smart constructor.
data GetDistributionConfigResponse = GetDistributionConfigResponse'
  { -- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
    eTag :: Core.Maybe Core.Text,
    -- | The distribution\'s configuration information.
    distributionConfig :: Core.Maybe DistributionConfig,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDistributionConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getDistributionConfigResponse_eTag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
--
-- 'distributionConfig', 'getDistributionConfigResponse_distributionConfig' - The distribution\'s configuration information.
--
-- 'httpStatus', 'getDistributionConfigResponse_httpStatus' - The response's http status code.
newGetDistributionConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDistributionConfigResponse
newGetDistributionConfigResponse pHttpStatus_ =
  GetDistributionConfigResponse'
    { eTag = Core.Nothing,
      distributionConfig = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
getDistributionConfigResponse_eTag :: Lens.Lens' GetDistributionConfigResponse (Core.Maybe Core.Text)
getDistributionConfigResponse_eTag = Lens.lens (\GetDistributionConfigResponse' {eTag} -> eTag) (\s@GetDistributionConfigResponse' {} a -> s {eTag = a} :: GetDistributionConfigResponse)

-- | The distribution\'s configuration information.
getDistributionConfigResponse_distributionConfig :: Lens.Lens' GetDistributionConfigResponse (Core.Maybe DistributionConfig)
getDistributionConfigResponse_distributionConfig = Lens.lens (\GetDistributionConfigResponse' {distributionConfig} -> distributionConfig) (\s@GetDistributionConfigResponse' {} a -> s {distributionConfig = a} :: GetDistributionConfigResponse)

-- | The response's http status code.
getDistributionConfigResponse_httpStatus :: Lens.Lens' GetDistributionConfigResponse Core.Int
getDistributionConfigResponse_httpStatus = Lens.lens (\GetDistributionConfigResponse' {httpStatus} -> httpStatus) (\s@GetDistributionConfigResponse' {} a -> s {httpStatus = a} :: GetDistributionConfigResponse)

instance Core.NFData GetDistributionConfigResponse
