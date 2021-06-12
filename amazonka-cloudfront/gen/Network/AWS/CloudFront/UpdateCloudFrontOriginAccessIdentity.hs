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
-- Module      : Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an origin access identity.
module Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
  ( -- * Creating a Request
    UpdateCloudFrontOriginAccessIdentity (..),
    newUpdateCloudFrontOriginAccessIdentity,

    -- * Request Lenses
    updateCloudFrontOriginAccessIdentity_ifMatch,
    updateCloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig,
    updateCloudFrontOriginAccessIdentity_id,

    -- * Destructuring the Response
    UpdateCloudFrontOriginAccessIdentityResponse (..),
    newUpdateCloudFrontOriginAccessIdentityResponse,

    -- * Response Lenses
    updateCloudFrontOriginAccessIdentityResponse_eTag,
    updateCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity,
    updateCloudFrontOriginAccessIdentityResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to update an origin access identity.
--
-- /See:/ 'newUpdateCloudFrontOriginAccessIdentity' smart constructor.
data UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentity'
  { -- | The value of the @ETag@ header that you received when retrieving the
    -- identity\'s configuration. For example: @E2QWRUHAPOMQZL@.
    ifMatch :: Core.Maybe Core.Text,
    -- | The identity\'s configuration information.
    cloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig,
    -- | The identity\'s id.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCloudFrontOriginAccessIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'updateCloudFrontOriginAccessIdentity_ifMatch' - The value of the @ETag@ header that you received when retrieving the
-- identity\'s configuration. For example: @E2QWRUHAPOMQZL@.
--
-- 'cloudFrontOriginAccessIdentityConfig', 'updateCloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig' - The identity\'s configuration information.
--
-- 'id', 'updateCloudFrontOriginAccessIdentity_id' - The identity\'s id.
newUpdateCloudFrontOriginAccessIdentity ::
  -- | 'cloudFrontOriginAccessIdentityConfig'
  CloudFrontOriginAccessIdentityConfig ->
  -- | 'id'
  Core.Text ->
  UpdateCloudFrontOriginAccessIdentity
newUpdateCloudFrontOriginAccessIdentity
  pCloudFrontOriginAccessIdentityConfig_
  pId_ =
    UpdateCloudFrontOriginAccessIdentity'
      { ifMatch =
          Core.Nothing,
        cloudFrontOriginAccessIdentityConfig =
          pCloudFrontOriginAccessIdentityConfig_,
        id = pId_
      }

-- | The value of the @ETag@ header that you received when retrieving the
-- identity\'s configuration. For example: @E2QWRUHAPOMQZL@.
updateCloudFrontOriginAccessIdentity_ifMatch :: Lens.Lens' UpdateCloudFrontOriginAccessIdentity (Core.Maybe Core.Text)
updateCloudFrontOriginAccessIdentity_ifMatch = Lens.lens (\UpdateCloudFrontOriginAccessIdentity' {ifMatch} -> ifMatch) (\s@UpdateCloudFrontOriginAccessIdentity' {} a -> s {ifMatch = a} :: UpdateCloudFrontOriginAccessIdentity)

-- | The identity\'s configuration information.
updateCloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig :: Lens.Lens' UpdateCloudFrontOriginAccessIdentity CloudFrontOriginAccessIdentityConfig
updateCloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig = Lens.lens (\UpdateCloudFrontOriginAccessIdentity' {cloudFrontOriginAccessIdentityConfig} -> cloudFrontOriginAccessIdentityConfig) (\s@UpdateCloudFrontOriginAccessIdentity' {} a -> s {cloudFrontOriginAccessIdentityConfig = a} :: UpdateCloudFrontOriginAccessIdentity)

-- | The identity\'s id.
updateCloudFrontOriginAccessIdentity_id :: Lens.Lens' UpdateCloudFrontOriginAccessIdentity Core.Text
updateCloudFrontOriginAccessIdentity_id = Lens.lens (\UpdateCloudFrontOriginAccessIdentity' {id} -> id) (\s@UpdateCloudFrontOriginAccessIdentity' {} a -> s {id = a} :: UpdateCloudFrontOriginAccessIdentity)

instance
  Core.AWSRequest
    UpdateCloudFrontOriginAccessIdentity
  where
  type
    AWSResponse UpdateCloudFrontOriginAccessIdentity =
      UpdateCloudFrontOriginAccessIdentityResponse
  request = Request.putXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateCloudFrontOriginAccessIdentityResponse'
            Core.<$> (h Core..#? "ETag") Core.<*> (Core.parseXML x)
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    UpdateCloudFrontOriginAccessIdentity

instance
  Core.NFData
    UpdateCloudFrontOriginAccessIdentity

instance
  Core.ToElement
    UpdateCloudFrontOriginAccessIdentity
  where
  toElement UpdateCloudFrontOriginAccessIdentity' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CloudFrontOriginAccessIdentityConfig"
      cloudFrontOriginAccessIdentityConfig

instance
  Core.ToHeaders
    UpdateCloudFrontOriginAccessIdentity
  where
  toHeaders UpdateCloudFrontOriginAccessIdentity' {..} =
    Core.mconcat ["If-Match" Core.=# ifMatch]

instance
  Core.ToPath
    UpdateCloudFrontOriginAccessIdentity
  where
  toPath UpdateCloudFrontOriginAccessIdentity' {..} =
    Core.mconcat
      [ "/2020-05-31/origin-access-identity/cloudfront/",
        Core.toBS id,
        "/config"
      ]

instance
  Core.ToQuery
    UpdateCloudFrontOriginAccessIdentity
  where
  toQuery = Core.const Core.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newUpdateCloudFrontOriginAccessIdentityResponse' smart constructor.
data UpdateCloudFrontOriginAccessIdentityResponse = UpdateCloudFrontOriginAccessIdentityResponse'
  { -- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
    eTag :: Core.Maybe Core.Text,
    -- | The origin access identity\'s information.
    cloudFrontOriginAccessIdentity :: Core.Maybe CloudFrontOriginAccessIdentity,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCloudFrontOriginAccessIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'updateCloudFrontOriginAccessIdentityResponse_eTag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
--
-- 'cloudFrontOriginAccessIdentity', 'updateCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity' - The origin access identity\'s information.
--
-- 'httpStatus', 'updateCloudFrontOriginAccessIdentityResponse_httpStatus' - The response's http status code.
newUpdateCloudFrontOriginAccessIdentityResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateCloudFrontOriginAccessIdentityResponse
newUpdateCloudFrontOriginAccessIdentityResponse
  pHttpStatus_ =
    UpdateCloudFrontOriginAccessIdentityResponse'
      { eTag =
          Core.Nothing,
        cloudFrontOriginAccessIdentity =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
updateCloudFrontOriginAccessIdentityResponse_eTag :: Lens.Lens' UpdateCloudFrontOriginAccessIdentityResponse (Core.Maybe Core.Text)
updateCloudFrontOriginAccessIdentityResponse_eTag = Lens.lens (\UpdateCloudFrontOriginAccessIdentityResponse' {eTag} -> eTag) (\s@UpdateCloudFrontOriginAccessIdentityResponse' {} a -> s {eTag = a} :: UpdateCloudFrontOriginAccessIdentityResponse)

-- | The origin access identity\'s information.
updateCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity :: Lens.Lens' UpdateCloudFrontOriginAccessIdentityResponse (Core.Maybe CloudFrontOriginAccessIdentity)
updateCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity = Lens.lens (\UpdateCloudFrontOriginAccessIdentityResponse' {cloudFrontOriginAccessIdentity} -> cloudFrontOriginAccessIdentity) (\s@UpdateCloudFrontOriginAccessIdentityResponse' {} a -> s {cloudFrontOriginAccessIdentity = a} :: UpdateCloudFrontOriginAccessIdentityResponse)

-- | The response's http status code.
updateCloudFrontOriginAccessIdentityResponse_httpStatus :: Lens.Lens' UpdateCloudFrontOriginAccessIdentityResponse Core.Int
updateCloudFrontOriginAccessIdentityResponse_httpStatus = Lens.lens (\UpdateCloudFrontOriginAccessIdentityResponse' {httpStatus} -> httpStatus) (\s@UpdateCloudFrontOriginAccessIdentityResponse' {} a -> s {httpStatus = a} :: UpdateCloudFrontOriginAccessIdentityResponse)

instance
  Core.NFData
    UpdateCloudFrontOriginAccessIdentityResponse
