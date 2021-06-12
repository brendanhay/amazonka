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
-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the information about an origin access identity.
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
  ( -- * Creating a Request
    GetCloudFrontOriginAccessIdentity (..),
    newGetCloudFrontOriginAccessIdentity,

    -- * Request Lenses
    getCloudFrontOriginAccessIdentity_id,

    -- * Destructuring the Response
    GetCloudFrontOriginAccessIdentityResponse (..),
    newGetCloudFrontOriginAccessIdentityResponse,

    -- * Response Lenses
    getCloudFrontOriginAccessIdentityResponse_eTag,
    getCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity,
    getCloudFrontOriginAccessIdentityResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to get an origin access identity\'s information.
--
-- /See:/ 'newGetCloudFrontOriginAccessIdentity' smart constructor.
data GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentity'
  { -- | The identity\'s ID.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCloudFrontOriginAccessIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getCloudFrontOriginAccessIdentity_id' - The identity\'s ID.
newGetCloudFrontOriginAccessIdentity ::
  -- | 'id'
  Core.Text ->
  GetCloudFrontOriginAccessIdentity
newGetCloudFrontOriginAccessIdentity pId_ =
  GetCloudFrontOriginAccessIdentity' {id = pId_}

-- | The identity\'s ID.
getCloudFrontOriginAccessIdentity_id :: Lens.Lens' GetCloudFrontOriginAccessIdentity Core.Text
getCloudFrontOriginAccessIdentity_id = Lens.lens (\GetCloudFrontOriginAccessIdentity' {id} -> id) (\s@GetCloudFrontOriginAccessIdentity' {} a -> s {id = a} :: GetCloudFrontOriginAccessIdentity)

instance
  Core.AWSRequest
    GetCloudFrontOriginAccessIdentity
  where
  type
    AWSResponse GetCloudFrontOriginAccessIdentity =
      GetCloudFrontOriginAccessIdentityResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetCloudFrontOriginAccessIdentityResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetCloudFrontOriginAccessIdentity

instance
  Core.NFData
    GetCloudFrontOriginAccessIdentity

instance
  Core.ToHeaders
    GetCloudFrontOriginAccessIdentity
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    GetCloudFrontOriginAccessIdentity
  where
  toPath GetCloudFrontOriginAccessIdentity' {..} =
    Core.mconcat
      [ "/2020-05-31/origin-access-identity/cloudfront/",
        Core.toBS id
      ]

instance
  Core.ToQuery
    GetCloudFrontOriginAccessIdentity
  where
  toQuery = Core.const Core.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newGetCloudFrontOriginAccessIdentityResponse' smart constructor.
data GetCloudFrontOriginAccessIdentityResponse = GetCloudFrontOriginAccessIdentityResponse'
  { -- | The current version of the origin access identity\'s information. For
    -- example: @E2QWRUHAPOMQZL@.
    eTag :: Core.Maybe Core.Text,
    -- | The origin access identity\'s information.
    cloudFrontOriginAccessIdentity :: Core.Maybe CloudFrontOriginAccessIdentity,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCloudFrontOriginAccessIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getCloudFrontOriginAccessIdentityResponse_eTag' - The current version of the origin access identity\'s information. For
-- example: @E2QWRUHAPOMQZL@.
--
-- 'cloudFrontOriginAccessIdentity', 'getCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity' - The origin access identity\'s information.
--
-- 'httpStatus', 'getCloudFrontOriginAccessIdentityResponse_httpStatus' - The response's http status code.
newGetCloudFrontOriginAccessIdentityResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetCloudFrontOriginAccessIdentityResponse
newGetCloudFrontOriginAccessIdentityResponse
  pHttpStatus_ =
    GetCloudFrontOriginAccessIdentityResponse'
      { eTag =
          Core.Nothing,
        cloudFrontOriginAccessIdentity =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current version of the origin access identity\'s information. For
-- example: @E2QWRUHAPOMQZL@.
getCloudFrontOriginAccessIdentityResponse_eTag :: Lens.Lens' GetCloudFrontOriginAccessIdentityResponse (Core.Maybe Core.Text)
getCloudFrontOriginAccessIdentityResponse_eTag = Lens.lens (\GetCloudFrontOriginAccessIdentityResponse' {eTag} -> eTag) (\s@GetCloudFrontOriginAccessIdentityResponse' {} a -> s {eTag = a} :: GetCloudFrontOriginAccessIdentityResponse)

-- | The origin access identity\'s information.
getCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity :: Lens.Lens' GetCloudFrontOriginAccessIdentityResponse (Core.Maybe CloudFrontOriginAccessIdentity)
getCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity = Lens.lens (\GetCloudFrontOriginAccessIdentityResponse' {cloudFrontOriginAccessIdentity} -> cloudFrontOriginAccessIdentity) (\s@GetCloudFrontOriginAccessIdentityResponse' {} a -> s {cloudFrontOriginAccessIdentity = a} :: GetCloudFrontOriginAccessIdentityResponse)

-- | The response's http status code.
getCloudFrontOriginAccessIdentityResponse_httpStatus :: Lens.Lens' GetCloudFrontOriginAccessIdentityResponse Core.Int
getCloudFrontOriginAccessIdentityResponse_httpStatus = Lens.lens (\GetCloudFrontOriginAccessIdentityResponse' {httpStatus} -> httpStatus) (\s@GetCloudFrontOriginAccessIdentityResponse' {} a -> s {httpStatus = a} :: GetCloudFrontOriginAccessIdentityResponse)

instance
  Core.NFData
    GetCloudFrontOriginAccessIdentityResponse
