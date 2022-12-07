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
-- Module      : Amazonka.CloudFront.GetCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the information about an origin access identity.
module Amazonka.CloudFront.GetCloudFrontOriginAccessIdentity
  ( -- * Creating a Request
    GetCloudFrontOriginAccessIdentity (..),
    newGetCloudFrontOriginAccessIdentity,

    -- * Request Lenses
    getCloudFrontOriginAccessIdentity_id,

    -- * Destructuring the Response
    GetCloudFrontOriginAccessIdentityResponse (..),
    newGetCloudFrontOriginAccessIdentityResponse,

    -- * Response Lenses
    getCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity,
    getCloudFrontOriginAccessIdentityResponse_eTag,
    getCloudFrontOriginAccessIdentityResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to get an origin access identity\'s information.
--
-- /See:/ 'newGetCloudFrontOriginAccessIdentity' smart constructor.
data GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentity'
  { -- | The identity\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetCloudFrontOriginAccessIdentity
newGetCloudFrontOriginAccessIdentity pId_ =
  GetCloudFrontOriginAccessIdentity' {id = pId_}

-- | The identity\'s ID.
getCloudFrontOriginAccessIdentity_id :: Lens.Lens' GetCloudFrontOriginAccessIdentity Prelude.Text
getCloudFrontOriginAccessIdentity_id = Lens.lens (\GetCloudFrontOriginAccessIdentity' {id} -> id) (\s@GetCloudFrontOriginAccessIdentity' {} a -> s {id = a} :: GetCloudFrontOriginAccessIdentity)

instance
  Core.AWSRequest
    GetCloudFrontOriginAccessIdentity
  where
  type
    AWSResponse GetCloudFrontOriginAccessIdentity =
      GetCloudFrontOriginAccessIdentityResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetCloudFrontOriginAccessIdentityResponse'
            Prelude.<$> (Data.parseXML x) Prelude.<*> (h Data..#? "ETag")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetCloudFrontOriginAccessIdentity
  where
  hashWithSalt
    _salt
    GetCloudFrontOriginAccessIdentity' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    GetCloudFrontOriginAccessIdentity
  where
  rnf GetCloudFrontOriginAccessIdentity' {..} =
    Prelude.rnf id

instance
  Data.ToHeaders
    GetCloudFrontOriginAccessIdentity
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetCloudFrontOriginAccessIdentity
  where
  toPath GetCloudFrontOriginAccessIdentity' {..} =
    Prelude.mconcat
      [ "/2020-05-31/origin-access-identity/cloudfront/",
        Data.toBS id
      ]

instance
  Data.ToQuery
    GetCloudFrontOriginAccessIdentity
  where
  toQuery = Prelude.const Prelude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newGetCloudFrontOriginAccessIdentityResponse' smart constructor.
data GetCloudFrontOriginAccessIdentityResponse = GetCloudFrontOriginAccessIdentityResponse'
  { -- | The origin access identity\'s information.
    cloudFrontOriginAccessIdentity :: Prelude.Maybe CloudFrontOriginAccessIdentity,
    -- | The current version of the origin access identity\'s information. For
    -- example: @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCloudFrontOriginAccessIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudFrontOriginAccessIdentity', 'getCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity' - The origin access identity\'s information.
--
-- 'eTag', 'getCloudFrontOriginAccessIdentityResponse_eTag' - The current version of the origin access identity\'s information. For
-- example: @E2QWRUHAPOMQZL@.
--
-- 'httpStatus', 'getCloudFrontOriginAccessIdentityResponse_httpStatus' - The response's http status code.
newGetCloudFrontOriginAccessIdentityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCloudFrontOriginAccessIdentityResponse
newGetCloudFrontOriginAccessIdentityResponse
  pHttpStatus_ =
    GetCloudFrontOriginAccessIdentityResponse'
      { cloudFrontOriginAccessIdentity =
          Prelude.Nothing,
        eTag = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The origin access identity\'s information.
getCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity :: Lens.Lens' GetCloudFrontOriginAccessIdentityResponse (Prelude.Maybe CloudFrontOriginAccessIdentity)
getCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity = Lens.lens (\GetCloudFrontOriginAccessIdentityResponse' {cloudFrontOriginAccessIdentity} -> cloudFrontOriginAccessIdentity) (\s@GetCloudFrontOriginAccessIdentityResponse' {} a -> s {cloudFrontOriginAccessIdentity = a} :: GetCloudFrontOriginAccessIdentityResponse)

-- | The current version of the origin access identity\'s information. For
-- example: @E2QWRUHAPOMQZL@.
getCloudFrontOriginAccessIdentityResponse_eTag :: Lens.Lens' GetCloudFrontOriginAccessIdentityResponse (Prelude.Maybe Prelude.Text)
getCloudFrontOriginAccessIdentityResponse_eTag = Lens.lens (\GetCloudFrontOriginAccessIdentityResponse' {eTag} -> eTag) (\s@GetCloudFrontOriginAccessIdentityResponse' {} a -> s {eTag = a} :: GetCloudFrontOriginAccessIdentityResponse)

-- | The response's http status code.
getCloudFrontOriginAccessIdentityResponse_httpStatus :: Lens.Lens' GetCloudFrontOriginAccessIdentityResponse Prelude.Int
getCloudFrontOriginAccessIdentityResponse_httpStatus = Lens.lens (\GetCloudFrontOriginAccessIdentityResponse' {httpStatus} -> httpStatus) (\s@GetCloudFrontOriginAccessIdentityResponse' {} a -> s {httpStatus = a} :: GetCloudFrontOriginAccessIdentityResponse)

instance
  Prelude.NFData
    GetCloudFrontOriginAccessIdentityResponse
  where
  rnf GetCloudFrontOriginAccessIdentityResponse' {..} =
    Prelude.rnf cloudFrontOriginAccessIdentity
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
