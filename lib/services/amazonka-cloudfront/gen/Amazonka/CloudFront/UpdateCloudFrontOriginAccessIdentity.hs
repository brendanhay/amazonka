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
-- Module      : Amazonka.CloudFront.UpdateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an origin access identity.
module Amazonka.CloudFront.UpdateCloudFrontOriginAccessIdentity
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
    updateCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity,
    updateCloudFrontOriginAccessIdentityResponse_eTag,
    updateCloudFrontOriginAccessIdentityResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to update an origin access identity.
--
-- /See:/ 'newUpdateCloudFrontOriginAccessIdentity' smart constructor.
data UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentity'
  { -- | The value of the @ETag@ header that you received when retrieving the
    -- identity\'s configuration. For example: @E2QWRUHAPOMQZL@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The identity\'s configuration information.
    cloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig,
    -- | The identity\'s id.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdateCloudFrontOriginAccessIdentity
newUpdateCloudFrontOriginAccessIdentity
  pCloudFrontOriginAccessIdentityConfig_
  pId_ =
    UpdateCloudFrontOriginAccessIdentity'
      { ifMatch =
          Prelude.Nothing,
        cloudFrontOriginAccessIdentityConfig =
          pCloudFrontOriginAccessIdentityConfig_,
        id = pId_
      }

-- | The value of the @ETag@ header that you received when retrieving the
-- identity\'s configuration. For example: @E2QWRUHAPOMQZL@.
updateCloudFrontOriginAccessIdentity_ifMatch :: Lens.Lens' UpdateCloudFrontOriginAccessIdentity (Prelude.Maybe Prelude.Text)
updateCloudFrontOriginAccessIdentity_ifMatch = Lens.lens (\UpdateCloudFrontOriginAccessIdentity' {ifMatch} -> ifMatch) (\s@UpdateCloudFrontOriginAccessIdentity' {} a -> s {ifMatch = a} :: UpdateCloudFrontOriginAccessIdentity)

-- | The identity\'s configuration information.
updateCloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig :: Lens.Lens' UpdateCloudFrontOriginAccessIdentity CloudFrontOriginAccessIdentityConfig
updateCloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig = Lens.lens (\UpdateCloudFrontOriginAccessIdentity' {cloudFrontOriginAccessIdentityConfig} -> cloudFrontOriginAccessIdentityConfig) (\s@UpdateCloudFrontOriginAccessIdentity' {} a -> s {cloudFrontOriginAccessIdentityConfig = a} :: UpdateCloudFrontOriginAccessIdentity)

-- | The identity\'s id.
updateCloudFrontOriginAccessIdentity_id :: Lens.Lens' UpdateCloudFrontOriginAccessIdentity Prelude.Text
updateCloudFrontOriginAccessIdentity_id = Lens.lens (\UpdateCloudFrontOriginAccessIdentity' {id} -> id) (\s@UpdateCloudFrontOriginAccessIdentity' {} a -> s {id = a} :: UpdateCloudFrontOriginAccessIdentity)

instance
  Core.AWSRequest
    UpdateCloudFrontOriginAccessIdentity
  where
  type
    AWSResponse UpdateCloudFrontOriginAccessIdentity =
      UpdateCloudFrontOriginAccessIdentityResponse
  request overrides =
    Request.putXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateCloudFrontOriginAccessIdentityResponse'
            Prelude.<$> (Data.parseXML x) Prelude.<*> (h Data..#? "ETag")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateCloudFrontOriginAccessIdentity
  where
  hashWithSalt
    _salt
    UpdateCloudFrontOriginAccessIdentity' {..} =
      _salt `Prelude.hashWithSalt` ifMatch
        `Prelude.hashWithSalt` cloudFrontOriginAccessIdentityConfig
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    UpdateCloudFrontOriginAccessIdentity
  where
  rnf UpdateCloudFrontOriginAccessIdentity' {..} =
    Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf cloudFrontOriginAccessIdentityConfig
      `Prelude.seq` Prelude.rnf id

instance
  Data.ToElement
    UpdateCloudFrontOriginAccessIdentity
  where
  toElement UpdateCloudFrontOriginAccessIdentity' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CloudFrontOriginAccessIdentityConfig"
      cloudFrontOriginAccessIdentityConfig

instance
  Data.ToHeaders
    UpdateCloudFrontOriginAccessIdentity
  where
  toHeaders UpdateCloudFrontOriginAccessIdentity' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance
  Data.ToPath
    UpdateCloudFrontOriginAccessIdentity
  where
  toPath UpdateCloudFrontOriginAccessIdentity' {..} =
    Prelude.mconcat
      [ "/2020-05-31/origin-access-identity/cloudfront/",
        Data.toBS id,
        "/config"
      ]

instance
  Data.ToQuery
    UpdateCloudFrontOriginAccessIdentity
  where
  toQuery = Prelude.const Prelude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newUpdateCloudFrontOriginAccessIdentityResponse' smart constructor.
data UpdateCloudFrontOriginAccessIdentityResponse = UpdateCloudFrontOriginAccessIdentityResponse'
  { -- | The origin access identity\'s information.
    cloudFrontOriginAccessIdentity :: Prelude.Maybe CloudFrontOriginAccessIdentity,
    -- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCloudFrontOriginAccessIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudFrontOriginAccessIdentity', 'updateCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity' - The origin access identity\'s information.
--
-- 'eTag', 'updateCloudFrontOriginAccessIdentityResponse_eTag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
--
-- 'httpStatus', 'updateCloudFrontOriginAccessIdentityResponse_httpStatus' - The response's http status code.
newUpdateCloudFrontOriginAccessIdentityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCloudFrontOriginAccessIdentityResponse
newUpdateCloudFrontOriginAccessIdentityResponse
  pHttpStatus_ =
    UpdateCloudFrontOriginAccessIdentityResponse'
      { cloudFrontOriginAccessIdentity =
          Prelude.Nothing,
        eTag = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The origin access identity\'s information.
updateCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity :: Lens.Lens' UpdateCloudFrontOriginAccessIdentityResponse (Prelude.Maybe CloudFrontOriginAccessIdentity)
updateCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity = Lens.lens (\UpdateCloudFrontOriginAccessIdentityResponse' {cloudFrontOriginAccessIdentity} -> cloudFrontOriginAccessIdentity) (\s@UpdateCloudFrontOriginAccessIdentityResponse' {} a -> s {cloudFrontOriginAccessIdentity = a} :: UpdateCloudFrontOriginAccessIdentityResponse)

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
updateCloudFrontOriginAccessIdentityResponse_eTag :: Lens.Lens' UpdateCloudFrontOriginAccessIdentityResponse (Prelude.Maybe Prelude.Text)
updateCloudFrontOriginAccessIdentityResponse_eTag = Lens.lens (\UpdateCloudFrontOriginAccessIdentityResponse' {eTag} -> eTag) (\s@UpdateCloudFrontOriginAccessIdentityResponse' {} a -> s {eTag = a} :: UpdateCloudFrontOriginAccessIdentityResponse)

-- | The response's http status code.
updateCloudFrontOriginAccessIdentityResponse_httpStatus :: Lens.Lens' UpdateCloudFrontOriginAccessIdentityResponse Prelude.Int
updateCloudFrontOriginAccessIdentityResponse_httpStatus = Lens.lens (\UpdateCloudFrontOriginAccessIdentityResponse' {httpStatus} -> httpStatus) (\s@UpdateCloudFrontOriginAccessIdentityResponse' {} a -> s {httpStatus = a} :: UpdateCloudFrontOriginAccessIdentityResponse)

instance
  Prelude.NFData
    UpdateCloudFrontOriginAccessIdentityResponse
  where
  rnf UpdateCloudFrontOriginAccessIdentityResponse' {..} =
    Prelude.rnf cloudFrontOriginAccessIdentity
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
