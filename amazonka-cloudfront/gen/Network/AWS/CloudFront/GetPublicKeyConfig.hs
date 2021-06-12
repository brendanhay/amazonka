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
-- Module      : Network.AWS.CloudFront.GetPublicKeyConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a public key configuration.
module Network.AWS.CloudFront.GetPublicKeyConfig
  ( -- * Creating a Request
    GetPublicKeyConfig (..),
    newGetPublicKeyConfig,

    -- * Request Lenses
    getPublicKeyConfig_id,

    -- * Destructuring the Response
    GetPublicKeyConfigResponse (..),
    newGetPublicKeyConfigResponse,

    -- * Response Lenses
    getPublicKeyConfigResponse_eTag,
    getPublicKeyConfigResponse_publicKeyConfig,
    getPublicKeyConfigResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPublicKeyConfig' smart constructor.
data GetPublicKeyConfig = GetPublicKeyConfig'
  { -- | The identifier of the public key whose configuration you are getting.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPublicKeyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getPublicKeyConfig_id' - The identifier of the public key whose configuration you are getting.
newGetPublicKeyConfig ::
  -- | 'id'
  Core.Text ->
  GetPublicKeyConfig
newGetPublicKeyConfig pId_ =
  GetPublicKeyConfig' {id = pId_}

-- | The identifier of the public key whose configuration you are getting.
getPublicKeyConfig_id :: Lens.Lens' GetPublicKeyConfig Core.Text
getPublicKeyConfig_id = Lens.lens (\GetPublicKeyConfig' {id} -> id) (\s@GetPublicKeyConfig' {} a -> s {id = a} :: GetPublicKeyConfig)

instance Core.AWSRequest GetPublicKeyConfig where
  type
    AWSResponse GetPublicKeyConfig =
      GetPublicKeyConfigResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetPublicKeyConfigResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetPublicKeyConfig

instance Core.NFData GetPublicKeyConfig

instance Core.ToHeaders GetPublicKeyConfig where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetPublicKeyConfig where
  toPath GetPublicKeyConfig' {..} =
    Core.mconcat
      ["/2020-05-31/public-key/", Core.toBS id, "/config"]

instance Core.ToQuery GetPublicKeyConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetPublicKeyConfigResponse' smart constructor.
data GetPublicKeyConfigResponse = GetPublicKeyConfigResponse'
  { -- | The identifier for this version of the public key configuration.
    eTag :: Core.Maybe Core.Text,
    -- | A public key configuration.
    publicKeyConfig :: Core.Maybe PublicKeyConfig,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPublicKeyConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getPublicKeyConfigResponse_eTag' - The identifier for this version of the public key configuration.
--
-- 'publicKeyConfig', 'getPublicKeyConfigResponse_publicKeyConfig' - A public key configuration.
--
-- 'httpStatus', 'getPublicKeyConfigResponse_httpStatus' - The response's http status code.
newGetPublicKeyConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetPublicKeyConfigResponse
newGetPublicKeyConfigResponse pHttpStatus_ =
  GetPublicKeyConfigResponse'
    { eTag = Core.Nothing,
      publicKeyConfig = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for this version of the public key configuration.
getPublicKeyConfigResponse_eTag :: Lens.Lens' GetPublicKeyConfigResponse (Core.Maybe Core.Text)
getPublicKeyConfigResponse_eTag = Lens.lens (\GetPublicKeyConfigResponse' {eTag} -> eTag) (\s@GetPublicKeyConfigResponse' {} a -> s {eTag = a} :: GetPublicKeyConfigResponse)

-- | A public key configuration.
getPublicKeyConfigResponse_publicKeyConfig :: Lens.Lens' GetPublicKeyConfigResponse (Core.Maybe PublicKeyConfig)
getPublicKeyConfigResponse_publicKeyConfig = Lens.lens (\GetPublicKeyConfigResponse' {publicKeyConfig} -> publicKeyConfig) (\s@GetPublicKeyConfigResponse' {} a -> s {publicKeyConfig = a} :: GetPublicKeyConfigResponse)

-- | The response's http status code.
getPublicKeyConfigResponse_httpStatus :: Lens.Lens' GetPublicKeyConfigResponse Core.Int
getPublicKeyConfigResponse_httpStatus = Lens.lens (\GetPublicKeyConfigResponse' {httpStatus} -> httpStatus) (\s@GetPublicKeyConfigResponse' {} a -> s {httpStatus = a} :: GetPublicKeyConfigResponse)

instance Core.NFData GetPublicKeyConfigResponse
