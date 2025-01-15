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
-- Module      : Amazonka.CloudFront.GetPublicKeyConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a public key configuration.
module Amazonka.CloudFront.GetPublicKeyConfig
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

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPublicKeyConfig' smart constructor.
data GetPublicKeyConfig = GetPublicKeyConfig'
  { -- | The identifier of the public key whose configuration you are getting.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetPublicKeyConfig
newGetPublicKeyConfig pId_ =
  GetPublicKeyConfig' {id = pId_}

-- | The identifier of the public key whose configuration you are getting.
getPublicKeyConfig_id :: Lens.Lens' GetPublicKeyConfig Prelude.Text
getPublicKeyConfig_id = Lens.lens (\GetPublicKeyConfig' {id} -> id) (\s@GetPublicKeyConfig' {} a -> s {id = a} :: GetPublicKeyConfig)

instance Core.AWSRequest GetPublicKeyConfig where
  type
    AWSResponse GetPublicKeyConfig =
      GetPublicKeyConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetPublicKeyConfigResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPublicKeyConfig where
  hashWithSalt _salt GetPublicKeyConfig' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetPublicKeyConfig where
  rnf GetPublicKeyConfig' {..} = Prelude.rnf id

instance Data.ToHeaders GetPublicKeyConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetPublicKeyConfig where
  toPath GetPublicKeyConfig' {..} =
    Prelude.mconcat
      ["/2020-05-31/public-key/", Data.toBS id, "/config"]

instance Data.ToQuery GetPublicKeyConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPublicKeyConfigResponse' smart constructor.
data GetPublicKeyConfigResponse = GetPublicKeyConfigResponse'
  { -- | The identifier for this version of the public key configuration.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | A public key configuration.
    publicKeyConfig :: Prelude.Maybe PublicKeyConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetPublicKeyConfigResponse
newGetPublicKeyConfigResponse pHttpStatus_ =
  GetPublicKeyConfigResponse'
    { eTag = Prelude.Nothing,
      publicKeyConfig = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for this version of the public key configuration.
getPublicKeyConfigResponse_eTag :: Lens.Lens' GetPublicKeyConfigResponse (Prelude.Maybe Prelude.Text)
getPublicKeyConfigResponse_eTag = Lens.lens (\GetPublicKeyConfigResponse' {eTag} -> eTag) (\s@GetPublicKeyConfigResponse' {} a -> s {eTag = a} :: GetPublicKeyConfigResponse)

-- | A public key configuration.
getPublicKeyConfigResponse_publicKeyConfig :: Lens.Lens' GetPublicKeyConfigResponse (Prelude.Maybe PublicKeyConfig)
getPublicKeyConfigResponse_publicKeyConfig = Lens.lens (\GetPublicKeyConfigResponse' {publicKeyConfig} -> publicKeyConfig) (\s@GetPublicKeyConfigResponse' {} a -> s {publicKeyConfig = a} :: GetPublicKeyConfigResponse)

-- | The response's http status code.
getPublicKeyConfigResponse_httpStatus :: Lens.Lens' GetPublicKeyConfigResponse Prelude.Int
getPublicKeyConfigResponse_httpStatus = Lens.lens (\GetPublicKeyConfigResponse' {httpStatus} -> httpStatus) (\s@GetPublicKeyConfigResponse' {} a -> s {httpStatus = a} :: GetPublicKeyConfigResponse)

instance Prelude.NFData GetPublicKeyConfigResponse where
  rnf GetPublicKeyConfigResponse' {..} =
    Prelude.rnf eTag `Prelude.seq`
      Prelude.rnf publicKeyConfig `Prelude.seq`
        Prelude.rnf httpStatus
