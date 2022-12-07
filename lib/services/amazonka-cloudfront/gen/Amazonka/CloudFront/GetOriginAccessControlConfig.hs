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
-- Module      : Amazonka.CloudFront.GetOriginAccessControlConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a CloudFront origin access control configuration.
module Amazonka.CloudFront.GetOriginAccessControlConfig
  ( -- * Creating a Request
    GetOriginAccessControlConfig (..),
    newGetOriginAccessControlConfig,

    -- * Request Lenses
    getOriginAccessControlConfig_id,

    -- * Destructuring the Response
    GetOriginAccessControlConfigResponse (..),
    newGetOriginAccessControlConfigResponse,

    -- * Response Lenses
    getOriginAccessControlConfigResponse_originAccessControlConfig,
    getOriginAccessControlConfigResponse_eTag,
    getOriginAccessControlConfigResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetOriginAccessControlConfig' smart constructor.
data GetOriginAccessControlConfig = GetOriginAccessControlConfig'
  { -- | The unique identifier of the origin access control.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOriginAccessControlConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getOriginAccessControlConfig_id' - The unique identifier of the origin access control.
newGetOriginAccessControlConfig ::
  -- | 'id'
  Prelude.Text ->
  GetOriginAccessControlConfig
newGetOriginAccessControlConfig pId_ =
  GetOriginAccessControlConfig' {id = pId_}

-- | The unique identifier of the origin access control.
getOriginAccessControlConfig_id :: Lens.Lens' GetOriginAccessControlConfig Prelude.Text
getOriginAccessControlConfig_id = Lens.lens (\GetOriginAccessControlConfig' {id} -> id) (\s@GetOriginAccessControlConfig' {} a -> s {id = a} :: GetOriginAccessControlConfig)

instance Core.AWSRequest GetOriginAccessControlConfig where
  type
    AWSResponse GetOriginAccessControlConfig =
      GetOriginAccessControlConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetOriginAccessControlConfigResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetOriginAccessControlConfig
  where
  hashWithSalt _salt GetOriginAccessControlConfig' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetOriginAccessControlConfig where
  rnf GetOriginAccessControlConfig' {..} =
    Prelude.rnf id

instance Data.ToHeaders GetOriginAccessControlConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetOriginAccessControlConfig where
  toPath GetOriginAccessControlConfig' {..} =
    Prelude.mconcat
      [ "/2020-05-31/origin-access-control/",
        Data.toBS id,
        "/config"
      ]

instance Data.ToQuery GetOriginAccessControlConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOriginAccessControlConfigResponse' smart constructor.
data GetOriginAccessControlConfigResponse = GetOriginAccessControlConfigResponse'
  { -- | Contains an origin access control configuration.
    originAccessControlConfig :: Prelude.Maybe OriginAccessControlConfig,
    -- | The version identifier for the current version of the origin access
    -- control.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOriginAccessControlConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originAccessControlConfig', 'getOriginAccessControlConfigResponse_originAccessControlConfig' - Contains an origin access control configuration.
--
-- 'eTag', 'getOriginAccessControlConfigResponse_eTag' - The version identifier for the current version of the origin access
-- control.
--
-- 'httpStatus', 'getOriginAccessControlConfigResponse_httpStatus' - The response's http status code.
newGetOriginAccessControlConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOriginAccessControlConfigResponse
newGetOriginAccessControlConfigResponse pHttpStatus_ =
  GetOriginAccessControlConfigResponse'
    { originAccessControlConfig =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains an origin access control configuration.
getOriginAccessControlConfigResponse_originAccessControlConfig :: Lens.Lens' GetOriginAccessControlConfigResponse (Prelude.Maybe OriginAccessControlConfig)
getOriginAccessControlConfigResponse_originAccessControlConfig = Lens.lens (\GetOriginAccessControlConfigResponse' {originAccessControlConfig} -> originAccessControlConfig) (\s@GetOriginAccessControlConfigResponse' {} a -> s {originAccessControlConfig = a} :: GetOriginAccessControlConfigResponse)

-- | The version identifier for the current version of the origin access
-- control.
getOriginAccessControlConfigResponse_eTag :: Lens.Lens' GetOriginAccessControlConfigResponse (Prelude.Maybe Prelude.Text)
getOriginAccessControlConfigResponse_eTag = Lens.lens (\GetOriginAccessControlConfigResponse' {eTag} -> eTag) (\s@GetOriginAccessControlConfigResponse' {} a -> s {eTag = a} :: GetOriginAccessControlConfigResponse)

-- | The response's http status code.
getOriginAccessControlConfigResponse_httpStatus :: Lens.Lens' GetOriginAccessControlConfigResponse Prelude.Int
getOriginAccessControlConfigResponse_httpStatus = Lens.lens (\GetOriginAccessControlConfigResponse' {httpStatus} -> httpStatus) (\s@GetOriginAccessControlConfigResponse' {} a -> s {httpStatus = a} :: GetOriginAccessControlConfigResponse)

instance
  Prelude.NFData
    GetOriginAccessControlConfigResponse
  where
  rnf GetOriginAccessControlConfigResponse' {..} =
    Prelude.rnf originAccessControlConfig
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
