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
-- Module      : Amazonka.OpenSearchServerless.GetSecurityConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an OpenSearch Serverless security
-- configuration. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-saml.html SAML authentication for Amazon OpenSearch Serverless>.
module Amazonka.OpenSearchServerless.GetSecurityConfig
  ( -- * Creating a Request
    GetSecurityConfig (..),
    newGetSecurityConfig,

    -- * Request Lenses
    getSecurityConfig_id,

    -- * Destructuring the Response
    GetSecurityConfigResponse (..),
    newGetSecurityConfigResponse,

    -- * Response Lenses
    getSecurityConfigResponse_securityConfigDetail,
    getSecurityConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSecurityConfig' smart constructor.
data GetSecurityConfig = GetSecurityConfig'
  { -- | The unique identifier of the security configuration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSecurityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getSecurityConfig_id' - The unique identifier of the security configuration.
newGetSecurityConfig ::
  -- | 'id'
  Prelude.Text ->
  GetSecurityConfig
newGetSecurityConfig pId_ =
  GetSecurityConfig' {id = pId_}

-- | The unique identifier of the security configuration.
getSecurityConfig_id :: Lens.Lens' GetSecurityConfig Prelude.Text
getSecurityConfig_id = Lens.lens (\GetSecurityConfig' {id} -> id) (\s@GetSecurityConfig' {} a -> s {id = a} :: GetSecurityConfig)

instance Core.AWSRequest GetSecurityConfig where
  type
    AWSResponse GetSecurityConfig =
      GetSecurityConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSecurityConfigResponse'
            Prelude.<$> (x Data..?> "securityConfigDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSecurityConfig where
  hashWithSalt _salt GetSecurityConfig' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetSecurityConfig where
  rnf GetSecurityConfig' {..} = Prelude.rnf id

instance Data.ToHeaders GetSecurityConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.GetSecurityConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSecurityConfig where
  toJSON GetSecurityConfig' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("id" Data..= id)])

instance Data.ToPath GetSecurityConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSecurityConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSecurityConfigResponse' smart constructor.
data GetSecurityConfigResponse = GetSecurityConfigResponse'
  { -- | Details of the requested security configuration.
    securityConfigDetail :: Prelude.Maybe SecurityConfigDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSecurityConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityConfigDetail', 'getSecurityConfigResponse_securityConfigDetail' - Details of the requested security configuration.
--
-- 'httpStatus', 'getSecurityConfigResponse_httpStatus' - The response's http status code.
newGetSecurityConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSecurityConfigResponse
newGetSecurityConfigResponse pHttpStatus_ =
  GetSecurityConfigResponse'
    { securityConfigDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details of the requested security configuration.
getSecurityConfigResponse_securityConfigDetail :: Lens.Lens' GetSecurityConfigResponse (Prelude.Maybe SecurityConfigDetail)
getSecurityConfigResponse_securityConfigDetail = Lens.lens (\GetSecurityConfigResponse' {securityConfigDetail} -> securityConfigDetail) (\s@GetSecurityConfigResponse' {} a -> s {securityConfigDetail = a} :: GetSecurityConfigResponse)

-- | The response's http status code.
getSecurityConfigResponse_httpStatus :: Lens.Lens' GetSecurityConfigResponse Prelude.Int
getSecurityConfigResponse_httpStatus = Lens.lens (\GetSecurityConfigResponse' {httpStatus} -> httpStatus) (\s@GetSecurityConfigResponse' {} a -> s {httpStatus = a} :: GetSecurityConfigResponse)

instance Prelude.NFData GetSecurityConfigResponse where
  rnf GetSecurityConfigResponse' {..} =
    Prelude.rnf securityConfigDetail
      `Prelude.seq` Prelude.rnf httpStatus
