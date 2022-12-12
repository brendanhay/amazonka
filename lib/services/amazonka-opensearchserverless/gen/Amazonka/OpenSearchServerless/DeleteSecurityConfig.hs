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
-- Module      : Amazonka.OpenSearchServerless.DeleteSecurityConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a security configuration for OpenSearch Serverless. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-saml.html SAML authentication for Amazon OpenSearch Serverless>.
module Amazonka.OpenSearchServerless.DeleteSecurityConfig
  ( -- * Creating a Request
    DeleteSecurityConfig (..),
    newDeleteSecurityConfig,

    -- * Request Lenses
    deleteSecurityConfig_clientToken,
    deleteSecurityConfig_id,

    -- * Destructuring the Response
    DeleteSecurityConfigResponse (..),
    newDeleteSecurityConfigResponse,

    -- * Response Lenses
    deleteSecurityConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSecurityConfig' smart constructor.
data DeleteSecurityConfig = DeleteSecurityConfig'
  { -- | Unique, case-sensitive identifier to ensure idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The security configuration identifier. For SAML the ID will be
    -- @saml\/\<accountId>\/\<idpProviderName>@. For example,
    -- @saml\/123456789123\/OKTADev@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSecurityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteSecurityConfig_clientToken' - Unique, case-sensitive identifier to ensure idempotency of the request.
--
-- 'id', 'deleteSecurityConfig_id' - The security configuration identifier. For SAML the ID will be
-- @saml\/\<accountId>\/\<idpProviderName>@. For example,
-- @saml\/123456789123\/OKTADev@.
newDeleteSecurityConfig ::
  -- | 'id'
  Prelude.Text ->
  DeleteSecurityConfig
newDeleteSecurityConfig pId_ =
  DeleteSecurityConfig'
    { clientToken =
        Prelude.Nothing,
      id = pId_
    }

-- | Unique, case-sensitive identifier to ensure idempotency of the request.
deleteSecurityConfig_clientToken :: Lens.Lens' DeleteSecurityConfig (Prelude.Maybe Prelude.Text)
deleteSecurityConfig_clientToken = Lens.lens (\DeleteSecurityConfig' {clientToken} -> clientToken) (\s@DeleteSecurityConfig' {} a -> s {clientToken = a} :: DeleteSecurityConfig)

-- | The security configuration identifier. For SAML the ID will be
-- @saml\/\<accountId>\/\<idpProviderName>@. For example,
-- @saml\/123456789123\/OKTADev@.
deleteSecurityConfig_id :: Lens.Lens' DeleteSecurityConfig Prelude.Text
deleteSecurityConfig_id = Lens.lens (\DeleteSecurityConfig' {id} -> id) (\s@DeleteSecurityConfig' {} a -> s {id = a} :: DeleteSecurityConfig)

instance Core.AWSRequest DeleteSecurityConfig where
  type
    AWSResponse DeleteSecurityConfig =
      DeleteSecurityConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSecurityConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSecurityConfig where
  hashWithSalt _salt DeleteSecurityConfig' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteSecurityConfig where
  rnf DeleteSecurityConfig' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeleteSecurityConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.DeleteSecurityConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSecurityConfig where
  toJSON DeleteSecurityConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("id" Data..= id)
          ]
      )

instance Data.ToPath DeleteSecurityConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSecurityConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSecurityConfigResponse' smart constructor.
data DeleteSecurityConfigResponse = DeleteSecurityConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSecurityConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSecurityConfigResponse_httpStatus' - The response's http status code.
newDeleteSecurityConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSecurityConfigResponse
newDeleteSecurityConfigResponse pHttpStatus_ =
  DeleteSecurityConfigResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSecurityConfigResponse_httpStatus :: Lens.Lens' DeleteSecurityConfigResponse Prelude.Int
deleteSecurityConfigResponse_httpStatus = Lens.lens (\DeleteSecurityConfigResponse' {httpStatus} -> httpStatus) (\s@DeleteSecurityConfigResponse' {} a -> s {httpStatus = a} :: DeleteSecurityConfigResponse)

instance Prelude.NFData DeleteSecurityConfigResponse where
  rnf DeleteSecurityConfigResponse' {..} =
    Prelude.rnf httpStatus
