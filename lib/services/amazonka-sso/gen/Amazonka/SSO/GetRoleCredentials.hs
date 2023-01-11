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
-- Module      : Amazonka.SSO.GetRoleCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the STS short-term credentials for a given role name that is
-- assigned to the user.
module Amazonka.SSO.GetRoleCredentials
  ( -- * Creating a Request
    GetRoleCredentials (..),
    newGetRoleCredentials,

    -- * Request Lenses
    getRoleCredentials_roleName,
    getRoleCredentials_accountId,
    getRoleCredentials_accessToken,

    -- * Destructuring the Response
    GetRoleCredentialsResponse (..),
    newGetRoleCredentialsResponse,

    -- * Response Lenses
    getRoleCredentialsResponse_httpStatus,
    getRoleCredentialsResponse_roleCredentials,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSO.Types

-- | /See:/ 'newGetRoleCredentials' smart constructor.
data GetRoleCredentials = GetRoleCredentials'
  { -- | The friendly name of the role that is assigned to the user.
    roleName :: Prelude.Text,
    -- | The identifier for the AWS account that is assigned to the user.
    accountId :: Prelude.Text,
    -- | The token issued by the @CreateToken@ API call. For more information,
    -- see
    -- <https://docs.aws.amazon.com/singlesignon/latest/OIDCAPIReference/API_CreateToken.html CreateToken>
    -- in the /IAM Identity Center OIDC API Reference Guide/.
    accessToken :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRoleCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'getRoleCredentials_roleName' - The friendly name of the role that is assigned to the user.
--
-- 'accountId', 'getRoleCredentials_accountId' - The identifier for the AWS account that is assigned to the user.
--
-- 'accessToken', 'getRoleCredentials_accessToken' - The token issued by the @CreateToken@ API call. For more information,
-- see
-- <https://docs.aws.amazon.com/singlesignon/latest/OIDCAPIReference/API_CreateToken.html CreateToken>
-- in the /IAM Identity Center OIDC API Reference Guide/.
newGetRoleCredentials ::
  -- | 'roleName'
  Prelude.Text ->
  -- | 'accountId'
  Prelude.Text ->
  -- | 'accessToken'
  Prelude.Text ->
  GetRoleCredentials
newGetRoleCredentials
  pRoleName_
  pAccountId_
  pAccessToken_ =
    GetRoleCredentials'
      { roleName = pRoleName_,
        accountId = pAccountId_,
        accessToken = Data._Sensitive Lens.# pAccessToken_
      }

-- | The friendly name of the role that is assigned to the user.
getRoleCredentials_roleName :: Lens.Lens' GetRoleCredentials Prelude.Text
getRoleCredentials_roleName = Lens.lens (\GetRoleCredentials' {roleName} -> roleName) (\s@GetRoleCredentials' {} a -> s {roleName = a} :: GetRoleCredentials)

-- | The identifier for the AWS account that is assigned to the user.
getRoleCredentials_accountId :: Lens.Lens' GetRoleCredentials Prelude.Text
getRoleCredentials_accountId = Lens.lens (\GetRoleCredentials' {accountId} -> accountId) (\s@GetRoleCredentials' {} a -> s {accountId = a} :: GetRoleCredentials)

-- | The token issued by the @CreateToken@ API call. For more information,
-- see
-- <https://docs.aws.amazon.com/singlesignon/latest/OIDCAPIReference/API_CreateToken.html CreateToken>
-- in the /IAM Identity Center OIDC API Reference Guide/.
getRoleCredentials_accessToken :: Lens.Lens' GetRoleCredentials Prelude.Text
getRoleCredentials_accessToken = Lens.lens (\GetRoleCredentials' {accessToken} -> accessToken) (\s@GetRoleCredentials' {} a -> s {accessToken = a} :: GetRoleCredentials) Prelude.. Data._Sensitive

instance Core.AWSRequest GetRoleCredentials where
  type
    AWSResponse GetRoleCredentials =
      GetRoleCredentialsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRoleCredentialsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "roleCredentials")
      )

instance Prelude.Hashable GetRoleCredentials where
  hashWithSalt _salt GetRoleCredentials' {..} =
    _salt `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` accessToken

instance Prelude.NFData GetRoleCredentials where
  rnf GetRoleCredentials' {..} =
    Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf accessToken

instance Data.ToHeaders GetRoleCredentials where
  toHeaders GetRoleCredentials' {..} =
    Prelude.mconcat
      [ "x-amz-sso_bearer_token" Data.=# accessToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath GetRoleCredentials where
  toPath = Prelude.const "/federation/credentials"

instance Data.ToQuery GetRoleCredentials where
  toQuery GetRoleCredentials' {..} =
    Prelude.mconcat
      [ "role_name" Data.=: roleName,
        "account_id" Data.=: accountId
      ]

-- | /See:/ 'newGetRoleCredentialsResponse' smart constructor.
data GetRoleCredentialsResponse = GetRoleCredentialsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The credentials for the role that is assigned to the user.
    roleCredentials :: RoleCredentials
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRoleCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getRoleCredentialsResponse_httpStatus' - The response's http status code.
--
-- 'roleCredentials', 'getRoleCredentialsResponse_roleCredentials' - The credentials for the role that is assigned to the user.
newGetRoleCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'roleCredentials'
  RoleCredentials ->
  GetRoleCredentialsResponse
newGetRoleCredentialsResponse
  pHttpStatus_
  pRoleCredentials_ =
    GetRoleCredentialsResponse'
      { httpStatus =
          pHttpStatus_,
        roleCredentials = pRoleCredentials_
      }

-- | The response's http status code.
getRoleCredentialsResponse_httpStatus :: Lens.Lens' GetRoleCredentialsResponse Prelude.Int
getRoleCredentialsResponse_httpStatus = Lens.lens (\GetRoleCredentialsResponse' {httpStatus} -> httpStatus) (\s@GetRoleCredentialsResponse' {} a -> s {httpStatus = a} :: GetRoleCredentialsResponse)

-- | The credentials for the role that is assigned to the user.
getRoleCredentialsResponse_roleCredentials :: Lens.Lens' GetRoleCredentialsResponse RoleCredentials
getRoleCredentialsResponse_roleCredentials = Lens.lens (\GetRoleCredentialsResponse' {roleCredentials} -> roleCredentials) (\s@GetRoleCredentialsResponse' {} a -> s {roleCredentials = a} :: GetRoleCredentialsResponse)

instance Prelude.NFData GetRoleCredentialsResponse where
  rnf GetRoleCredentialsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf roleCredentials
