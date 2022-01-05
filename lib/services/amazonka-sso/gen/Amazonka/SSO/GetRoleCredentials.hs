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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    getRoleCredentialsResponse_roleCredentials,
    getRoleCredentialsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
    -- in the /AWS SSO OIDC API Reference Guide/.
    accessToken :: Core.Sensitive Prelude.Text
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
-- in the /AWS SSO OIDC API Reference Guide/.
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
        accessToken = Core._Sensitive Lens.# pAccessToken_
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
-- in the /AWS SSO OIDC API Reference Guide/.
getRoleCredentials_accessToken :: Lens.Lens' GetRoleCredentials Prelude.Text
getRoleCredentials_accessToken = Lens.lens (\GetRoleCredentials' {accessToken} -> accessToken) (\s@GetRoleCredentials' {} a -> s {accessToken = a} :: GetRoleCredentials) Prelude.. Core._Sensitive

instance Core.AWSRequest GetRoleCredentials where
  type
    AWSResponse GetRoleCredentials =
      GetRoleCredentialsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRoleCredentialsResponse'
            Prelude.<$> (x Core..?> "roleCredentials")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
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

instance Core.ToHeaders GetRoleCredentials where
  toHeaders GetRoleCredentials' {..} =
    Prelude.mconcat
      [ "x-amz-sso_bearer_token" Core.=# accessToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToPath GetRoleCredentials where
  toPath = Prelude.const "/federation/credentials"

instance Core.ToQuery GetRoleCredentials where
  toQuery GetRoleCredentials' {..} =
    Prelude.mconcat
      [ "role_name" Core.=: roleName,
        "account_id" Core.=: accountId
      ]

-- | /See:/ 'newGetRoleCredentialsResponse' smart constructor.
data GetRoleCredentialsResponse = GetRoleCredentialsResponse'
  { -- | The credentials for the role that is assigned to the user.
    roleCredentials :: Prelude.Maybe RoleCredentials,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'roleCredentials', 'getRoleCredentialsResponse_roleCredentials' - The credentials for the role that is assigned to the user.
--
-- 'httpStatus', 'getRoleCredentialsResponse_httpStatus' - The response's http status code.
newGetRoleCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRoleCredentialsResponse
newGetRoleCredentialsResponse pHttpStatus_ =
  GetRoleCredentialsResponse'
    { roleCredentials =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The credentials for the role that is assigned to the user.
getRoleCredentialsResponse_roleCredentials :: Lens.Lens' GetRoleCredentialsResponse (Prelude.Maybe RoleCredentials)
getRoleCredentialsResponse_roleCredentials = Lens.lens (\GetRoleCredentialsResponse' {roleCredentials} -> roleCredentials) (\s@GetRoleCredentialsResponse' {} a -> s {roleCredentials = a} :: GetRoleCredentialsResponse)

-- | The response's http status code.
getRoleCredentialsResponse_httpStatus :: Lens.Lens' GetRoleCredentialsResponse Prelude.Int
getRoleCredentialsResponse_httpStatus = Lens.lens (\GetRoleCredentialsResponse' {httpStatus} -> httpStatus) (\s@GetRoleCredentialsResponse' {} a -> s {httpStatus = a} :: GetRoleCredentialsResponse)

instance Prelude.NFData GetRoleCredentialsResponse where
  rnf GetRoleCredentialsResponse' {..} =
    Prelude.rnf roleCredentials
      `Prelude.seq` Prelude.rnf httpStatus
