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
-- Module      : Network.AWS.CognitoIdentity.GetIdentityPoolRoles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the roles for an identity pool.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.GetIdentityPoolRoles
  ( -- * Creating a Request
    GetIdentityPoolRoles (..),
    newGetIdentityPoolRoles,

    -- * Request Lenses
    getIdentityPoolRoles_identityPoolId,

    -- * Destructuring the Response
    GetIdentityPoolRolesResponse (..),
    newGetIdentityPoolRolesResponse,

    -- * Response Lenses
    getIdentityPoolRolesResponse_identityPoolId,
    getIdentityPoolRolesResponse_roles,
    getIdentityPoolRolesResponse_roleMappings,
    getIdentityPoolRolesResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @GetIdentityPoolRoles@ action.
--
-- /See:/ 'newGetIdentityPoolRoles' smart constructor.
data GetIdentityPoolRoles = GetIdentityPoolRoles'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIdentityPoolRoles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'getIdentityPoolRoles_identityPoolId' - An identity pool ID in the format REGION:GUID.
newGetIdentityPoolRoles ::
  -- | 'identityPoolId'
  Prelude.Text ->
  GetIdentityPoolRoles
newGetIdentityPoolRoles pIdentityPoolId_ =
  GetIdentityPoolRoles'
    { identityPoolId =
        pIdentityPoolId_
    }

-- | An identity pool ID in the format REGION:GUID.
getIdentityPoolRoles_identityPoolId :: Lens.Lens' GetIdentityPoolRoles Prelude.Text
getIdentityPoolRoles_identityPoolId = Lens.lens (\GetIdentityPoolRoles' {identityPoolId} -> identityPoolId) (\s@GetIdentityPoolRoles' {} a -> s {identityPoolId = a} :: GetIdentityPoolRoles)

instance Core.AWSRequest GetIdentityPoolRoles where
  type
    AWSResponse GetIdentityPoolRoles =
      GetIdentityPoolRolesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIdentityPoolRolesResponse'
            Prelude.<$> (x Core..?> "IdentityPoolId")
            Prelude.<*> (x Core..?> "Roles" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "RoleMappings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIdentityPoolRoles

instance Prelude.NFData GetIdentityPoolRoles

instance Core.ToHeaders GetIdentityPoolRoles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.GetIdentityPoolRoles" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetIdentityPoolRoles where
  toJSON GetIdentityPoolRoles' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityPoolId" Core..= identityPoolId)
          ]
      )

instance Core.ToPath GetIdentityPoolRoles where
  toPath = Prelude.const "/"

instance Core.ToQuery GetIdentityPoolRoles where
  toQuery = Prelude.const Prelude.mempty

-- | Returned in response to a successful @GetIdentityPoolRoles@ operation.
--
-- /See:/ 'newGetIdentityPoolRolesResponse' smart constructor.
data GetIdentityPoolRolesResponse = GetIdentityPoolRolesResponse'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Prelude.Maybe Prelude.Text,
    -- | The map of roles associated with this pool. Currently only authenticated
    -- and unauthenticated roles are supported.
    roles :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | How users for a specific identity provider are to mapped to roles. This
    -- is a String-to-RoleMapping object map. The string identifies the
    -- identity provider, for example, \"graph.facebook.com\" or
    -- \"cognito-idp.us-east-1.amazonaws.com\/us-east-1_abcdefghi:app_client_id\".
    roleMappings :: Prelude.Maybe (Prelude.HashMap Prelude.Text RoleMapping),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIdentityPoolRolesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'getIdentityPoolRolesResponse_identityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- 'roles', 'getIdentityPoolRolesResponse_roles' - The map of roles associated with this pool. Currently only authenticated
-- and unauthenticated roles are supported.
--
-- 'roleMappings', 'getIdentityPoolRolesResponse_roleMappings' - How users for a specific identity provider are to mapped to roles. This
-- is a String-to-RoleMapping object map. The string identifies the
-- identity provider, for example, \"graph.facebook.com\" or
-- \"cognito-idp.us-east-1.amazonaws.com\/us-east-1_abcdefghi:app_client_id\".
--
-- 'httpStatus', 'getIdentityPoolRolesResponse_httpStatus' - The response's http status code.
newGetIdentityPoolRolesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIdentityPoolRolesResponse
newGetIdentityPoolRolesResponse pHttpStatus_ =
  GetIdentityPoolRolesResponse'
    { identityPoolId =
        Prelude.Nothing,
      roles = Prelude.Nothing,
      roleMappings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identity pool ID in the format REGION:GUID.
getIdentityPoolRolesResponse_identityPoolId :: Lens.Lens' GetIdentityPoolRolesResponse (Prelude.Maybe Prelude.Text)
getIdentityPoolRolesResponse_identityPoolId = Lens.lens (\GetIdentityPoolRolesResponse' {identityPoolId} -> identityPoolId) (\s@GetIdentityPoolRolesResponse' {} a -> s {identityPoolId = a} :: GetIdentityPoolRolesResponse)

-- | The map of roles associated with this pool. Currently only authenticated
-- and unauthenticated roles are supported.
getIdentityPoolRolesResponse_roles :: Lens.Lens' GetIdentityPoolRolesResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getIdentityPoolRolesResponse_roles = Lens.lens (\GetIdentityPoolRolesResponse' {roles} -> roles) (\s@GetIdentityPoolRolesResponse' {} a -> s {roles = a} :: GetIdentityPoolRolesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | How users for a specific identity provider are to mapped to roles. This
-- is a String-to-RoleMapping object map. The string identifies the
-- identity provider, for example, \"graph.facebook.com\" or
-- \"cognito-idp.us-east-1.amazonaws.com\/us-east-1_abcdefghi:app_client_id\".
getIdentityPoolRolesResponse_roleMappings :: Lens.Lens' GetIdentityPoolRolesResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text RoleMapping))
getIdentityPoolRolesResponse_roleMappings = Lens.lens (\GetIdentityPoolRolesResponse' {roleMappings} -> roleMappings) (\s@GetIdentityPoolRolesResponse' {} a -> s {roleMappings = a} :: GetIdentityPoolRolesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getIdentityPoolRolesResponse_httpStatus :: Lens.Lens' GetIdentityPoolRolesResponse Prelude.Int
getIdentityPoolRolesResponse_httpStatus = Lens.lens (\GetIdentityPoolRolesResponse' {httpStatus} -> httpStatus) (\s@GetIdentityPoolRolesResponse' {} a -> s {httpStatus = a} :: GetIdentityPoolRolesResponse)

instance Prelude.NFData GetIdentityPoolRolesResponse
