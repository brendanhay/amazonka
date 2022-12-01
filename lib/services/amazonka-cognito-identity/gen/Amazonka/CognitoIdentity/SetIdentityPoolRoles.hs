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
-- Module      : Amazonka.CognitoIdentity.SetIdentityPoolRoles
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the roles for an identity pool. These roles are used when making
-- calls to GetCredentialsForIdentity action.
--
-- You must use AWS Developer credentials to call this API.
module Amazonka.CognitoIdentity.SetIdentityPoolRoles
  ( -- * Creating a Request
    SetIdentityPoolRoles (..),
    newSetIdentityPoolRoles,

    -- * Request Lenses
    setIdentityPoolRoles_roleMappings,
    setIdentityPoolRoles_identityPoolId,
    setIdentityPoolRoles_roles,

    -- * Destructuring the Response
    SetIdentityPoolRolesResponse (..),
    newSetIdentityPoolRolesResponse,
  )
where

import Amazonka.CognitoIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Input to the @SetIdentityPoolRoles@ action.
--
-- /See:/ 'newSetIdentityPoolRoles' smart constructor.
data SetIdentityPoolRoles = SetIdentityPoolRoles'
  { -- | How users for a specific identity provider are to mapped to roles. This
    -- is a string to RoleMapping object map. The string identifies the
    -- identity provider, for example, \"graph.facebook.com\" or
    -- \"cognito-idp.us-east-1.amazonaws.com\/us-east-1_abcdefghi:app_client_id\".
    --
    -- Up to 25 rules can be specified per identity provider.
    roleMappings :: Prelude.Maybe (Prelude.HashMap Prelude.Text RoleMapping),
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Prelude.Text,
    -- | The map of roles associated with this pool. For a given role, the key
    -- will be either \"authenticated\" or \"unauthenticated\" and the value
    -- will be the Role ARN.
    roles :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetIdentityPoolRoles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleMappings', 'setIdentityPoolRoles_roleMappings' - How users for a specific identity provider are to mapped to roles. This
-- is a string to RoleMapping object map. The string identifies the
-- identity provider, for example, \"graph.facebook.com\" or
-- \"cognito-idp.us-east-1.amazonaws.com\/us-east-1_abcdefghi:app_client_id\".
--
-- Up to 25 rules can be specified per identity provider.
--
-- 'identityPoolId', 'setIdentityPoolRoles_identityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- 'roles', 'setIdentityPoolRoles_roles' - The map of roles associated with this pool. For a given role, the key
-- will be either \"authenticated\" or \"unauthenticated\" and the value
-- will be the Role ARN.
newSetIdentityPoolRoles ::
  -- | 'identityPoolId'
  Prelude.Text ->
  SetIdentityPoolRoles
newSetIdentityPoolRoles pIdentityPoolId_ =
  SetIdentityPoolRoles'
    { roleMappings =
        Prelude.Nothing,
      identityPoolId = pIdentityPoolId_,
      roles = Prelude.mempty
    }

-- | How users for a specific identity provider are to mapped to roles. This
-- is a string to RoleMapping object map. The string identifies the
-- identity provider, for example, \"graph.facebook.com\" or
-- \"cognito-idp.us-east-1.amazonaws.com\/us-east-1_abcdefghi:app_client_id\".
--
-- Up to 25 rules can be specified per identity provider.
setIdentityPoolRoles_roleMappings :: Lens.Lens' SetIdentityPoolRoles (Prelude.Maybe (Prelude.HashMap Prelude.Text RoleMapping))
setIdentityPoolRoles_roleMappings = Lens.lens (\SetIdentityPoolRoles' {roleMappings} -> roleMappings) (\s@SetIdentityPoolRoles' {} a -> s {roleMappings = a} :: SetIdentityPoolRoles) Prelude.. Lens.mapping Lens.coerced

-- | An identity pool ID in the format REGION:GUID.
setIdentityPoolRoles_identityPoolId :: Lens.Lens' SetIdentityPoolRoles Prelude.Text
setIdentityPoolRoles_identityPoolId = Lens.lens (\SetIdentityPoolRoles' {identityPoolId} -> identityPoolId) (\s@SetIdentityPoolRoles' {} a -> s {identityPoolId = a} :: SetIdentityPoolRoles)

-- | The map of roles associated with this pool. For a given role, the key
-- will be either \"authenticated\" or \"unauthenticated\" and the value
-- will be the Role ARN.
setIdentityPoolRoles_roles :: Lens.Lens' SetIdentityPoolRoles (Prelude.HashMap Prelude.Text Prelude.Text)
setIdentityPoolRoles_roles = Lens.lens (\SetIdentityPoolRoles' {roles} -> roles) (\s@SetIdentityPoolRoles' {} a -> s {roles = a} :: SetIdentityPoolRoles) Prelude.. Lens.coerced

instance Core.AWSRequest SetIdentityPoolRoles where
  type
    AWSResponse SetIdentityPoolRoles =
      SetIdentityPoolRolesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull SetIdentityPoolRolesResponse'

instance Prelude.Hashable SetIdentityPoolRoles where
  hashWithSalt _salt SetIdentityPoolRoles' {..} =
    _salt `Prelude.hashWithSalt` roleMappings
      `Prelude.hashWithSalt` identityPoolId
      `Prelude.hashWithSalt` roles

instance Prelude.NFData SetIdentityPoolRoles where
  rnf SetIdentityPoolRoles' {..} =
    Prelude.rnf roleMappings
      `Prelude.seq` Prelude.rnf identityPoolId
      `Prelude.seq` Prelude.rnf roles

instance Core.ToHeaders SetIdentityPoolRoles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.SetIdentityPoolRoles" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SetIdentityPoolRoles where
  toJSON SetIdentityPoolRoles' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RoleMappings" Core..=) Prelude.<$> roleMappings,
            Prelude.Just
              ("IdentityPoolId" Core..= identityPoolId),
            Prelude.Just ("Roles" Core..= roles)
          ]
      )

instance Core.ToPath SetIdentityPoolRoles where
  toPath = Prelude.const "/"

instance Core.ToQuery SetIdentityPoolRoles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetIdentityPoolRolesResponse' smart constructor.
data SetIdentityPoolRolesResponse = SetIdentityPoolRolesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetIdentityPoolRolesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetIdentityPoolRolesResponse ::
  SetIdentityPoolRolesResponse
newSetIdentityPoolRolesResponse =
  SetIdentityPoolRolesResponse'

instance Prelude.NFData SetIdentityPoolRolesResponse where
  rnf _ = ()
