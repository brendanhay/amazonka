{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.SetIdentityPoolRoles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the roles for an identity pool. These roles are used when making calls to 'GetCredentialsForIdentity' action.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.SetIdentityPoolRoles
  ( -- * Creating a request
    SetIdentityPoolRoles (..),
    mkSetIdentityPoolRoles,

    -- ** Request lenses
    siprRoleMappings,
    siprIdentityPoolId,
    siprRoles,

    -- * Destructuring the response
    SetIdentityPoolRolesResponse (..),
    mkSetIdentityPoolRolesResponse,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the @SetIdentityPoolRoles@ action.
--
-- /See:/ 'mkSetIdentityPoolRoles' smart constructor.
data SetIdentityPoolRoles = SetIdentityPoolRoles'
  { roleMappings ::
      Lude.Maybe (Lude.HashMap Lude.Text (RoleMapping)),
    identityPoolId :: Lude.Text,
    roles :: Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetIdentityPoolRoles' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - An identity pool ID in the format REGION:GUID.
-- * 'roleMappings' - How users for a specific identity provider are to mapped to roles. This is a string to 'RoleMapping' object map. The string identifies the identity provider, for example, "graph.facebook.com" or "cognito-idp-east-1.amazonaws.com/us-east-1_abcdefghi:app_client_id".
--
-- Up to 25 rules can be specified per identity provider.
-- * 'roles' - The map of roles associated with this pool. For a given role, the key will be either "authenticated" or "unauthenticated" and the value will be the Role ARN.
mkSetIdentityPoolRoles ::
  -- | 'identityPoolId'
  Lude.Text ->
  SetIdentityPoolRoles
mkSetIdentityPoolRoles pIdentityPoolId_ =
  SetIdentityPoolRoles'
    { roleMappings = Lude.Nothing,
      identityPoolId = pIdentityPoolId_,
      roles = Lude.mempty
    }

-- | How users for a specific identity provider are to mapped to roles. This is a string to 'RoleMapping' object map. The string identifies the identity provider, for example, "graph.facebook.com" or "cognito-idp-east-1.amazonaws.com/us-east-1_abcdefghi:app_client_id".
--
-- Up to 25 rules can be specified per identity provider.
--
-- /Note:/ Consider using 'roleMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siprRoleMappings :: Lens.Lens' SetIdentityPoolRoles (Lude.Maybe (Lude.HashMap Lude.Text (RoleMapping)))
siprRoleMappings = Lens.lens (roleMappings :: SetIdentityPoolRoles -> Lude.Maybe (Lude.HashMap Lude.Text (RoleMapping))) (\s a -> s {roleMappings = a} :: SetIdentityPoolRoles)
{-# DEPRECATED siprRoleMappings "Use generic-lens or generic-optics with 'roleMappings' instead." #-}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siprIdentityPoolId :: Lens.Lens' SetIdentityPoolRoles Lude.Text
siprIdentityPoolId = Lens.lens (identityPoolId :: SetIdentityPoolRoles -> Lude.Text) (\s a -> s {identityPoolId = a} :: SetIdentityPoolRoles)
{-# DEPRECATED siprIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | The map of roles associated with this pool. For a given role, the key will be either "authenticated" or "unauthenticated" and the value will be the Role ARN.
--
-- /Note:/ Consider using 'roles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siprRoles :: Lens.Lens' SetIdentityPoolRoles (Lude.HashMap Lude.Text (Lude.Text))
siprRoles = Lens.lens (roles :: SetIdentityPoolRoles -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {roles = a} :: SetIdentityPoolRoles)
{-# DEPRECATED siprRoles "Use generic-lens or generic-optics with 'roles' instead." #-}

instance Lude.AWSRequest SetIdentityPoolRoles where
  type Rs SetIdentityPoolRoles = SetIdentityPoolRolesResponse
  request = Req.postJSON cognitoIdentityService
  response = Res.receiveNull SetIdentityPoolRolesResponse'

instance Lude.ToHeaders SetIdentityPoolRoles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityService.SetIdentityPoolRoles" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetIdentityPoolRoles where
  toJSON SetIdentityPoolRoles' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RoleMappings" Lude..=) Lude.<$> roleMappings,
            Lude.Just ("IdentityPoolId" Lude..= identityPoolId),
            Lude.Just ("Roles" Lude..= roles)
          ]
      )

instance Lude.ToPath SetIdentityPoolRoles where
  toPath = Lude.const "/"

instance Lude.ToQuery SetIdentityPoolRoles where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetIdentityPoolRolesResponse' smart constructor.
data SetIdentityPoolRolesResponse = SetIdentityPoolRolesResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetIdentityPoolRolesResponse' with the minimum fields required to make a request.
mkSetIdentityPoolRolesResponse ::
  SetIdentityPoolRolesResponse
mkSetIdentityPoolRolesResponse = SetIdentityPoolRolesResponse'
