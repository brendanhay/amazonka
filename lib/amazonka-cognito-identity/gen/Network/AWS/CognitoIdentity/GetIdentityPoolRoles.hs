{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetIdentityPoolRoles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the roles for an identity pool.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.GetIdentityPoolRoles
  ( -- * Creating a request
    GetIdentityPoolRoles (..),
    mkGetIdentityPoolRoles,

    -- ** Request lenses
    giprIdentityPoolId,

    -- * Destructuring the response
    GetIdentityPoolRolesResponse (..),
    mkGetIdentityPoolRolesResponse,

    -- ** Response lenses
    giprrsRoles,
    giprrsIdentityPoolId,
    giprrsRoleMappings,
    giprrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the @GetIdentityPoolRoles@ action.
--
-- /See:/ 'mkGetIdentityPoolRoles' smart constructor.
newtype GetIdentityPoolRoles = GetIdentityPoolRoles'
  { identityPoolId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdentityPoolRoles' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - An identity pool ID in the format REGION:GUID.
mkGetIdentityPoolRoles ::
  -- | 'identityPoolId'
  Lude.Text ->
  GetIdentityPoolRoles
mkGetIdentityPoolRoles pIdentityPoolId_ =
  GetIdentityPoolRoles' {identityPoolId = pIdentityPoolId_}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprIdentityPoolId :: Lens.Lens' GetIdentityPoolRoles Lude.Text
giprIdentityPoolId = Lens.lens (identityPoolId :: GetIdentityPoolRoles -> Lude.Text) (\s a -> s {identityPoolId = a} :: GetIdentityPoolRoles)
{-# DEPRECATED giprIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

instance Lude.AWSRequest GetIdentityPoolRoles where
  type Rs GetIdentityPoolRoles = GetIdentityPoolRolesResponse
  request = Req.postJSON cognitoIdentityService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetIdentityPoolRolesResponse'
            Lude.<$> (x Lude..?> "Roles" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "IdentityPoolId")
            Lude.<*> (x Lude..?> "RoleMappings" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetIdentityPoolRoles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityService.GetIdentityPoolRoles" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetIdentityPoolRoles where
  toJSON GetIdentityPoolRoles' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("IdentityPoolId" Lude..= identityPoolId)]
      )

instance Lude.ToPath GetIdentityPoolRoles where
  toPath = Lude.const "/"

instance Lude.ToQuery GetIdentityPoolRoles where
  toQuery = Lude.const Lude.mempty

-- | Returned in response to a successful @GetIdentityPoolRoles@ operation.
--
-- /See:/ 'mkGetIdentityPoolRolesResponse' smart constructor.
data GetIdentityPoolRolesResponse = GetIdentityPoolRolesResponse'
  { roles ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    identityPoolId ::
      Lude.Maybe Lude.Text,
    roleMappings ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (RoleMapping)
        ),
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdentityPoolRolesResponse' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - An identity pool ID in the format REGION:GUID.
-- * 'responseStatus' - The response status code.
-- * 'roleMappings' - How users for a specific identity provider are to mapped to roles. This is a String-to-'RoleMapping' object map. The string identifies the identity provider, for example, "graph.facebook.com" or "cognito-idp.us-east-1.amazonaws.com/us-east-1_abcdefghi:app_client_id".
-- * 'roles' - The map of roles associated with this pool. Currently only authenticated and unauthenticated roles are supported.
mkGetIdentityPoolRolesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetIdentityPoolRolesResponse
mkGetIdentityPoolRolesResponse pResponseStatus_ =
  GetIdentityPoolRolesResponse'
    { roles = Lude.Nothing,
      identityPoolId = Lude.Nothing,
      roleMappings = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The map of roles associated with this pool. Currently only authenticated and unauthenticated roles are supported.
--
-- /Note:/ Consider using 'roles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrsRoles :: Lens.Lens' GetIdentityPoolRolesResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
giprrsRoles = Lens.lens (roles :: GetIdentityPoolRolesResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {roles = a} :: GetIdentityPoolRolesResponse)
{-# DEPRECATED giprrsRoles "Use generic-lens or generic-optics with 'roles' instead." #-}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrsIdentityPoolId :: Lens.Lens' GetIdentityPoolRolesResponse (Lude.Maybe Lude.Text)
giprrsIdentityPoolId = Lens.lens (identityPoolId :: GetIdentityPoolRolesResponse -> Lude.Maybe Lude.Text) (\s a -> s {identityPoolId = a} :: GetIdentityPoolRolesResponse)
{-# DEPRECATED giprrsIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | How users for a specific identity provider are to mapped to roles. This is a String-to-'RoleMapping' object map. The string identifies the identity provider, for example, "graph.facebook.com" or "cognito-idp.us-east-1.amazonaws.com/us-east-1_abcdefghi:app_client_id".
--
-- /Note:/ Consider using 'roleMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrsRoleMappings :: Lens.Lens' GetIdentityPoolRolesResponse (Lude.Maybe (Lude.HashMap Lude.Text (RoleMapping)))
giprrsRoleMappings = Lens.lens (roleMappings :: GetIdentityPoolRolesResponse -> Lude.Maybe (Lude.HashMap Lude.Text (RoleMapping))) (\s a -> s {roleMappings = a} :: GetIdentityPoolRolesResponse)
{-# DEPRECATED giprrsRoleMappings "Use generic-lens or generic-optics with 'roleMappings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrsResponseStatus :: Lens.Lens' GetIdentityPoolRolesResponse Lude.Int
giprrsResponseStatus = Lens.lens (responseStatus :: GetIdentityPoolRolesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetIdentityPoolRolesResponse)
{-# DEPRECATED giprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
