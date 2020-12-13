{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateResourceServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and scopes of resource server. All other fields are read-only.
--
-- /Important:/ If you don't provide a value for an attribute, it will be set to the default value.
module Network.AWS.CognitoIdentityProvider.UpdateResourceServer
  ( -- * Creating a request
    UpdateResourceServer (..),
    mkUpdateResourceServer,

    -- ** Request lenses
    ursUserPoolId,
    ursIdentifier,
    ursScopes,
    ursName,

    -- * Destructuring the response
    UpdateResourceServerResponse (..),
    mkUpdateResourceServerResponse,

    -- ** Response lenses
    ursrsResourceServer,
    ursrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateResourceServer' smart constructor.
data UpdateResourceServer = UpdateResourceServer'
  { -- | The user pool ID for the user pool.
    userPoolId :: Lude.Text,
    -- | The identifier for the resource server.
    identifier :: Lude.Text,
    -- | The scope values to be set for the resource server.
    scopes :: Lude.Maybe [ResourceServerScopeType],
    -- | The name of the resource server.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateResourceServer' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID for the user pool.
-- * 'identifier' - The identifier for the resource server.
-- * 'scopes' - The scope values to be set for the resource server.
-- * 'name' - The name of the resource server.
mkUpdateResourceServer ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'identifier'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  UpdateResourceServer
mkUpdateResourceServer pUserPoolId_ pIdentifier_ pName_ =
  UpdateResourceServer'
    { userPoolId = pUserPoolId_,
      identifier = pIdentifier_,
      scopes = Lude.Nothing,
      name = pName_
    }

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursUserPoolId :: Lens.Lens' UpdateResourceServer Lude.Text
ursUserPoolId = Lens.lens (userPoolId :: UpdateResourceServer -> Lude.Text) (\s a -> s {userPoolId = a} :: UpdateResourceServer)
{-# DEPRECATED ursUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identifier for the resource server.
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursIdentifier :: Lens.Lens' UpdateResourceServer Lude.Text
ursIdentifier = Lens.lens (identifier :: UpdateResourceServer -> Lude.Text) (\s a -> s {identifier = a} :: UpdateResourceServer)
{-# DEPRECATED ursIdentifier "Use generic-lens or generic-optics with 'identifier' instead." #-}

-- | The scope values to be set for the resource server.
--
-- /Note:/ Consider using 'scopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursScopes :: Lens.Lens' UpdateResourceServer (Lude.Maybe [ResourceServerScopeType])
ursScopes = Lens.lens (scopes :: UpdateResourceServer -> Lude.Maybe [ResourceServerScopeType]) (\s a -> s {scopes = a} :: UpdateResourceServer)
{-# DEPRECATED ursScopes "Use generic-lens or generic-optics with 'scopes' instead." #-}

-- | The name of the resource server.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursName :: Lens.Lens' UpdateResourceServer Lude.Text
ursName = Lens.lens (name :: UpdateResourceServer -> Lude.Text) (\s a -> s {name = a} :: UpdateResourceServer)
{-# DEPRECATED ursName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest UpdateResourceServer where
  type Rs UpdateResourceServer = UpdateResourceServerResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateResourceServerResponse'
            Lude.<$> (x Lude..:> "ResourceServer")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateResourceServer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.UpdateResourceServer" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateResourceServer where
  toJSON UpdateResourceServer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Identifier" Lude..= identifier),
            ("Scopes" Lude..=) Lude.<$> scopes,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath UpdateResourceServer where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateResourceServer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateResourceServerResponse' smart constructor.
data UpdateResourceServerResponse = UpdateResourceServerResponse'
  { -- | The resource server.
    resourceServer :: ResourceServerType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateResourceServerResponse' with the minimum fields required to make a request.
--
-- * 'resourceServer' - The resource server.
-- * 'responseStatus' - The response status code.
mkUpdateResourceServerResponse ::
  -- | 'resourceServer'
  ResourceServerType ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateResourceServerResponse
mkUpdateResourceServerResponse pResourceServer_ pResponseStatus_ =
  UpdateResourceServerResponse'
    { resourceServer = pResourceServer_,
      responseStatus = pResponseStatus_
    }

-- | The resource server.
--
-- /Note:/ Consider using 'resourceServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursrsResourceServer :: Lens.Lens' UpdateResourceServerResponse ResourceServerType
ursrsResourceServer = Lens.lens (resourceServer :: UpdateResourceServerResponse -> ResourceServerType) (\s a -> s {resourceServer = a} :: UpdateResourceServerResponse)
{-# DEPRECATED ursrsResourceServer "Use generic-lens or generic-optics with 'resourceServer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursrsResponseStatus :: Lens.Lens' UpdateResourceServerResponse Lude.Int
ursrsResponseStatus = Lens.lens (responseStatus :: UpdateResourceServerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateResourceServerResponse)
{-# DEPRECATED ursrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
