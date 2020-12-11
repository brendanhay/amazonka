{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.CreateResourceServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new OAuth2.0 resource server and defines custom scopes in it.
module Network.AWS.CognitoIdentityProvider.CreateResourceServer
  ( -- * Creating a request
    CreateResourceServer (..),
    mkCreateResourceServer,

    -- ** Request lenses
    crsScopes,
    crsUserPoolId,
    crsIdentifier,
    crsName,

    -- * Destructuring the response
    CreateResourceServerResponse (..),
    mkCreateResourceServerResponse,

    -- ** Response lenses
    crsrsResponseStatus,
    crsrsResourceServer,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateResourceServer' smart constructor.
data CreateResourceServer = CreateResourceServer'
  { scopes ::
      Lude.Maybe [ResourceServerScopeType],
    userPoolId :: Lude.Text,
    identifier :: Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateResourceServer' with the minimum fields required to make a request.
--
-- * 'identifier' - A unique resource server identifier for the resource server. This could be an HTTPS endpoint where the resource server is located. For example, @https://my-weather-api.example.com@ .
-- * 'name' - A friendly name for the resource server.
-- * 'scopes' - A list of scopes. Each scope is map, where the keys are @name@ and @description@ .
-- * 'userPoolId' - The user pool ID for the user pool.
mkCreateResourceServer ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'identifier'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  CreateResourceServer
mkCreateResourceServer pUserPoolId_ pIdentifier_ pName_ =
  CreateResourceServer'
    { scopes = Lude.Nothing,
      userPoolId = pUserPoolId_,
      identifier = pIdentifier_,
      name = pName_
    }

-- | A list of scopes. Each scope is map, where the keys are @name@ and @description@ .
--
-- /Note:/ Consider using 'scopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsScopes :: Lens.Lens' CreateResourceServer (Lude.Maybe [ResourceServerScopeType])
crsScopes = Lens.lens (scopes :: CreateResourceServer -> Lude.Maybe [ResourceServerScopeType]) (\s a -> s {scopes = a} :: CreateResourceServer)
{-# DEPRECATED crsScopes "Use generic-lens or generic-optics with 'scopes' instead." #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsUserPoolId :: Lens.Lens' CreateResourceServer Lude.Text
crsUserPoolId = Lens.lens (userPoolId :: CreateResourceServer -> Lude.Text) (\s a -> s {userPoolId = a} :: CreateResourceServer)
{-# DEPRECATED crsUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | A unique resource server identifier for the resource server. This could be an HTTPS endpoint where the resource server is located. For example, @https://my-weather-api.example.com@ .
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsIdentifier :: Lens.Lens' CreateResourceServer Lude.Text
crsIdentifier = Lens.lens (identifier :: CreateResourceServer -> Lude.Text) (\s a -> s {identifier = a} :: CreateResourceServer)
{-# DEPRECATED crsIdentifier "Use generic-lens or generic-optics with 'identifier' instead." #-}

-- | A friendly name for the resource server.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsName :: Lens.Lens' CreateResourceServer Lude.Text
crsName = Lens.lens (name :: CreateResourceServer -> Lude.Text) (\s a -> s {name = a} :: CreateResourceServer)
{-# DEPRECATED crsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateResourceServer where
  type Rs CreateResourceServer = CreateResourceServerResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateResourceServerResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "ResourceServer")
      )

instance Lude.ToHeaders CreateResourceServer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.CreateResourceServer" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateResourceServer where
  toJSON CreateResourceServer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Scopes" Lude..=) Lude.<$> scopes,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Identifier" Lude..= identifier),
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateResourceServer where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateResourceServer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateResourceServerResponse' smart constructor.
data CreateResourceServerResponse = CreateResourceServerResponse'
  { responseStatus ::
      Lude.Int,
    resourceServer ::
      ResourceServerType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateResourceServerResponse' with the minimum fields required to make a request.
--
-- * 'resourceServer' - The newly created resource server.
-- * 'responseStatus' - The response status code.
mkCreateResourceServerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'resourceServer'
  ResourceServerType ->
  CreateResourceServerResponse
mkCreateResourceServerResponse pResponseStatus_ pResourceServer_ =
  CreateResourceServerResponse'
    { responseStatus = pResponseStatus_,
      resourceServer = pResourceServer_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsrsResponseStatus :: Lens.Lens' CreateResourceServerResponse Lude.Int
crsrsResponseStatus = Lens.lens (responseStatus :: CreateResourceServerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateResourceServerResponse)
{-# DEPRECATED crsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The newly created resource server.
--
-- /Note:/ Consider using 'resourceServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsrsResourceServer :: Lens.Lens' CreateResourceServerResponse ResourceServerType
crsrsResourceServer = Lens.lens (resourceServer :: CreateResourceServerResponse -> ResourceServerType) (\s a -> s {resourceServer = a} :: CreateResourceServerResponse)
{-# DEPRECATED crsrsResourceServer "Use generic-lens or generic-optics with 'resourceServer' instead." #-}
