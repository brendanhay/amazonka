{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.CreateGraphqlAPI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @GraphqlApi@ object.
module Network.AWS.AppSync.CreateGraphqlAPI
  ( -- * Creating a request
    CreateGraphqlAPI (..),
    mkCreateGraphqlAPI,

    -- ** Request lenses
    cgaXrayEnabled,
    cgaOpenIdConnectConfig,
    cgaAdditionalAuthenticationProviders,
    cgaName,
    cgaUserPoolConfig,
    cgaAuthenticationType,
    cgaLogConfig,
    cgaTags,

    -- * Destructuring the response
    CreateGraphqlAPIResponse (..),
    mkCreateGraphqlAPIResponse,

    -- ** Response lenses
    cgarsGraphqlAPI,
    cgarsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateGraphqlAPI' smart constructor.
data CreateGraphqlAPI = CreateGraphqlAPI'
  { -- | A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@ .
    xrayEnabled :: Lude.Maybe Lude.Bool,
    -- | The OpenID Connect configuration.
    openIdConnectConfig :: Lude.Maybe OpenIdConnectConfig,
    -- | A list of additional authentication providers for the @GraphqlApi@ API.
    additionalAuthenticationProviders :: Lude.Maybe [AdditionalAuthenticationProvider],
    -- | A user-supplied name for the @GraphqlApi@ .
    name :: Lude.Text,
    -- | The Amazon Cognito user pool configuration.
    userPoolConfig :: Lude.Maybe UserPoolConfig,
    -- | The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user pools.
    authenticationType :: AuthenticationType,
    -- | The Amazon CloudWatch Logs configuration.
    logConfig :: Lude.Maybe LogConfig,
    -- | A @TagMap@ object.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGraphqlAPI' with the minimum fields required to make a request.
--
-- * 'xrayEnabled' - A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@ .
-- * 'openIdConnectConfig' - The OpenID Connect configuration.
-- * 'additionalAuthenticationProviders' - A list of additional authentication providers for the @GraphqlApi@ API.
-- * 'name' - A user-supplied name for the @GraphqlApi@ .
-- * 'userPoolConfig' - The Amazon Cognito user pool configuration.
-- * 'authenticationType' - The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user pools.
-- * 'logConfig' - The Amazon CloudWatch Logs configuration.
-- * 'tags' - A @TagMap@ object.
mkCreateGraphqlAPI ::
  -- | 'name'
  Lude.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  CreateGraphqlAPI
mkCreateGraphqlAPI pName_ pAuthenticationType_ =
  CreateGraphqlAPI'
    { xrayEnabled = Lude.Nothing,
      openIdConnectConfig = Lude.Nothing,
      additionalAuthenticationProviders = Lude.Nothing,
      name = pName_,
      userPoolConfig = Lude.Nothing,
      authenticationType = pAuthenticationType_,
      logConfig = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@ .
--
-- /Note:/ Consider using 'xrayEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgaXrayEnabled :: Lens.Lens' CreateGraphqlAPI (Lude.Maybe Lude.Bool)
cgaXrayEnabled = Lens.lens (xrayEnabled :: CreateGraphqlAPI -> Lude.Maybe Lude.Bool) (\s a -> s {xrayEnabled = a} :: CreateGraphqlAPI)
{-# DEPRECATED cgaXrayEnabled "Use generic-lens or generic-optics with 'xrayEnabled' instead." #-}

-- | The OpenID Connect configuration.
--
-- /Note:/ Consider using 'openIdConnectConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgaOpenIdConnectConfig :: Lens.Lens' CreateGraphqlAPI (Lude.Maybe OpenIdConnectConfig)
cgaOpenIdConnectConfig = Lens.lens (openIdConnectConfig :: CreateGraphqlAPI -> Lude.Maybe OpenIdConnectConfig) (\s a -> s {openIdConnectConfig = a} :: CreateGraphqlAPI)
{-# DEPRECATED cgaOpenIdConnectConfig "Use generic-lens or generic-optics with 'openIdConnectConfig' instead." #-}

-- | A list of additional authentication providers for the @GraphqlApi@ API.
--
-- /Note:/ Consider using 'additionalAuthenticationProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgaAdditionalAuthenticationProviders :: Lens.Lens' CreateGraphqlAPI (Lude.Maybe [AdditionalAuthenticationProvider])
cgaAdditionalAuthenticationProviders = Lens.lens (additionalAuthenticationProviders :: CreateGraphqlAPI -> Lude.Maybe [AdditionalAuthenticationProvider]) (\s a -> s {additionalAuthenticationProviders = a} :: CreateGraphqlAPI)
{-# DEPRECATED cgaAdditionalAuthenticationProviders "Use generic-lens or generic-optics with 'additionalAuthenticationProviders' instead." #-}

-- | A user-supplied name for the @GraphqlApi@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgaName :: Lens.Lens' CreateGraphqlAPI Lude.Text
cgaName = Lens.lens (name :: CreateGraphqlAPI -> Lude.Text) (\s a -> s {name = a} :: CreateGraphqlAPI)
{-# DEPRECATED cgaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon Cognito user pool configuration.
--
-- /Note:/ Consider using 'userPoolConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgaUserPoolConfig :: Lens.Lens' CreateGraphqlAPI (Lude.Maybe UserPoolConfig)
cgaUserPoolConfig = Lens.lens (userPoolConfig :: CreateGraphqlAPI -> Lude.Maybe UserPoolConfig) (\s a -> s {userPoolConfig = a} :: CreateGraphqlAPI)
{-# DEPRECATED cgaUserPoolConfig "Use generic-lens or generic-optics with 'userPoolConfig' instead." #-}

-- | The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user pools.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgaAuthenticationType :: Lens.Lens' CreateGraphqlAPI AuthenticationType
cgaAuthenticationType = Lens.lens (authenticationType :: CreateGraphqlAPI -> AuthenticationType) (\s a -> s {authenticationType = a} :: CreateGraphqlAPI)
{-# DEPRECATED cgaAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

-- | The Amazon CloudWatch Logs configuration.
--
-- /Note:/ Consider using 'logConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgaLogConfig :: Lens.Lens' CreateGraphqlAPI (Lude.Maybe LogConfig)
cgaLogConfig = Lens.lens (logConfig :: CreateGraphqlAPI -> Lude.Maybe LogConfig) (\s a -> s {logConfig = a} :: CreateGraphqlAPI)
{-# DEPRECATED cgaLogConfig "Use generic-lens or generic-optics with 'logConfig' instead." #-}

-- | A @TagMap@ object.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgaTags :: Lens.Lens' CreateGraphqlAPI (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cgaTags = Lens.lens (tags :: CreateGraphqlAPI -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateGraphqlAPI)
{-# DEPRECATED cgaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateGraphqlAPI where
  type Rs CreateGraphqlAPI = CreateGraphqlAPIResponse
  request = Req.postJSON appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateGraphqlAPIResponse'
            Lude.<$> (x Lude..?> "graphqlApi") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGraphqlAPI where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateGraphqlAPI where
  toJSON CreateGraphqlAPI' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("xrayEnabled" Lude..=) Lude.<$> xrayEnabled,
            ("openIDConnectConfig" Lude..=) Lude.<$> openIdConnectConfig,
            ("additionalAuthenticationProviders" Lude..=)
              Lude.<$> additionalAuthenticationProviders,
            Lude.Just ("name" Lude..= name),
            ("userPoolConfig" Lude..=) Lude.<$> userPoolConfig,
            Lude.Just ("authenticationType" Lude..= authenticationType),
            ("logConfig" Lude..=) Lude.<$> logConfig,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateGraphqlAPI where
  toPath = Lude.const "/v1/apis"

instance Lude.ToQuery CreateGraphqlAPI where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateGraphqlAPIResponse' smart constructor.
data CreateGraphqlAPIResponse = CreateGraphqlAPIResponse'
  { -- | The @GraphqlApi@ .
    graphqlAPI :: Lude.Maybe GraphqlAPI,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGraphqlAPIResponse' with the minimum fields required to make a request.
--
-- * 'graphqlAPI' - The @GraphqlApi@ .
-- * 'responseStatus' - The response status code.
mkCreateGraphqlAPIResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGraphqlAPIResponse
mkCreateGraphqlAPIResponse pResponseStatus_ =
  CreateGraphqlAPIResponse'
    { graphqlAPI = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @GraphqlApi@ .
--
-- /Note:/ Consider using 'graphqlAPI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgarsGraphqlAPI :: Lens.Lens' CreateGraphqlAPIResponse (Lude.Maybe GraphqlAPI)
cgarsGraphqlAPI = Lens.lens (graphqlAPI :: CreateGraphqlAPIResponse -> Lude.Maybe GraphqlAPI) (\s a -> s {graphqlAPI = a} :: CreateGraphqlAPIResponse)
{-# DEPRECATED cgarsGraphqlAPI "Use generic-lens or generic-optics with 'graphqlAPI' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgarsResponseStatus :: Lens.Lens' CreateGraphqlAPIResponse Lude.Int
cgarsResponseStatus = Lens.lens (responseStatus :: CreateGraphqlAPIResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGraphqlAPIResponse)
{-# DEPRECATED cgarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
