{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.UpdateGraphqlAPI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @GraphqlApi@ object.
module Network.AWS.AppSync.UpdateGraphqlAPI
  ( -- * Creating a request
    UpdateGraphqlAPI (..),
    mkUpdateGraphqlAPI,

    -- ** Request lenses
    ugaXrayEnabled,
    ugaOpenIdConnectConfig,
    ugaAdditionalAuthenticationProviders,
    ugaUserPoolConfig,
    ugaAuthenticationType,
    ugaLogConfig,
    ugaApiId,
    ugaName,

    -- * Destructuring the response
    UpdateGraphqlAPIResponse (..),
    mkUpdateGraphqlAPIResponse,

    -- ** Response lenses
    ugarsGraphqlAPI,
    ugarsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateGraphqlAPI' smart constructor.
data UpdateGraphqlAPI = UpdateGraphqlAPI'
  { xrayEnabled ::
      Lude.Maybe Lude.Bool,
    openIdConnectConfig :: Lude.Maybe OpenIdConnectConfig,
    additionalAuthenticationProviders ::
      Lude.Maybe [AdditionalAuthenticationProvider],
    userPoolConfig :: Lude.Maybe UserPoolConfig,
    authenticationType :: Lude.Maybe AuthenticationType,
    logConfig :: Lude.Maybe LogConfig,
    apiId :: Lude.Text,
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

-- | Creates a value of 'UpdateGraphqlAPI' with the minimum fields required to make a request.
--
-- * 'additionalAuthenticationProviders' - A list of additional authentication providers for the @GraphqlApi@ API.
-- * 'apiId' - The API ID.
-- * 'authenticationType' - The new authentication type for the @GraphqlApi@ object.
-- * 'logConfig' - The Amazon CloudWatch Logs configuration for the @GraphqlApi@ object.
-- * 'name' - The new name for the @GraphqlApi@ object.
-- * 'openIdConnectConfig' - The OpenID Connect configuration for the @GraphqlApi@ object.
-- * 'userPoolConfig' - The new Amazon Cognito user pool configuration for the @GraphqlApi@ object.
-- * 'xrayEnabled' - A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@ .
mkUpdateGraphqlAPI ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  UpdateGraphqlAPI
mkUpdateGraphqlAPI pApiId_ pName_ =
  UpdateGraphqlAPI'
    { xrayEnabled = Lude.Nothing,
      openIdConnectConfig = Lude.Nothing,
      additionalAuthenticationProviders = Lude.Nothing,
      userPoolConfig = Lude.Nothing,
      authenticationType = Lude.Nothing,
      logConfig = Lude.Nothing,
      apiId = pApiId_,
      name = pName_
    }

-- | A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@ .
--
-- /Note:/ Consider using 'xrayEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugaXrayEnabled :: Lens.Lens' UpdateGraphqlAPI (Lude.Maybe Lude.Bool)
ugaXrayEnabled = Lens.lens (xrayEnabled :: UpdateGraphqlAPI -> Lude.Maybe Lude.Bool) (\s a -> s {xrayEnabled = a} :: UpdateGraphqlAPI)
{-# DEPRECATED ugaXrayEnabled "Use generic-lens or generic-optics with 'xrayEnabled' instead." #-}

-- | The OpenID Connect configuration for the @GraphqlApi@ object.
--
-- /Note:/ Consider using 'openIdConnectConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugaOpenIdConnectConfig :: Lens.Lens' UpdateGraphqlAPI (Lude.Maybe OpenIdConnectConfig)
ugaOpenIdConnectConfig = Lens.lens (openIdConnectConfig :: UpdateGraphqlAPI -> Lude.Maybe OpenIdConnectConfig) (\s a -> s {openIdConnectConfig = a} :: UpdateGraphqlAPI)
{-# DEPRECATED ugaOpenIdConnectConfig "Use generic-lens or generic-optics with 'openIdConnectConfig' instead." #-}

-- | A list of additional authentication providers for the @GraphqlApi@ API.
--
-- /Note:/ Consider using 'additionalAuthenticationProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugaAdditionalAuthenticationProviders :: Lens.Lens' UpdateGraphqlAPI (Lude.Maybe [AdditionalAuthenticationProvider])
ugaAdditionalAuthenticationProviders = Lens.lens (additionalAuthenticationProviders :: UpdateGraphqlAPI -> Lude.Maybe [AdditionalAuthenticationProvider]) (\s a -> s {additionalAuthenticationProviders = a} :: UpdateGraphqlAPI)
{-# DEPRECATED ugaAdditionalAuthenticationProviders "Use generic-lens or generic-optics with 'additionalAuthenticationProviders' instead." #-}

-- | The new Amazon Cognito user pool configuration for the @GraphqlApi@ object.
--
-- /Note:/ Consider using 'userPoolConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugaUserPoolConfig :: Lens.Lens' UpdateGraphqlAPI (Lude.Maybe UserPoolConfig)
ugaUserPoolConfig = Lens.lens (userPoolConfig :: UpdateGraphqlAPI -> Lude.Maybe UserPoolConfig) (\s a -> s {userPoolConfig = a} :: UpdateGraphqlAPI)
{-# DEPRECATED ugaUserPoolConfig "Use generic-lens or generic-optics with 'userPoolConfig' instead." #-}

-- | The new authentication type for the @GraphqlApi@ object.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugaAuthenticationType :: Lens.Lens' UpdateGraphqlAPI (Lude.Maybe AuthenticationType)
ugaAuthenticationType = Lens.lens (authenticationType :: UpdateGraphqlAPI -> Lude.Maybe AuthenticationType) (\s a -> s {authenticationType = a} :: UpdateGraphqlAPI)
{-# DEPRECATED ugaAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

-- | The Amazon CloudWatch Logs configuration for the @GraphqlApi@ object.
--
-- /Note:/ Consider using 'logConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugaLogConfig :: Lens.Lens' UpdateGraphqlAPI (Lude.Maybe LogConfig)
ugaLogConfig = Lens.lens (logConfig :: UpdateGraphqlAPI -> Lude.Maybe LogConfig) (\s a -> s {logConfig = a} :: UpdateGraphqlAPI)
{-# DEPRECATED ugaLogConfig "Use generic-lens or generic-optics with 'logConfig' instead." #-}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugaApiId :: Lens.Lens' UpdateGraphqlAPI Lude.Text
ugaApiId = Lens.lens (apiId :: UpdateGraphqlAPI -> Lude.Text) (\s a -> s {apiId = a} :: UpdateGraphqlAPI)
{-# DEPRECATED ugaApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The new name for the @GraphqlApi@ object.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugaName :: Lens.Lens' UpdateGraphqlAPI Lude.Text
ugaName = Lens.lens (name :: UpdateGraphqlAPI -> Lude.Text) (\s a -> s {name = a} :: UpdateGraphqlAPI)
{-# DEPRECATED ugaName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest UpdateGraphqlAPI where
  type Rs UpdateGraphqlAPI = UpdateGraphqlAPIResponse
  request = Req.postJSON appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateGraphqlAPIResponse'
            Lude.<$> (x Lude..?> "graphqlApi") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGraphqlAPI where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateGraphqlAPI where
  toJSON UpdateGraphqlAPI' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("xrayEnabled" Lude..=) Lude.<$> xrayEnabled,
            ("openIDConnectConfig" Lude..=) Lude.<$> openIdConnectConfig,
            ("additionalAuthenticationProviders" Lude..=)
              Lude.<$> additionalAuthenticationProviders,
            ("userPoolConfig" Lude..=) Lude.<$> userPoolConfig,
            ("authenticationType" Lude..=) Lude.<$> authenticationType,
            ("logConfig" Lude..=) Lude.<$> logConfig,
            Lude.Just ("name" Lude..= name)
          ]
      )

instance Lude.ToPath UpdateGraphqlAPI where
  toPath UpdateGraphqlAPI' {..} =
    Lude.mconcat ["/v1/apis/", Lude.toBS apiId]

instance Lude.ToQuery UpdateGraphqlAPI where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateGraphqlAPIResponse' smart constructor.
data UpdateGraphqlAPIResponse = UpdateGraphqlAPIResponse'
  { graphqlAPI ::
      Lude.Maybe GraphqlAPI,
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

-- | Creates a value of 'UpdateGraphqlAPIResponse' with the minimum fields required to make a request.
--
-- * 'graphqlAPI' - The updated @GraphqlApi@ object.
-- * 'responseStatus' - The response status code.
mkUpdateGraphqlAPIResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGraphqlAPIResponse
mkUpdateGraphqlAPIResponse pResponseStatus_ =
  UpdateGraphqlAPIResponse'
    { graphqlAPI = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated @GraphqlApi@ object.
--
-- /Note:/ Consider using 'graphqlAPI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugarsGraphqlAPI :: Lens.Lens' UpdateGraphqlAPIResponse (Lude.Maybe GraphqlAPI)
ugarsGraphqlAPI = Lens.lens (graphqlAPI :: UpdateGraphqlAPIResponse -> Lude.Maybe GraphqlAPI) (\s a -> s {graphqlAPI = a} :: UpdateGraphqlAPIResponse)
{-# DEPRECATED ugarsGraphqlAPI "Use generic-lens or generic-optics with 'graphqlAPI' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugarsResponseStatus :: Lens.Lens' UpdateGraphqlAPIResponse Lude.Int
ugarsResponseStatus = Lens.lens (responseStatus :: UpdateGraphqlAPIResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGraphqlAPIResponse)
{-# DEPRECATED ugarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
