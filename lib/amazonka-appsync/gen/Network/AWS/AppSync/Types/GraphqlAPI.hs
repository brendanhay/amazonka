{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.GraphqlAPI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.GraphqlAPI
  ( GraphqlAPI (..),

    -- * Smart constructor
    mkGraphqlAPI,

    -- * Lenses
    gaXrayEnabled,
    gaArn,
    gaApiId,
    gaUris,
    gaOpenIdConnectConfig,
    gaWafWebACLARN,
    gaAdditionalAuthenticationProviders,
    gaName,
    gaUserPoolConfig,
    gaAuthenticationType,
    gaLogConfig,
    gaTags,
  )
where

import Network.AWS.AppSync.Types.AdditionalAuthenticationProvider
import Network.AWS.AppSync.Types.AuthenticationType
import Network.AWS.AppSync.Types.LogConfig
import Network.AWS.AppSync.Types.OpenIdConnectConfig
import Network.AWS.AppSync.Types.UserPoolConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a GraphQL API.
--
-- /See:/ 'mkGraphqlAPI' smart constructor.
data GraphqlAPI = GraphqlAPI'
  { -- | A flag representing whether X-Ray tracing is enabled for this @GraphqlApi@ .
    xrayEnabled :: Lude.Maybe Lude.Bool,
    -- | The ARN.
    arn :: Lude.Maybe Lude.Text,
    -- | The API ID.
    apiId :: Lude.Maybe Lude.Text,
    -- | The URIs.
    uris :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The OpenID Connect configuration.
    openIdConnectConfig :: Lude.Maybe OpenIdConnectConfig,
    -- | The ARN of the AWS Web Application Firewall (WAF) ACL associated with this @GraphqlApi@ , if one exists.
    wafWebACLARN :: Lude.Maybe Lude.Text,
    -- | A list of additional authentication providers for the @GraphqlApi@ API.
    additionalAuthenticationProviders :: Lude.Maybe [AdditionalAuthenticationProvider],
    -- | The API name.
    name :: Lude.Maybe Lude.Text,
    -- | The Amazon Cognito user pool configuration.
    userPoolConfig :: Lude.Maybe UserPoolConfig,
    -- | The authentication type.
    authenticationType :: Lude.Maybe AuthenticationType,
    -- | The Amazon CloudWatch Logs configuration.
    logConfig :: Lude.Maybe LogConfig,
    -- | The tags.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GraphqlAPI' with the minimum fields required to make a request.
--
-- * 'xrayEnabled' - A flag representing whether X-Ray tracing is enabled for this @GraphqlApi@ .
-- * 'arn' - The ARN.
-- * 'apiId' - The API ID.
-- * 'uris' - The URIs.
-- * 'openIdConnectConfig' - The OpenID Connect configuration.
-- * 'wafWebACLARN' - The ARN of the AWS Web Application Firewall (WAF) ACL associated with this @GraphqlApi@ , if one exists.
-- * 'additionalAuthenticationProviders' - A list of additional authentication providers for the @GraphqlApi@ API.
-- * 'name' - The API name.
-- * 'userPoolConfig' - The Amazon Cognito user pool configuration.
-- * 'authenticationType' - The authentication type.
-- * 'logConfig' - The Amazon CloudWatch Logs configuration.
-- * 'tags' - The tags.
mkGraphqlAPI ::
  GraphqlAPI
mkGraphqlAPI =
  GraphqlAPI'
    { xrayEnabled = Lude.Nothing,
      arn = Lude.Nothing,
      apiId = Lude.Nothing,
      uris = Lude.Nothing,
      openIdConnectConfig = Lude.Nothing,
      wafWebACLARN = Lude.Nothing,
      additionalAuthenticationProviders = Lude.Nothing,
      name = Lude.Nothing,
      userPoolConfig = Lude.Nothing,
      authenticationType = Lude.Nothing,
      logConfig = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A flag representing whether X-Ray tracing is enabled for this @GraphqlApi@ .
--
-- /Note:/ Consider using 'xrayEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaXrayEnabled :: Lens.Lens' GraphqlAPI (Lude.Maybe Lude.Bool)
gaXrayEnabled = Lens.lens (xrayEnabled :: GraphqlAPI -> Lude.Maybe Lude.Bool) (\s a -> s {xrayEnabled = a} :: GraphqlAPI)
{-# DEPRECATED gaXrayEnabled "Use generic-lens or generic-optics with 'xrayEnabled' instead." #-}

-- | The ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaArn :: Lens.Lens' GraphqlAPI (Lude.Maybe Lude.Text)
gaArn = Lens.lens (arn :: GraphqlAPI -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GraphqlAPI)
{-# DEPRECATED gaArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaApiId :: Lens.Lens' GraphqlAPI (Lude.Maybe Lude.Text)
gaApiId = Lens.lens (apiId :: GraphqlAPI -> Lude.Maybe Lude.Text) (\s a -> s {apiId = a} :: GraphqlAPI)
{-# DEPRECATED gaApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The URIs.
--
-- /Note:/ Consider using 'uris' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaUris :: Lens.Lens' GraphqlAPI (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gaUris = Lens.lens (uris :: GraphqlAPI -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {uris = a} :: GraphqlAPI)
{-# DEPRECATED gaUris "Use generic-lens or generic-optics with 'uris' instead." #-}

-- | The OpenID Connect configuration.
--
-- /Note:/ Consider using 'openIdConnectConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaOpenIdConnectConfig :: Lens.Lens' GraphqlAPI (Lude.Maybe OpenIdConnectConfig)
gaOpenIdConnectConfig = Lens.lens (openIdConnectConfig :: GraphqlAPI -> Lude.Maybe OpenIdConnectConfig) (\s a -> s {openIdConnectConfig = a} :: GraphqlAPI)
{-# DEPRECATED gaOpenIdConnectConfig "Use generic-lens or generic-optics with 'openIdConnectConfig' instead." #-}

-- | The ARN of the AWS Web Application Firewall (WAF) ACL associated with this @GraphqlApi@ , if one exists.
--
-- /Note:/ Consider using 'wafWebACLARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaWafWebACLARN :: Lens.Lens' GraphqlAPI (Lude.Maybe Lude.Text)
gaWafWebACLARN = Lens.lens (wafWebACLARN :: GraphqlAPI -> Lude.Maybe Lude.Text) (\s a -> s {wafWebACLARN = a} :: GraphqlAPI)
{-# DEPRECATED gaWafWebACLARN "Use generic-lens or generic-optics with 'wafWebACLARN' instead." #-}

-- | A list of additional authentication providers for the @GraphqlApi@ API.
--
-- /Note:/ Consider using 'additionalAuthenticationProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAdditionalAuthenticationProviders :: Lens.Lens' GraphqlAPI (Lude.Maybe [AdditionalAuthenticationProvider])
gaAdditionalAuthenticationProviders = Lens.lens (additionalAuthenticationProviders :: GraphqlAPI -> Lude.Maybe [AdditionalAuthenticationProvider]) (\s a -> s {additionalAuthenticationProviders = a} :: GraphqlAPI)
{-# DEPRECATED gaAdditionalAuthenticationProviders "Use generic-lens or generic-optics with 'additionalAuthenticationProviders' instead." #-}

-- | The API name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaName :: Lens.Lens' GraphqlAPI (Lude.Maybe Lude.Text)
gaName = Lens.lens (name :: GraphqlAPI -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GraphqlAPI)
{-# DEPRECATED gaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon Cognito user pool configuration.
--
-- /Note:/ Consider using 'userPoolConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaUserPoolConfig :: Lens.Lens' GraphqlAPI (Lude.Maybe UserPoolConfig)
gaUserPoolConfig = Lens.lens (userPoolConfig :: GraphqlAPI -> Lude.Maybe UserPoolConfig) (\s a -> s {userPoolConfig = a} :: GraphqlAPI)
{-# DEPRECATED gaUserPoolConfig "Use generic-lens or generic-optics with 'userPoolConfig' instead." #-}

-- | The authentication type.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAuthenticationType :: Lens.Lens' GraphqlAPI (Lude.Maybe AuthenticationType)
gaAuthenticationType = Lens.lens (authenticationType :: GraphqlAPI -> Lude.Maybe AuthenticationType) (\s a -> s {authenticationType = a} :: GraphqlAPI)
{-# DEPRECATED gaAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

-- | The Amazon CloudWatch Logs configuration.
--
-- /Note:/ Consider using 'logConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaLogConfig :: Lens.Lens' GraphqlAPI (Lude.Maybe LogConfig)
gaLogConfig = Lens.lens (logConfig :: GraphqlAPI -> Lude.Maybe LogConfig) (\s a -> s {logConfig = a} :: GraphqlAPI)
{-# DEPRECATED gaLogConfig "Use generic-lens or generic-optics with 'logConfig' instead." #-}

-- | The tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaTags :: Lens.Lens' GraphqlAPI (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gaTags = Lens.lens (tags :: GraphqlAPI -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: GraphqlAPI)
{-# DEPRECATED gaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON GraphqlAPI where
  parseJSON =
    Lude.withObject
      "GraphqlAPI"
      ( \x ->
          GraphqlAPI'
            Lude.<$> (x Lude..:? "xrayEnabled")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "apiId")
            Lude.<*> (x Lude..:? "uris" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "openIDConnectConfig")
            Lude.<*> (x Lude..:? "wafWebAclArn")
            Lude.<*> ( x Lude..:? "additionalAuthenticationProviders"
                         Lude..!= Lude.mempty
                     )
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "userPoolConfig")
            Lude.<*> (x Lude..:? "authenticationType")
            Lude.<*> (x Lude..:? "logConfig")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
