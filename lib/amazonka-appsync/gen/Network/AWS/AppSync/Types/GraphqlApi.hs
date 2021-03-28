{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.GraphqlApi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.GraphqlApi
  ( GraphqlApi (..)
  -- * Smart constructor
  , mkGraphqlApi
  -- * Lenses
  , gaAdditionalAuthenticationProviders
  , gaApiId
  , gaArn
  , gaAuthenticationType
  , gaLogConfig
  , gaName
  , gaOpenIDConnectConfig
  , gaTags
  , gaUris
  , gaUserPoolConfig
  , gaWafWebAclArn
  , gaXrayEnabled
  ) where

import qualified Network.AWS.AppSync.Types.AdditionalAuthenticationProvider as Types
import qualified Network.AWS.AppSync.Types.AuthenticationType as Types
import qualified Network.AWS.AppSync.Types.LogConfig as Types
import qualified Network.AWS.AppSync.Types.OpenIDConnectConfig as Types
import qualified Network.AWS.AppSync.Types.ResourceName as Types
import qualified Network.AWS.AppSync.Types.TagKey as Types
import qualified Network.AWS.AppSync.Types.TagValue as Types
import qualified Network.AWS.AppSync.Types.UserPoolConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a GraphQL API.
--
-- /See:/ 'mkGraphqlApi' smart constructor.
data GraphqlApi = GraphqlApi'
  { additionalAuthenticationProviders :: Core.Maybe [Types.AdditionalAuthenticationProvider]
    -- ^ A list of additional authentication providers for the @GraphqlApi@ API.
  , apiId :: Core.Maybe Core.Text
    -- ^ The API ID.
  , arn :: Core.Maybe Core.Text
    -- ^ The ARN.
  , authenticationType :: Core.Maybe Types.AuthenticationType
    -- ^ The authentication type.
  , logConfig :: Core.Maybe Types.LogConfig
    -- ^ The Amazon CloudWatch Logs configuration.
  , name :: Core.Maybe Types.ResourceName
    -- ^ The API name.
  , openIDConnectConfig :: Core.Maybe Types.OpenIDConnectConfig
    -- ^ The OpenID Connect configuration.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags.
  , uris :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The URIs.
  , userPoolConfig :: Core.Maybe Types.UserPoolConfig
    -- ^ The Amazon Cognito user pool configuration.
  , wafWebAclArn :: Core.Maybe Core.Text
    -- ^ The ARN of the AWS Web Application Firewall (WAF) ACL associated with this @GraphqlApi@ , if one exists.
  , xrayEnabled :: Core.Maybe Core.Bool
    -- ^ A flag representing whether X-Ray tracing is enabled for this @GraphqlApi@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GraphqlApi' value with any optional fields omitted.
mkGraphqlApi
    :: GraphqlApi
mkGraphqlApi
  = GraphqlApi'{additionalAuthenticationProviders = Core.Nothing,
                apiId = Core.Nothing, arn = Core.Nothing,
                authenticationType = Core.Nothing, logConfig = Core.Nothing,
                name = Core.Nothing, openIDConnectConfig = Core.Nothing,
                tags = Core.Nothing, uris = Core.Nothing,
                userPoolConfig = Core.Nothing, wafWebAclArn = Core.Nothing,
                xrayEnabled = Core.Nothing}

-- | A list of additional authentication providers for the @GraphqlApi@ API.
--
-- /Note:/ Consider using 'additionalAuthenticationProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAdditionalAuthenticationProviders :: Lens.Lens' GraphqlApi (Core.Maybe [Types.AdditionalAuthenticationProvider])
gaAdditionalAuthenticationProviders = Lens.field @"additionalAuthenticationProviders"
{-# INLINEABLE gaAdditionalAuthenticationProviders #-}
{-# DEPRECATED additionalAuthenticationProviders "Use generic-lens or generic-optics with 'additionalAuthenticationProviders' instead"  #-}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaApiId :: Lens.Lens' GraphqlApi (Core.Maybe Core.Text)
gaApiId = Lens.field @"apiId"
{-# INLINEABLE gaApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaArn :: Lens.Lens' GraphqlApi (Core.Maybe Core.Text)
gaArn = Lens.field @"arn"
{-# INLINEABLE gaArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The authentication type.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAuthenticationType :: Lens.Lens' GraphqlApi (Core.Maybe Types.AuthenticationType)
gaAuthenticationType = Lens.field @"authenticationType"
{-# INLINEABLE gaAuthenticationType #-}
{-# DEPRECATED authenticationType "Use generic-lens or generic-optics with 'authenticationType' instead"  #-}

-- | The Amazon CloudWatch Logs configuration.
--
-- /Note:/ Consider using 'logConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaLogConfig :: Lens.Lens' GraphqlApi (Core.Maybe Types.LogConfig)
gaLogConfig = Lens.field @"logConfig"
{-# INLINEABLE gaLogConfig #-}
{-# DEPRECATED logConfig "Use generic-lens or generic-optics with 'logConfig' instead"  #-}

-- | The API name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaName :: Lens.Lens' GraphqlApi (Core.Maybe Types.ResourceName)
gaName = Lens.field @"name"
{-# INLINEABLE gaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The OpenID Connect configuration.
--
-- /Note:/ Consider using 'openIDConnectConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaOpenIDConnectConfig :: Lens.Lens' GraphqlApi (Core.Maybe Types.OpenIDConnectConfig)
gaOpenIDConnectConfig = Lens.field @"openIDConnectConfig"
{-# INLINEABLE gaOpenIDConnectConfig #-}
{-# DEPRECATED openIDConnectConfig "Use generic-lens or generic-optics with 'openIDConnectConfig' instead"  #-}

-- | The tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaTags :: Lens.Lens' GraphqlApi (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
gaTags = Lens.field @"tags"
{-# INLINEABLE gaTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The URIs.
--
-- /Note:/ Consider using 'uris' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaUris :: Lens.Lens' GraphqlApi (Core.Maybe (Core.HashMap Core.Text Core.Text))
gaUris = Lens.field @"uris"
{-# INLINEABLE gaUris #-}
{-# DEPRECATED uris "Use generic-lens or generic-optics with 'uris' instead"  #-}

-- | The Amazon Cognito user pool configuration.
--
-- /Note:/ Consider using 'userPoolConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaUserPoolConfig :: Lens.Lens' GraphqlApi (Core.Maybe Types.UserPoolConfig)
gaUserPoolConfig = Lens.field @"userPoolConfig"
{-# INLINEABLE gaUserPoolConfig #-}
{-# DEPRECATED userPoolConfig "Use generic-lens or generic-optics with 'userPoolConfig' instead"  #-}

-- | The ARN of the AWS Web Application Firewall (WAF) ACL associated with this @GraphqlApi@ , if one exists.
--
-- /Note:/ Consider using 'wafWebAclArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaWafWebAclArn :: Lens.Lens' GraphqlApi (Core.Maybe Core.Text)
gaWafWebAclArn = Lens.field @"wafWebAclArn"
{-# INLINEABLE gaWafWebAclArn #-}
{-# DEPRECATED wafWebAclArn "Use generic-lens or generic-optics with 'wafWebAclArn' instead"  #-}

-- | A flag representing whether X-Ray tracing is enabled for this @GraphqlApi@ .
--
-- /Note:/ Consider using 'xrayEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaXrayEnabled :: Lens.Lens' GraphqlApi (Core.Maybe Core.Bool)
gaXrayEnabled = Lens.field @"xrayEnabled"
{-# INLINEABLE gaXrayEnabled #-}
{-# DEPRECATED xrayEnabled "Use generic-lens or generic-optics with 'xrayEnabled' instead"  #-}

instance Core.FromJSON GraphqlApi where
        parseJSON
          = Core.withObject "GraphqlApi" Core.$
              \ x ->
                GraphqlApi' Core.<$>
                  (x Core..:? "additionalAuthenticationProviders") Core.<*>
                    x Core..:? "apiId"
                    Core.<*> x Core..:? "arn"
                    Core.<*> x Core..:? "authenticationType"
                    Core.<*> x Core..:? "logConfig"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "openIDConnectConfig"
                    Core.<*> x Core..:? "tags"
                    Core.<*> x Core..:? "uris"
                    Core.<*> x Core..:? "userPoolConfig"
                    Core.<*> x Core..:? "wafWebAclArn"
                    Core.<*> x Core..:? "xrayEnabled"
