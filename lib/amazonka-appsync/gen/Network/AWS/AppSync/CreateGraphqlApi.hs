{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.CreateGraphqlApi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @GraphqlApi@ object.
module Network.AWS.AppSync.CreateGraphqlApi
    (
    -- * Creating a request
      CreateGraphqlApi (..)
    , mkCreateGraphqlApi
    -- ** Request lenses
    , cgaName
    , cgaAuthenticationType
    , cgaAdditionalAuthenticationProviders
    , cgaLogConfig
    , cgaOpenIDConnectConfig
    , cgaTags
    , cgaUserPoolConfig
    , cgaXrayEnabled

    -- * Destructuring the response
    , CreateGraphqlApiResponse (..)
    , mkCreateGraphqlApiResponse
    -- ** Response lenses
    , cgarrsGraphqlApi
    , cgarrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateGraphqlApi' smart constructor.
data CreateGraphqlApi = CreateGraphqlApi'
  { name :: Core.Text
    -- ^ A user-supplied name for the @GraphqlApi@ .
  , authenticationType :: Types.AuthenticationType
    -- ^ The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user pools.
  , additionalAuthenticationProviders :: Core.Maybe [Types.AdditionalAuthenticationProvider]
    -- ^ A list of additional authentication providers for the @GraphqlApi@ API.
  , logConfig :: Core.Maybe Types.LogConfig
    -- ^ The Amazon CloudWatch Logs configuration.
  , openIDConnectConfig :: Core.Maybe Types.OpenIDConnectConfig
    -- ^ The OpenID Connect configuration.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ A @TagMap@ object.
  , userPoolConfig :: Core.Maybe Types.UserPoolConfig
    -- ^ The Amazon Cognito user pool configuration.
  , xrayEnabled :: Core.Maybe Core.Bool
    -- ^ A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGraphqlApi' value with any optional fields omitted.
mkCreateGraphqlApi
    :: Core.Text -- ^ 'name'
    -> Types.AuthenticationType -- ^ 'authenticationType'
    -> CreateGraphqlApi
mkCreateGraphqlApi name authenticationType
  = CreateGraphqlApi'{name, authenticationType,
                      additionalAuthenticationProviders = Core.Nothing,
                      logConfig = Core.Nothing, openIDConnectConfig = Core.Nothing,
                      tags = Core.Nothing, userPoolConfig = Core.Nothing,
                      xrayEnabled = Core.Nothing}

-- | A user-supplied name for the @GraphqlApi@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgaName :: Lens.Lens' CreateGraphqlApi Core.Text
cgaName = Lens.field @"name"
{-# INLINEABLE cgaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user pools.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgaAuthenticationType :: Lens.Lens' CreateGraphqlApi Types.AuthenticationType
cgaAuthenticationType = Lens.field @"authenticationType"
{-# INLINEABLE cgaAuthenticationType #-}
{-# DEPRECATED authenticationType "Use generic-lens or generic-optics with 'authenticationType' instead"  #-}

-- | A list of additional authentication providers for the @GraphqlApi@ API.
--
-- /Note:/ Consider using 'additionalAuthenticationProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgaAdditionalAuthenticationProviders :: Lens.Lens' CreateGraphqlApi (Core.Maybe [Types.AdditionalAuthenticationProvider])
cgaAdditionalAuthenticationProviders = Lens.field @"additionalAuthenticationProviders"
{-# INLINEABLE cgaAdditionalAuthenticationProviders #-}
{-# DEPRECATED additionalAuthenticationProviders "Use generic-lens or generic-optics with 'additionalAuthenticationProviders' instead"  #-}

-- | The Amazon CloudWatch Logs configuration.
--
-- /Note:/ Consider using 'logConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgaLogConfig :: Lens.Lens' CreateGraphqlApi (Core.Maybe Types.LogConfig)
cgaLogConfig = Lens.field @"logConfig"
{-# INLINEABLE cgaLogConfig #-}
{-# DEPRECATED logConfig "Use generic-lens or generic-optics with 'logConfig' instead"  #-}

-- | The OpenID Connect configuration.
--
-- /Note:/ Consider using 'openIDConnectConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgaOpenIDConnectConfig :: Lens.Lens' CreateGraphqlApi (Core.Maybe Types.OpenIDConnectConfig)
cgaOpenIDConnectConfig = Lens.field @"openIDConnectConfig"
{-# INLINEABLE cgaOpenIDConnectConfig #-}
{-# DEPRECATED openIDConnectConfig "Use generic-lens or generic-optics with 'openIDConnectConfig' instead"  #-}

-- | A @TagMap@ object.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgaTags :: Lens.Lens' CreateGraphqlApi (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cgaTags = Lens.field @"tags"
{-# INLINEABLE cgaTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The Amazon Cognito user pool configuration.
--
-- /Note:/ Consider using 'userPoolConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgaUserPoolConfig :: Lens.Lens' CreateGraphqlApi (Core.Maybe Types.UserPoolConfig)
cgaUserPoolConfig = Lens.field @"userPoolConfig"
{-# INLINEABLE cgaUserPoolConfig #-}
{-# DEPRECATED userPoolConfig "Use generic-lens or generic-optics with 'userPoolConfig' instead"  #-}

-- | A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@ .
--
-- /Note:/ Consider using 'xrayEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgaXrayEnabled :: Lens.Lens' CreateGraphqlApi (Core.Maybe Core.Bool)
cgaXrayEnabled = Lens.field @"xrayEnabled"
{-# INLINEABLE cgaXrayEnabled #-}
{-# DEPRECATED xrayEnabled "Use generic-lens or generic-optics with 'xrayEnabled' instead"  #-}

instance Core.ToQuery CreateGraphqlApi where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateGraphqlApi where
        toHeaders CreateGraphqlApi{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateGraphqlApi where
        toJSON CreateGraphqlApi{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("authenticationType" Core..= authenticationType),
                  ("additionalAuthenticationProviders" Core..=) Core.<$>
                    additionalAuthenticationProviders,
                  ("logConfig" Core..=) Core.<$> logConfig,
                  ("openIDConnectConfig" Core..=) Core.<$> openIDConnectConfig,
                  ("tags" Core..=) Core.<$> tags,
                  ("userPoolConfig" Core..=) Core.<$> userPoolConfig,
                  ("xrayEnabled" Core..=) Core.<$> xrayEnabled])

instance Core.AWSRequest CreateGraphqlApi where
        type Rs CreateGraphqlApi = CreateGraphqlApiResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/v1/apis",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateGraphqlApiResponse' Core.<$>
                   (x Core..:? "graphqlApi") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateGraphqlApiResponse' smart constructor.
data CreateGraphqlApiResponse = CreateGraphqlApiResponse'
  { graphqlApi :: Core.Maybe Types.GraphqlApi
    -- ^ The @GraphqlApi@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGraphqlApiResponse' value with any optional fields omitted.
mkCreateGraphqlApiResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateGraphqlApiResponse
mkCreateGraphqlApiResponse responseStatus
  = CreateGraphqlApiResponse'{graphqlApi = Core.Nothing,
                              responseStatus}

-- | The @GraphqlApi@ .
--
-- /Note:/ Consider using 'graphqlApi' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgarrsGraphqlApi :: Lens.Lens' CreateGraphqlApiResponse (Core.Maybe Types.GraphqlApi)
cgarrsGraphqlApi = Lens.field @"graphqlApi"
{-# INLINEABLE cgarrsGraphqlApi #-}
{-# DEPRECATED graphqlApi "Use generic-lens or generic-optics with 'graphqlApi' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgarrsResponseStatus :: Lens.Lens' CreateGraphqlApiResponse Core.Int
cgarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cgarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
