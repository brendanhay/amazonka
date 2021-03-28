{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.UpdateGraphqlApi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @GraphqlApi@ object.
module Network.AWS.AppSync.UpdateGraphqlApi
    (
    -- * Creating a request
      UpdateGraphqlApi (..)
    , mkUpdateGraphqlApi
    -- ** Request lenses
    , ugaApiId
    , ugaName
    , ugaAdditionalAuthenticationProviders
    , ugaAuthenticationType
    , ugaLogConfig
    , ugaOpenIDConnectConfig
    , ugaUserPoolConfig
    , ugaXrayEnabled

    -- * Destructuring the response
    , UpdateGraphqlApiResponse (..)
    , mkUpdateGraphqlApiResponse
    -- ** Response lenses
    , ugarrsGraphqlApi
    , ugarrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGraphqlApi' smart constructor.
data UpdateGraphqlApi = UpdateGraphqlApi'
  { apiId :: Core.Text
    -- ^ The API ID.
  , name :: Core.Text
    -- ^ The new name for the @GraphqlApi@ object.
  , additionalAuthenticationProviders :: Core.Maybe [Types.AdditionalAuthenticationProvider]
    -- ^ A list of additional authentication providers for the @GraphqlApi@ API.
  , authenticationType :: Core.Maybe Types.AuthenticationType
    -- ^ The new authentication type for the @GraphqlApi@ object.
  , logConfig :: Core.Maybe Types.LogConfig
    -- ^ The Amazon CloudWatch Logs configuration for the @GraphqlApi@ object.
  , openIDConnectConfig :: Core.Maybe Types.OpenIDConnectConfig
    -- ^ The OpenID Connect configuration for the @GraphqlApi@ object.
  , userPoolConfig :: Core.Maybe Types.UserPoolConfig
    -- ^ The new Amazon Cognito user pool configuration for the @GraphqlApi@ object.
  , xrayEnabled :: Core.Maybe Core.Bool
    -- ^ A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGraphqlApi' value with any optional fields omitted.
mkUpdateGraphqlApi
    :: Core.Text -- ^ 'apiId'
    -> Core.Text -- ^ 'name'
    -> UpdateGraphqlApi
mkUpdateGraphqlApi apiId name
  = UpdateGraphqlApi'{apiId, name,
                      additionalAuthenticationProviders = Core.Nothing,
                      authenticationType = Core.Nothing, logConfig = Core.Nothing,
                      openIDConnectConfig = Core.Nothing, userPoolConfig = Core.Nothing,
                      xrayEnabled = Core.Nothing}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugaApiId :: Lens.Lens' UpdateGraphqlApi Core.Text
ugaApiId = Lens.field @"apiId"
{-# INLINEABLE ugaApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The new name for the @GraphqlApi@ object.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugaName :: Lens.Lens' UpdateGraphqlApi Core.Text
ugaName = Lens.field @"name"
{-# INLINEABLE ugaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of additional authentication providers for the @GraphqlApi@ API.
--
-- /Note:/ Consider using 'additionalAuthenticationProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugaAdditionalAuthenticationProviders :: Lens.Lens' UpdateGraphqlApi (Core.Maybe [Types.AdditionalAuthenticationProvider])
ugaAdditionalAuthenticationProviders = Lens.field @"additionalAuthenticationProviders"
{-# INLINEABLE ugaAdditionalAuthenticationProviders #-}
{-# DEPRECATED additionalAuthenticationProviders "Use generic-lens or generic-optics with 'additionalAuthenticationProviders' instead"  #-}

-- | The new authentication type for the @GraphqlApi@ object.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugaAuthenticationType :: Lens.Lens' UpdateGraphqlApi (Core.Maybe Types.AuthenticationType)
ugaAuthenticationType = Lens.field @"authenticationType"
{-# INLINEABLE ugaAuthenticationType #-}
{-# DEPRECATED authenticationType "Use generic-lens or generic-optics with 'authenticationType' instead"  #-}

-- | The Amazon CloudWatch Logs configuration for the @GraphqlApi@ object.
--
-- /Note:/ Consider using 'logConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugaLogConfig :: Lens.Lens' UpdateGraphqlApi (Core.Maybe Types.LogConfig)
ugaLogConfig = Lens.field @"logConfig"
{-# INLINEABLE ugaLogConfig #-}
{-# DEPRECATED logConfig "Use generic-lens or generic-optics with 'logConfig' instead"  #-}

-- | The OpenID Connect configuration for the @GraphqlApi@ object.
--
-- /Note:/ Consider using 'openIDConnectConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugaOpenIDConnectConfig :: Lens.Lens' UpdateGraphqlApi (Core.Maybe Types.OpenIDConnectConfig)
ugaOpenIDConnectConfig = Lens.field @"openIDConnectConfig"
{-# INLINEABLE ugaOpenIDConnectConfig #-}
{-# DEPRECATED openIDConnectConfig "Use generic-lens or generic-optics with 'openIDConnectConfig' instead"  #-}

-- | The new Amazon Cognito user pool configuration for the @GraphqlApi@ object.
--
-- /Note:/ Consider using 'userPoolConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugaUserPoolConfig :: Lens.Lens' UpdateGraphqlApi (Core.Maybe Types.UserPoolConfig)
ugaUserPoolConfig = Lens.field @"userPoolConfig"
{-# INLINEABLE ugaUserPoolConfig #-}
{-# DEPRECATED userPoolConfig "Use generic-lens or generic-optics with 'userPoolConfig' instead"  #-}

-- | A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@ .
--
-- /Note:/ Consider using 'xrayEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugaXrayEnabled :: Lens.Lens' UpdateGraphqlApi (Core.Maybe Core.Bool)
ugaXrayEnabled = Lens.field @"xrayEnabled"
{-# INLINEABLE ugaXrayEnabled #-}
{-# DEPRECATED xrayEnabled "Use generic-lens or generic-optics with 'xrayEnabled' instead"  #-}

instance Core.ToQuery UpdateGraphqlApi where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateGraphqlApi where
        toHeaders UpdateGraphqlApi{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateGraphqlApi where
        toJSON UpdateGraphqlApi{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  ("additionalAuthenticationProviders" Core..=) Core.<$>
                    additionalAuthenticationProviders,
                  ("authenticationType" Core..=) Core.<$> authenticationType,
                  ("logConfig" Core..=) Core.<$> logConfig,
                  ("openIDConnectConfig" Core..=) Core.<$> openIDConnectConfig,
                  ("userPoolConfig" Core..=) Core.<$> userPoolConfig,
                  ("xrayEnabled" Core..=) Core.<$> xrayEnabled])

instance Core.AWSRequest UpdateGraphqlApi where
        type Rs UpdateGraphqlApi = UpdateGraphqlApiResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/v1/apis/" Core.<> Core.toText apiId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateGraphqlApiResponse' Core.<$>
                   (x Core..:? "graphqlApi") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateGraphqlApiResponse' smart constructor.
data UpdateGraphqlApiResponse = UpdateGraphqlApiResponse'
  { graphqlApi :: Core.Maybe Types.GraphqlApi
    -- ^ The updated @GraphqlApi@ object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGraphqlApiResponse' value with any optional fields omitted.
mkUpdateGraphqlApiResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateGraphqlApiResponse
mkUpdateGraphqlApiResponse responseStatus
  = UpdateGraphqlApiResponse'{graphqlApi = Core.Nothing,
                              responseStatus}

-- | The updated @GraphqlApi@ object.
--
-- /Note:/ Consider using 'graphqlApi' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugarrsGraphqlApi :: Lens.Lens' UpdateGraphqlApiResponse (Core.Maybe Types.GraphqlApi)
ugarrsGraphqlApi = Lens.field @"graphqlApi"
{-# INLINEABLE ugarrsGraphqlApi #-}
{-# DEPRECATED graphqlApi "Use generic-lens or generic-optics with 'graphqlApi' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugarrsResponseStatus :: Lens.Lens' UpdateGraphqlApiResponse Core.Int
ugarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ugarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
