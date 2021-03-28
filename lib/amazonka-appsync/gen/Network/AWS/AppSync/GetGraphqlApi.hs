{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetGraphqlApi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @GraphqlApi@ object.
module Network.AWS.AppSync.GetGraphqlApi
    (
    -- * Creating a request
      GetGraphqlApi (..)
    , mkGetGraphqlApi
    -- ** Request lenses
    , ggaApiId

    -- * Destructuring the response
    , GetGraphqlApiResponse (..)
    , mkGetGraphqlApiResponse
    -- ** Response lenses
    , ggarrsGraphqlApi
    , ggarrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetGraphqlApi' smart constructor.
newtype GetGraphqlApi = GetGraphqlApi'
  { apiId :: Core.Text
    -- ^ The API ID for the GraphQL API.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetGraphqlApi' value with any optional fields omitted.
mkGetGraphqlApi
    :: Core.Text -- ^ 'apiId'
    -> GetGraphqlApi
mkGetGraphqlApi apiId = GetGraphqlApi'{apiId}

-- | The API ID for the GraphQL API.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggaApiId :: Lens.Lens' GetGraphqlApi Core.Text
ggaApiId = Lens.field @"apiId"
{-# INLINEABLE ggaApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

instance Core.ToQuery GetGraphqlApi where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetGraphqlApi where
        toHeaders GetGraphqlApi{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetGraphqlApi where
        type Rs GetGraphqlApi = GetGraphqlApiResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/v1/apis/" Core.<> Core.toText apiId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetGraphqlApiResponse' Core.<$>
                   (x Core..:? "graphqlApi") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetGraphqlApiResponse' smart constructor.
data GetGraphqlApiResponse = GetGraphqlApiResponse'
  { graphqlApi :: Core.Maybe Types.GraphqlApi
    -- ^ The @GraphqlApi@ object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGraphqlApiResponse' value with any optional fields omitted.
mkGetGraphqlApiResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetGraphqlApiResponse
mkGetGraphqlApiResponse responseStatus
  = GetGraphqlApiResponse'{graphqlApi = Core.Nothing, responseStatus}

-- | The @GraphqlApi@ object.
--
-- /Note:/ Consider using 'graphqlApi' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggarrsGraphqlApi :: Lens.Lens' GetGraphqlApiResponse (Core.Maybe Types.GraphqlApi)
ggarrsGraphqlApi = Lens.field @"graphqlApi"
{-# INLINEABLE ggarrsGraphqlApi #-}
{-# DEPRECATED graphqlApi "Use generic-lens or generic-optics with 'graphqlApi' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggarrsResponseStatus :: Lens.Lens' GetGraphqlApiResponse Core.Int
ggarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ggarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
