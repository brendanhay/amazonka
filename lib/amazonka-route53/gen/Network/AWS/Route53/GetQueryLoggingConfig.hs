{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetQueryLoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified configuration for DNS query logging.
--
-- For more information about DNS query logs, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateQueryLoggingConfig.html CreateQueryLoggingConfig> and <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html Logging DNS Queries> .
module Network.AWS.Route53.GetQueryLoggingConfig
    (
    -- * Creating a request
      GetQueryLoggingConfig (..)
    , mkGetQueryLoggingConfig
    -- ** Request lenses
    , gqlcId

    -- * Destructuring the response
    , GetQueryLoggingConfigResponse (..)
    , mkGetQueryLoggingConfigResponse
    -- ** Response lenses
    , gqlcrrsQueryLoggingConfig
    , gqlcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | /See:/ 'mkGetQueryLoggingConfig' smart constructor.
newtype GetQueryLoggingConfig = GetQueryLoggingConfig'
  { id :: Types.QueryLoggingConfigId
    -- ^ The ID of the configuration for DNS query logging that you want to get information about.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetQueryLoggingConfig' value with any optional fields omitted.
mkGetQueryLoggingConfig
    :: Types.QueryLoggingConfigId -- ^ 'id'
    -> GetQueryLoggingConfig
mkGetQueryLoggingConfig id = GetQueryLoggingConfig'{id}

-- | The ID of the configuration for DNS query logging that you want to get information about.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqlcId :: Lens.Lens' GetQueryLoggingConfig Types.QueryLoggingConfigId
gqlcId = Lens.field @"id"
{-# INLINEABLE gqlcId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetQueryLoggingConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetQueryLoggingConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetQueryLoggingConfig where
        type Rs GetQueryLoggingConfig = GetQueryLoggingConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2013-04-01/queryloggingconfig/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetQueryLoggingConfigResponse' Core.<$>
                   (x Core..@ "QueryLoggingConfig") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetQueryLoggingConfigResponse' smart constructor.
data GetQueryLoggingConfigResponse = GetQueryLoggingConfigResponse'
  { queryLoggingConfig :: Types.QueryLoggingConfig
    -- ^ A complex type that contains information about the query logging configuration that you specified in a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetQueryLoggingConfig.html GetQueryLoggingConfig> request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetQueryLoggingConfigResponse' value with any optional fields omitted.
mkGetQueryLoggingConfigResponse
    :: Types.QueryLoggingConfig -- ^ 'queryLoggingConfig'
    -> Core.Int -- ^ 'responseStatus'
    -> GetQueryLoggingConfigResponse
mkGetQueryLoggingConfigResponse queryLoggingConfig responseStatus
  = GetQueryLoggingConfigResponse'{queryLoggingConfig,
                                   responseStatus}

-- | A complex type that contains information about the query logging configuration that you specified in a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetQueryLoggingConfig.html GetQueryLoggingConfig> request.
--
-- /Note:/ Consider using 'queryLoggingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqlcrrsQueryLoggingConfig :: Lens.Lens' GetQueryLoggingConfigResponse Types.QueryLoggingConfig
gqlcrrsQueryLoggingConfig = Lens.field @"queryLoggingConfig"
{-# INLINEABLE gqlcrrsQueryLoggingConfig #-}
{-# DEPRECATED queryLoggingConfig "Use generic-lens or generic-optics with 'queryLoggingConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqlcrrsResponseStatus :: Lens.Lens' GetQueryLoggingConfigResponse Core.Int
gqlcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gqlcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
