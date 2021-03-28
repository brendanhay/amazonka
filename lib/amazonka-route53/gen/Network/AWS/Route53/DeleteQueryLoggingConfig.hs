{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteQueryLoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration for DNS query logging. If you delete a configuration, Amazon Route 53 stops sending query logs to CloudWatch Logs. Route 53 doesn't delete any logs that are already in CloudWatch Logs.
--
-- For more information about DNS query logs, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateQueryLoggingConfig.html CreateQueryLoggingConfig> .
module Network.AWS.Route53.DeleteQueryLoggingConfig
    (
    -- * Creating a request
      DeleteQueryLoggingConfig (..)
    , mkDeleteQueryLoggingConfig
    -- ** Request lenses
    , dqlcId

    -- * Destructuring the response
    , DeleteQueryLoggingConfigResponse (..)
    , mkDeleteQueryLoggingConfigResponse
    -- ** Response lenses
    , dqlcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | /See:/ 'mkDeleteQueryLoggingConfig' smart constructor.
newtype DeleteQueryLoggingConfig = DeleteQueryLoggingConfig'
  { id :: Types.Id
    -- ^ The ID of the configuration that you want to delete. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQueryLoggingConfig' value with any optional fields omitted.
mkDeleteQueryLoggingConfig
    :: Types.Id -- ^ 'id'
    -> DeleteQueryLoggingConfig
mkDeleteQueryLoggingConfig id = DeleteQueryLoggingConfig'{id}

-- | The ID of the configuration that you want to delete. 
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqlcId :: Lens.Lens' DeleteQueryLoggingConfig Types.Id
dqlcId = Lens.field @"id"
{-# INLINEABLE dqlcId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DeleteQueryLoggingConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteQueryLoggingConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteQueryLoggingConfig where
        type Rs DeleteQueryLoggingConfig = DeleteQueryLoggingConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/2013-04-01/queryloggingconfig/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteQueryLoggingConfigResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteQueryLoggingConfigResponse' smart constructor.
newtype DeleteQueryLoggingConfigResponse = DeleteQueryLoggingConfigResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQueryLoggingConfigResponse' value with any optional fields omitted.
mkDeleteQueryLoggingConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteQueryLoggingConfigResponse
mkDeleteQueryLoggingConfigResponse responseStatus
  = DeleteQueryLoggingConfigResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqlcrrsResponseStatus :: Lens.Lens' DeleteQueryLoggingConfigResponse Core.Int
dqlcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dqlcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
