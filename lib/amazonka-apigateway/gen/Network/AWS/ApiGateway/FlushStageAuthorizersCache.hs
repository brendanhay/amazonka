{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.FlushStageAuthorizersCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Flushes all authorizer cache entries on a stage.
module Network.AWS.ApiGateway.FlushStageAuthorizersCache
    (
    -- * Creating a request
      FlushStageAuthorizersCache (..)
    , mkFlushStageAuthorizersCache
    -- ** Request lenses
    , fsacRestApiId
    , fsacStageName

    -- * Destructuring the response
    , FlushStageAuthorizersCacheResponse (..)
    , mkFlushStageAuthorizersCacheResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to flush authorizer cache entries on a specified stage.
--
-- /See:/ 'mkFlushStageAuthorizersCache' smart constructor.
data FlushStageAuthorizersCache = FlushStageAuthorizersCache'
  { restApiId :: Core.Text
    -- ^ The string identifier of the associated 'RestApi' .
  , stageName :: Core.Text
    -- ^ The name of the stage to flush.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FlushStageAuthorizersCache' value with any optional fields omitted.
mkFlushStageAuthorizersCache
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'stageName'
    -> FlushStageAuthorizersCache
mkFlushStageAuthorizersCache restApiId stageName
  = FlushStageAuthorizersCache'{restApiId, stageName}

-- | The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsacRestApiId :: Lens.Lens' FlushStageAuthorizersCache Core.Text
fsacRestApiId = Lens.field @"restApiId"
{-# INLINEABLE fsacRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | The name of the stage to flush.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsacStageName :: Lens.Lens' FlushStageAuthorizersCache Core.Text
fsacStageName = Lens.field @"stageName"
{-# INLINEABLE fsacStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

instance Core.ToQuery FlushStageAuthorizersCache where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders FlushStageAuthorizersCache where
        toHeaders FlushStageAuthorizersCache{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest FlushStageAuthorizersCache where
        type Rs FlushStageAuthorizersCache =
             FlushStageAuthorizersCacheResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/stages/"
                             Core.<> Core.toText stageName
                             Core.<> "/cache/authorizers",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull FlushStageAuthorizersCacheResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkFlushStageAuthorizersCacheResponse' smart constructor.
data FlushStageAuthorizersCacheResponse = FlushStageAuthorizersCacheResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FlushStageAuthorizersCacheResponse' value with any optional fields omitted.
mkFlushStageAuthorizersCacheResponse
    :: FlushStageAuthorizersCacheResponse
mkFlushStageAuthorizersCacheResponse
  = FlushStageAuthorizersCacheResponse'
