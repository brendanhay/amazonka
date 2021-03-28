{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.FlushStageCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Flushes a stage's cache.
module Network.AWS.ApiGateway.FlushStageCache
    (
    -- * Creating a request
      FlushStageCache (..)
    , mkFlushStageCache
    -- ** Request lenses
    , fscRestApiId
    , fscStageName

    -- * Destructuring the response
    , FlushStageCacheResponse (..)
    , mkFlushStageCacheResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to flush a stage's cache.
--
-- /See:/ 'mkFlushStageCache' smart constructor.
data FlushStageCache = FlushStageCache'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , stageName :: Core.Text
    -- ^ [Required] The name of the stage to flush its cache.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FlushStageCache' value with any optional fields omitted.
mkFlushStageCache
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'stageName'
    -> FlushStageCache
mkFlushStageCache restApiId stageName
  = FlushStageCache'{restApiId, stageName}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscRestApiId :: Lens.Lens' FlushStageCache Core.Text
fscRestApiId = Lens.field @"restApiId"
{-# INLINEABLE fscRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The name of the stage to flush its cache.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscStageName :: Lens.Lens' FlushStageCache Core.Text
fscStageName = Lens.field @"stageName"
{-# INLINEABLE fscStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

instance Core.ToQuery FlushStageCache where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders FlushStageCache where
        toHeaders FlushStageCache{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest FlushStageCache where
        type Rs FlushStageCache = FlushStageCacheResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/stages/"
                             Core.<> Core.toText stageName
                             Core.<> "/cache/data",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull FlushStageCacheResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkFlushStageCacheResponse' smart constructor.
data FlushStageCacheResponse = FlushStageCacheResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FlushStageCacheResponse' value with any optional fields omitted.
mkFlushStageCacheResponse
    :: FlushStageCacheResponse
mkFlushStageCacheResponse = FlushStageCacheResponse'
