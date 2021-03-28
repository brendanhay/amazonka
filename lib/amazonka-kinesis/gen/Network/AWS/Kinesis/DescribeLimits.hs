{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.DescribeLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the shard limits and usage for the account.
--
-- If you update your account limits, the old limits might be returned for a few minutes.
-- This operation has a limit of one transaction per second per account.
module Network.AWS.Kinesis.DescribeLimits
    (
    -- * Creating a request
      DescribeLimits (..)
    , mkDescribeLimits

    -- * Destructuring the response
    , DescribeLimitsResponse (..)
    , mkDescribeLimitsResponse
    -- ** Response lenses
    , dlrrsShardLimit
    , dlrrsOpenShardCount
    , dlrrsResponseStatus
    ) where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLimits' smart constructor.
data DescribeLimits = DescribeLimits'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLimits' value with any optional fields omitted.
mkDescribeLimits
    :: DescribeLimits
mkDescribeLimits = DescribeLimits'

instance Core.ToQuery DescribeLimits where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeLimits where
        toHeaders DescribeLimits{..}
          = Core.pure ("X-Amz-Target", "Kinesis_20131202.DescribeLimits")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeLimits where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeLimits where
        type Rs DescribeLimits = DescribeLimitsResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeLimitsResponse' Core.<$>
                   (x Core..: "ShardLimit") Core.<*> x Core..: "OpenShardCount"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeLimitsResponse' smart constructor.
data DescribeLimitsResponse = DescribeLimitsResponse'
  { shardLimit :: Core.Natural
    -- ^ The maximum number of shards.
  , openShardCount :: Core.Natural
    -- ^ The number of open shards.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLimitsResponse' value with any optional fields omitted.
mkDescribeLimitsResponse
    :: Core.Natural -- ^ 'shardLimit'
    -> Core.Natural -- ^ 'openShardCount'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeLimitsResponse
mkDescribeLimitsResponse shardLimit openShardCount responseStatus
  = DescribeLimitsResponse'{shardLimit, openShardCount,
                            responseStatus}

-- | The maximum number of shards.
--
-- /Note:/ Consider using 'shardLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsShardLimit :: Lens.Lens' DescribeLimitsResponse Core.Natural
dlrrsShardLimit = Lens.field @"shardLimit"
{-# INLINEABLE dlrrsShardLimit #-}
{-# DEPRECATED shardLimit "Use generic-lens or generic-optics with 'shardLimit' instead"  #-}

-- | The number of open shards.
--
-- /Note:/ Consider using 'openShardCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsOpenShardCount :: Lens.Lens' DescribeLimitsResponse Core.Natural
dlrrsOpenShardCount = Lens.field @"openShardCount"
{-# INLINEABLE dlrrsOpenShardCount #-}
{-# DEPRECATED openShardCount "Use generic-lens or generic-optics with 'openShardCount' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsResponseStatus :: Lens.Lens' DescribeLimitsResponse Core.Int
dlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
