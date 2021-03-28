{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeletePendingAggregationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes pending authorization requests for a specified aggregator account in a specified region.
module Network.AWS.Config.DeletePendingAggregationRequest
    (
    -- * Creating a request
      DeletePendingAggregationRequest (..)
    , mkDeletePendingAggregationRequest
    -- ** Request lenses
    , dparRequesterAccountId
    , dparRequesterAwsRegion

    -- * Destructuring the response
    , DeletePendingAggregationRequestResponse (..)
    , mkDeletePendingAggregationRequestResponse
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePendingAggregationRequest' smart constructor.
data DeletePendingAggregationRequest = DeletePendingAggregationRequest'
  { requesterAccountId :: Types.RequesterAccountId
    -- ^ The 12-digit account ID of the account requesting to aggregate data.
  , requesterAwsRegion :: Types.RequesterAwsRegion
    -- ^ The region requesting to aggregate data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePendingAggregationRequest' value with any optional fields omitted.
mkDeletePendingAggregationRequest
    :: Types.RequesterAccountId -- ^ 'requesterAccountId'
    -> Types.RequesterAwsRegion -- ^ 'requesterAwsRegion'
    -> DeletePendingAggregationRequest
mkDeletePendingAggregationRequest requesterAccountId
  requesterAwsRegion
  = DeletePendingAggregationRequest'{requesterAccountId,
                                     requesterAwsRegion}

-- | The 12-digit account ID of the account requesting to aggregate data.
--
-- /Note:/ Consider using 'requesterAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparRequesterAccountId :: Lens.Lens' DeletePendingAggregationRequest Types.RequesterAccountId
dparRequesterAccountId = Lens.field @"requesterAccountId"
{-# INLINEABLE dparRequesterAccountId #-}
{-# DEPRECATED requesterAccountId "Use generic-lens or generic-optics with 'requesterAccountId' instead"  #-}

-- | The region requesting to aggregate data.
--
-- /Note:/ Consider using 'requesterAwsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparRequesterAwsRegion :: Lens.Lens' DeletePendingAggregationRequest Types.RequesterAwsRegion
dparRequesterAwsRegion = Lens.field @"requesterAwsRegion"
{-# INLINEABLE dparRequesterAwsRegion #-}
{-# DEPRECATED requesterAwsRegion "Use generic-lens or generic-optics with 'requesterAwsRegion' instead"  #-}

instance Core.ToQuery DeletePendingAggregationRequest where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeletePendingAggregationRequest where
        toHeaders DeletePendingAggregationRequest{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.DeletePendingAggregationRequest")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeletePendingAggregationRequest where
        toJSON DeletePendingAggregationRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RequesterAccountId" Core..= requesterAccountId),
                  Core.Just ("RequesterAwsRegion" Core..= requesterAwsRegion)])

instance Core.AWSRequest DeletePendingAggregationRequest where
        type Rs DeletePendingAggregationRequest =
             DeletePendingAggregationRequestResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeletePendingAggregationRequestResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeletePendingAggregationRequestResponse' smart constructor.
data DeletePendingAggregationRequestResponse = DeletePendingAggregationRequestResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePendingAggregationRequestResponse' value with any optional fields omitted.
mkDeletePendingAggregationRequestResponse
    :: DeletePendingAggregationRequestResponse
mkDeletePendingAggregationRequestResponse
  = DeletePendingAggregationRequestResponse'
