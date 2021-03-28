{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DeleteBroker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a broker. Note: This API is asynchronous.
module Network.AWS.MQ.DeleteBroker
    (
    -- * Creating a request
      DeleteBroker (..)
    , mkDeleteBroker
    -- ** Request lenses
    , dbBrokerId

    -- * Destructuring the response
    , DeleteBrokerResponse (..)
    , mkDeleteBrokerResponse
    -- ** Response lenses
    , drsBrokerId
    , drsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBroker' smart constructor.
newtype DeleteBroker = DeleteBroker'
  { brokerId :: Core.Text
    -- ^ The unique ID that Amazon MQ generates for the broker.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBroker' value with any optional fields omitted.
mkDeleteBroker
    :: Core.Text -- ^ 'brokerId'
    -> DeleteBroker
mkDeleteBroker brokerId = DeleteBroker'{brokerId}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBrokerId :: Lens.Lens' DeleteBroker Core.Text
dbBrokerId = Lens.field @"brokerId"
{-# INLINEABLE dbBrokerId #-}
{-# DEPRECATED brokerId "Use generic-lens or generic-optics with 'brokerId' instead"  #-}

instance Core.ToQuery DeleteBroker where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteBroker where
        toHeaders DeleteBroker{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteBroker where
        type Rs DeleteBroker = DeleteBrokerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/v1/brokers/" Core.<> Core.toText brokerId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteBrokerResponse' Core.<$>
                   (x Core..:? "brokerId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBrokerResponse' smart constructor.
data DeleteBrokerResponse = DeleteBrokerResponse'
  { brokerId :: Core.Maybe Core.Text
    -- ^ The unique ID that Amazon MQ generates for the broker.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBrokerResponse' value with any optional fields omitted.
mkDeleteBrokerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteBrokerResponse
mkDeleteBrokerResponse responseStatus
  = DeleteBrokerResponse'{brokerId = Core.Nothing, responseStatus}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsBrokerId :: Lens.Lens' DeleteBrokerResponse (Core.Maybe Core.Text)
drsBrokerId = Lens.field @"brokerId"
{-# INLINEABLE drsBrokerId #-}
{-# DEPRECATED brokerId "Use generic-lens or generic-optics with 'brokerId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteBrokerResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
