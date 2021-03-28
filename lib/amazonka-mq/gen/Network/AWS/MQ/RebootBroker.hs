{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.RebootBroker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a broker. Note: This API is asynchronous.
module Network.AWS.MQ.RebootBroker
    (
    -- * Creating a request
      RebootBroker (..)
    , mkRebootBroker
    -- ** Request lenses
    , rbBrokerId

    -- * Destructuring the response
    , RebootBrokerResponse (..)
    , mkRebootBrokerResponse
    -- ** Response lenses
    , rbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRebootBroker' smart constructor.
newtype RebootBroker = RebootBroker'
  { brokerId :: Core.Text
    -- ^ The unique ID that Amazon MQ generates for the broker.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RebootBroker' value with any optional fields omitted.
mkRebootBroker
    :: Core.Text -- ^ 'brokerId'
    -> RebootBroker
mkRebootBroker brokerId = RebootBroker'{brokerId}

-- | The unique ID that Amazon MQ generates for the broker.
--
-- /Note:/ Consider using 'brokerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbBrokerId :: Lens.Lens' RebootBroker Core.Text
rbBrokerId = Lens.field @"brokerId"
{-# INLINEABLE rbBrokerId #-}
{-# DEPRECATED brokerId "Use generic-lens or generic-optics with 'brokerId' instead"  #-}

instance Core.ToQuery RebootBroker where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RebootBroker where
        toHeaders RebootBroker{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RebootBroker where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest RebootBroker where
        type Rs RebootBroker = RebootBrokerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/brokers/" Core.<> Core.toText brokerId Core.<> "/reboot",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 RebootBrokerResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRebootBrokerResponse' smart constructor.
newtype RebootBrokerResponse = RebootBrokerResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RebootBrokerResponse' value with any optional fields omitted.
mkRebootBrokerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RebootBrokerResponse
mkRebootBrokerResponse responseStatus
  = RebootBrokerResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrrsResponseStatus :: Lens.Lens' RebootBrokerResponse Core.Int
rbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
