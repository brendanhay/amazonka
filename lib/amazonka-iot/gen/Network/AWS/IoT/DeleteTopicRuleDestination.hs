{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteTopicRuleDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a topic rule destination.
module Network.AWS.IoT.DeleteTopicRuleDestination
    (
    -- * Creating a request
      DeleteTopicRuleDestination (..)
    , mkDeleteTopicRuleDestination
    -- ** Request lenses
    , dtrdArn

    -- * Destructuring the response
    , DeleteTopicRuleDestinationResponse (..)
    , mkDeleteTopicRuleDestinationResponse
    -- ** Response lenses
    , dtrdrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTopicRuleDestination' smart constructor.
newtype DeleteTopicRuleDestination = DeleteTopicRuleDestination'
  { arn :: Types.Arn
    -- ^ The ARN of the topic rule destination to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTopicRuleDestination' value with any optional fields omitted.
mkDeleteTopicRuleDestination
    :: Types.Arn -- ^ 'arn'
    -> DeleteTopicRuleDestination
mkDeleteTopicRuleDestination arn = DeleteTopicRuleDestination'{arn}

-- | The ARN of the topic rule destination to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrdArn :: Lens.Lens' DeleteTopicRuleDestination Types.Arn
dtrdArn = Lens.field @"arn"
{-# INLINEABLE dtrdArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery DeleteTopicRuleDestination where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteTopicRuleDestination where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteTopicRuleDestination where
        type Rs DeleteTopicRuleDestination =
             DeleteTopicRuleDestinationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/destinations/" Core.<> Core.toText arn,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteTopicRuleDestinationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTopicRuleDestinationResponse' smart constructor.
newtype DeleteTopicRuleDestinationResponse = DeleteTopicRuleDestinationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTopicRuleDestinationResponse' value with any optional fields omitted.
mkDeleteTopicRuleDestinationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTopicRuleDestinationResponse
mkDeleteTopicRuleDestinationResponse responseStatus
  = DeleteTopicRuleDestinationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrdrrsResponseStatus :: Lens.Lens' DeleteTopicRuleDestinationResponse Core.Int
dtrdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
