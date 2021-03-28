{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DescribeEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists details about a partner event source that is shared with your account.
module Network.AWS.CloudWatchEvents.DescribeEventSource
    (
    -- * Creating a request
      DescribeEventSource (..)
    , mkDescribeEventSource
    -- ** Request lenses
    , desName

    -- * Destructuring the response
    , DescribeEventSourceResponse (..)
    , mkDescribeEventSourceResponse
    -- ** Response lenses
    , desrrsArn
    , desrrsCreatedBy
    , desrrsCreationTime
    , desrrsExpirationTime
    , desrrsName
    , desrrsState
    , desrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEventSource' smart constructor.
newtype DescribeEventSource = DescribeEventSource'
  { name :: Types.Name
    -- ^ The name of the partner event source to display the details of.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventSource' value with any optional fields omitted.
mkDescribeEventSource
    :: Types.Name -- ^ 'name'
    -> DescribeEventSource
mkDescribeEventSource name = DescribeEventSource'{name}

-- | The name of the partner event source to display the details of.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desName :: Lens.Lens' DescribeEventSource Types.Name
desName = Lens.field @"name"
{-# INLINEABLE desName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DescribeEventSource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEventSource where
        toHeaders DescribeEventSource{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.DescribeEventSource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEventSource where
        toJSON DescribeEventSource{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DescribeEventSource where
        type Rs DescribeEventSource = DescribeEventSourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEventSourceResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreatedBy" Core.<*>
                     x Core..:? "CreationTime"
                     Core.<*> x Core..:? "ExpirationTime"
                     Core.<*> x Core..:? "Name"
                     Core.<*> x Core..:? "State"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeEventSourceResponse' smart constructor.
data DescribeEventSourceResponse = DescribeEventSourceResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ The ARN of the partner event source.
  , createdBy :: Core.Maybe Core.Text
    -- ^ The name of the SaaS partner that created the event source.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the event source was created.
  , expirationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the event source will expire if you do not create a matching event bus.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the partner event source.
  , state :: Core.Maybe Types.EventSourceState
    -- ^ The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEventSourceResponse' value with any optional fields omitted.
mkDescribeEventSourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEventSourceResponse
mkDescribeEventSourceResponse responseStatus
  = DescribeEventSourceResponse'{arn = Core.Nothing,
                                 createdBy = Core.Nothing, creationTime = Core.Nothing,
                                 expirationTime = Core.Nothing, name = Core.Nothing,
                                 state = Core.Nothing, responseStatus}

-- | The ARN of the partner event source.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsArn :: Lens.Lens' DescribeEventSourceResponse (Core.Maybe Core.Text)
desrrsArn = Lens.field @"arn"
{-# INLINEABLE desrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The name of the SaaS partner that created the event source.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsCreatedBy :: Lens.Lens' DescribeEventSourceResponse (Core.Maybe Core.Text)
desrrsCreatedBy = Lens.field @"createdBy"
{-# INLINEABLE desrrsCreatedBy #-}
{-# DEPRECATED createdBy "Use generic-lens or generic-optics with 'createdBy' instead"  #-}

-- | The date and time that the event source was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsCreationTime :: Lens.Lens' DescribeEventSourceResponse (Core.Maybe Core.NominalDiffTime)
desrrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE desrrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The date and time that the event source will expire if you do not create a matching event bus.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsExpirationTime :: Lens.Lens' DescribeEventSourceResponse (Core.Maybe Core.NominalDiffTime)
desrrsExpirationTime = Lens.field @"expirationTime"
{-# INLINEABLE desrrsExpirationTime #-}
{-# DEPRECATED expirationTime "Use generic-lens or generic-optics with 'expirationTime' instead"  #-}

-- | The name of the partner event source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsName :: Lens.Lens' DescribeEventSourceResponse (Core.Maybe Core.Text)
desrrsName = Lens.field @"name"
{-# INLINEABLE desrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsState :: Lens.Lens' DescribeEventSourceResponse (Core.Maybe Types.EventSourceState)
desrrsState = Lens.field @"state"
{-# INLINEABLE desrrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsResponseStatus :: Lens.Lens' DescribeEventSourceResponse Core.Int
desrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE desrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
