{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeEntityAggregates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of entities that are affected by each of the specified events. If no events are specified, the counts of all affected entities are returned.
module Network.AWS.AWSHealth.DescribeEntityAggregates
    (
    -- * Creating a request
      DescribeEntityAggregates (..)
    , mkDescribeEntityAggregates
    -- ** Request lenses
    , deaEventArns

    -- * Destructuring the response
    , DescribeEntityAggregatesResponse (..)
    , mkDescribeEntityAggregatesResponse
    -- ** Response lenses
    , dearrsEntityAggregates
    , dearrsResponseStatus
    ) where

import qualified Network.AWS.AWSHealth.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEntityAggregates' smart constructor.
newtype DescribeEntityAggregates = DescribeEntityAggregates'
  { eventArns :: Core.Maybe (Core.NonEmpty Types.EventArn)
    -- ^ A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEntityAggregates' value with any optional fields omitted.
mkDescribeEntityAggregates
    :: DescribeEntityAggregates
mkDescribeEntityAggregates
  = DescribeEntityAggregates'{eventArns = Core.Nothing}

-- | A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@ 
--
-- /Note:/ Consider using 'eventArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deaEventArns :: Lens.Lens' DescribeEntityAggregates (Core.Maybe (Core.NonEmpty Types.EventArn))
deaEventArns = Lens.field @"eventArns"
{-# INLINEABLE deaEventArns #-}
{-# DEPRECATED eventArns "Use generic-lens or generic-optics with 'eventArns' instead"  #-}

instance Core.ToQuery DescribeEntityAggregates where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEntityAggregates where
        toHeaders DescribeEntityAggregates{..}
          = Core.pure
              ("X-Amz-Target", "AWSHealth_20160804.DescribeEntityAggregates")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEntityAggregates where
        toJSON DescribeEntityAggregates{..}
          = Core.object
              (Core.catMaybes [("eventArns" Core..=) Core.<$> eventArns])

instance Core.AWSRequest DescribeEntityAggregates where
        type Rs DescribeEntityAggregates = DescribeEntityAggregatesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEntityAggregatesResponse' Core.<$>
                   (x Core..:? "entityAggregates") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeEntityAggregatesResponse' smart constructor.
data DescribeEntityAggregatesResponse = DescribeEntityAggregatesResponse'
  { entityAggregates :: Core.Maybe [Types.EntityAggregate]
    -- ^ The number of entities that are affected by each of the specified events.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEntityAggregatesResponse' value with any optional fields omitted.
mkDescribeEntityAggregatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEntityAggregatesResponse
mkDescribeEntityAggregatesResponse responseStatus
  = DescribeEntityAggregatesResponse'{entityAggregates =
                                        Core.Nothing,
                                      responseStatus}

-- | The number of entities that are affected by each of the specified events.
--
-- /Note:/ Consider using 'entityAggregates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dearrsEntityAggregates :: Lens.Lens' DescribeEntityAggregatesResponse (Core.Maybe [Types.EntityAggregate])
dearrsEntityAggregates = Lens.field @"entityAggregates"
{-# INLINEABLE dearrsEntityAggregates #-}
{-# DEPRECATED entityAggregates "Use generic-lens or generic-optics with 'entityAggregates' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dearrsResponseStatus :: Lens.Lens' DescribeEntityAggregatesResponse Core.Int
dearrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dearrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
