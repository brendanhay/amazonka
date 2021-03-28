{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.ModifyEventSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing AWS DMS event notification subscription. 
module Network.AWS.DMS.ModifyEventSubscription
    (
    -- * Creating a request
      ModifyEventSubscription (..)
    , mkModifyEventSubscription
    -- ** Request lenses
    , mesSubscriptionName
    , mesEnabled
    , mesEventCategories
    , mesSnsTopicArn
    , mesSourceType

    -- * Destructuring the response
    , ModifyEventSubscriptionResponse (..)
    , mkModifyEventSubscriptionResponse
    -- ** Response lenses
    , mesrrsEventSubscription
    , mesrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkModifyEventSubscription' smart constructor.
data ModifyEventSubscription = ModifyEventSubscription'
  { subscriptionName :: Core.Text
    -- ^ The name of the AWS DMS event notification subscription to be modified.
  , enabled :: Core.Maybe Core.Bool
    -- ^ A Boolean value; set to __true__ to activate the subscription. 
  , eventCategories :: Core.Maybe [Core.Text]
    -- ^ A list of event categories for a source type that you want to subscribe to. Use the @DescribeEventCategories@ action to see a list of event categories. 
  , snsTopicArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the Amazon SNS topic created for event notification. The ARN is created by Amazon SNS when you create a topic and subscribe to it.
  , sourceType :: Core.Maybe Core.Text
    -- ^ The type of AWS DMS resource that generates the events you want to subscribe to. 
--
-- Valid values: replication-instance | replication-task
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyEventSubscription' value with any optional fields omitted.
mkModifyEventSubscription
    :: Core.Text -- ^ 'subscriptionName'
    -> ModifyEventSubscription
mkModifyEventSubscription subscriptionName
  = ModifyEventSubscription'{subscriptionName,
                             enabled = Core.Nothing, eventCategories = Core.Nothing,
                             snsTopicArn = Core.Nothing, sourceType = Core.Nothing}

-- | The name of the AWS DMS event notification subscription to be modified.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesSubscriptionName :: Lens.Lens' ModifyEventSubscription Core.Text
mesSubscriptionName = Lens.field @"subscriptionName"
{-# INLINEABLE mesSubscriptionName #-}
{-# DEPRECATED subscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead"  #-}

-- | A Boolean value; set to __true__ to activate the subscription. 
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesEnabled :: Lens.Lens' ModifyEventSubscription (Core.Maybe Core.Bool)
mesEnabled = Lens.field @"enabled"
{-# INLINEABLE mesEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | A list of event categories for a source type that you want to subscribe to. Use the @DescribeEventCategories@ action to see a list of event categories. 
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesEventCategories :: Lens.Lens' ModifyEventSubscription (Core.Maybe [Core.Text])
mesEventCategories = Lens.field @"eventCategories"
{-# INLINEABLE mesEventCategories #-}
{-# DEPRECATED eventCategories "Use generic-lens or generic-optics with 'eventCategories' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic created for event notification. The ARN is created by Amazon SNS when you create a topic and subscribe to it.
--
-- /Note:/ Consider using 'snsTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesSnsTopicArn :: Lens.Lens' ModifyEventSubscription (Core.Maybe Core.Text)
mesSnsTopicArn = Lens.field @"snsTopicArn"
{-# INLINEABLE mesSnsTopicArn #-}
{-# DEPRECATED snsTopicArn "Use generic-lens or generic-optics with 'snsTopicArn' instead"  #-}

-- | The type of AWS DMS resource that generates the events you want to subscribe to. 
--
-- Valid values: replication-instance | replication-task
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesSourceType :: Lens.Lens' ModifyEventSubscription (Core.Maybe Core.Text)
mesSourceType = Lens.field @"sourceType"
{-# INLINEABLE mesSourceType #-}
{-# DEPRECATED sourceType "Use generic-lens or generic-optics with 'sourceType' instead"  #-}

instance Core.ToQuery ModifyEventSubscription where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ModifyEventSubscription where
        toHeaders ModifyEventSubscription{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.ModifyEventSubscription")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ModifyEventSubscription where
        toJSON ModifyEventSubscription{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SubscriptionName" Core..= subscriptionName),
                  ("Enabled" Core..=) Core.<$> enabled,
                  ("EventCategories" Core..=) Core.<$> eventCategories,
                  ("SnsTopicArn" Core..=) Core.<$> snsTopicArn,
                  ("SourceType" Core..=) Core.<$> sourceType])

instance Core.AWSRequest ModifyEventSubscription where
        type Rs ModifyEventSubscription = ModifyEventSubscriptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ModifyEventSubscriptionResponse' Core.<$>
                   (x Core..:? "EventSubscription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkModifyEventSubscriptionResponse' smart constructor.
data ModifyEventSubscriptionResponse = ModifyEventSubscriptionResponse'
  { eventSubscription :: Core.Maybe Types.EventSubscription
    -- ^ The modified event subscription.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyEventSubscriptionResponse' value with any optional fields omitted.
mkModifyEventSubscriptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyEventSubscriptionResponse
mkModifyEventSubscriptionResponse responseStatus
  = ModifyEventSubscriptionResponse'{eventSubscription =
                                       Core.Nothing,
                                     responseStatus}

-- | The modified event subscription.
--
-- /Note:/ Consider using 'eventSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesrrsEventSubscription :: Lens.Lens' ModifyEventSubscriptionResponse (Core.Maybe Types.EventSubscription)
mesrrsEventSubscription = Lens.field @"eventSubscription"
{-# INLINEABLE mesrrsEventSubscription #-}
{-# DEPRECATED eventSubscription "Use generic-lens or generic-optics with 'eventSubscription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesrrsResponseStatus :: Lens.Lens' ModifyEventSubscriptionResponse Core.Int
mesrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mesrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
