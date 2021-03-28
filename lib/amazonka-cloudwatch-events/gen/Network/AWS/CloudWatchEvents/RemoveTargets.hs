{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.RemoveTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified targets from the specified rule. When the rule is triggered, those targets are no longer be invoked.
--
-- When you remove a target, when the associated rule triggers, removed targets might continue to be invoked. Allow a short period of time for changes to take effect.
-- This action can partially fail if too many requests are made at the same time. If that happens, @FailedEntryCount@ is non-zero in the response and each entry in @FailedEntries@ provides the ID of the failed target and the error code.
module Network.AWS.CloudWatchEvents.RemoveTargets
    (
    -- * Creating a request
      RemoveTargets (..)
    , mkRemoveTargets
    -- ** Request lenses
    , rtRule
    , rtIds
    , rtEventBusName
    , rtForce

    -- * Destructuring the response
    , RemoveTargetsResponse (..)
    , mkRemoveTargetsResponse
    -- ** Response lenses
    , rtrrsFailedEntries
    , rtrrsFailedEntryCount
    , rtrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveTargets' smart constructor.
data RemoveTargets = RemoveTargets'
  { rule :: Types.RuleName
    -- ^ The name of the rule.
  , ids :: Core.NonEmpty Types.TargetId
    -- ^ The IDs of the targets to remove from the rule.
  , eventBusName :: Core.Maybe Types.EventBusNameOrArn
    -- ^ The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
  , force :: Core.Maybe Core.Bool
    -- ^ If this is a managed rule, created by an AWS service on your behalf, you must specify @Force@ as @True@ to remove targets. This parameter is ignored for rules that are not managed rules. You can check whether a rule is a managed rule by using @DescribeRule@ or @ListRules@ and checking the @ManagedBy@ field of the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTargets' value with any optional fields omitted.
mkRemoveTargets
    :: Types.RuleName -- ^ 'rule'
    -> Core.NonEmpty Types.TargetId -- ^ 'ids'
    -> RemoveTargets
mkRemoveTargets rule ids
  = RemoveTargets'{rule, ids, eventBusName = Core.Nothing,
                   force = Core.Nothing}

-- | The name of the rule.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtRule :: Lens.Lens' RemoveTargets Types.RuleName
rtRule = Lens.field @"rule"
{-# INLINEABLE rtRule #-}
{-# DEPRECATED rule "Use generic-lens or generic-optics with 'rule' instead"  #-}

-- | The IDs of the targets to remove from the rule.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtIds :: Lens.Lens' RemoveTargets (Core.NonEmpty Types.TargetId)
rtIds = Lens.field @"ids"
{-# INLINEABLE rtIds #-}
{-# DEPRECATED ids "Use generic-lens or generic-optics with 'ids' instead"  #-}

-- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtEventBusName :: Lens.Lens' RemoveTargets (Core.Maybe Types.EventBusNameOrArn)
rtEventBusName = Lens.field @"eventBusName"
{-# INLINEABLE rtEventBusName #-}
{-# DEPRECATED eventBusName "Use generic-lens or generic-optics with 'eventBusName' instead"  #-}

-- | If this is a managed rule, created by an AWS service on your behalf, you must specify @Force@ as @True@ to remove targets. This parameter is ignored for rules that are not managed rules. You can check whether a rule is a managed rule by using @DescribeRule@ or @ListRules@ and checking the @ManagedBy@ field of the response.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtForce :: Lens.Lens' RemoveTargets (Core.Maybe Core.Bool)
rtForce = Lens.field @"force"
{-# INLINEABLE rtForce #-}
{-# DEPRECATED force "Use generic-lens or generic-optics with 'force' instead"  #-}

instance Core.ToQuery RemoveTargets where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RemoveTargets where
        toHeaders RemoveTargets{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.RemoveTargets") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RemoveTargets where
        toJSON RemoveTargets{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Rule" Core..= rule), Core.Just ("Ids" Core..= ids),
                  ("EventBusName" Core..=) Core.<$> eventBusName,
                  ("Force" Core..=) Core.<$> force])

instance Core.AWSRequest RemoveTargets where
        type Rs RemoveTargets = RemoveTargetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RemoveTargetsResponse' Core.<$>
                   (x Core..:? "FailedEntries") Core.<*> x Core..:? "FailedEntryCount"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemoveTargetsResponse' smart constructor.
data RemoveTargetsResponse = RemoveTargetsResponse'
  { failedEntries :: Core.Maybe [Types.RemoveTargetsResultEntry]
    -- ^ The failed target entries.
  , failedEntryCount :: Core.Maybe Core.Int
    -- ^ The number of failed entries.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTargetsResponse' value with any optional fields omitted.
mkRemoveTargetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RemoveTargetsResponse
mkRemoveTargetsResponse responseStatus
  = RemoveTargetsResponse'{failedEntries = Core.Nothing,
                           failedEntryCount = Core.Nothing, responseStatus}

-- | The failed target entries.
--
-- /Note:/ Consider using 'failedEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsFailedEntries :: Lens.Lens' RemoveTargetsResponse (Core.Maybe [Types.RemoveTargetsResultEntry])
rtrrsFailedEntries = Lens.field @"failedEntries"
{-# INLINEABLE rtrrsFailedEntries #-}
{-# DEPRECATED failedEntries "Use generic-lens or generic-optics with 'failedEntries' instead"  #-}

-- | The number of failed entries.
--
-- /Note:/ Consider using 'failedEntryCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsFailedEntryCount :: Lens.Lens' RemoveTargetsResponse (Core.Maybe Core.Int)
rtrrsFailedEntryCount = Lens.field @"failedEntryCount"
{-# INLINEABLE rtrrsFailedEntryCount #-}
{-# DEPRECATED failedEntryCount "Use generic-lens or generic-optics with 'failedEntryCount' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsResponseStatus :: Lens.Lens' RemoveTargetsResponse Core.Int
rtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
