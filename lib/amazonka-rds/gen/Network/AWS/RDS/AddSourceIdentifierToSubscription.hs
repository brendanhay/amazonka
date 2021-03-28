{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.AddSourceIdentifierToSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a source identifier to an existing RDS event notification subscription.
module Network.AWS.RDS.AddSourceIdentifierToSubscription
    (
    -- * Creating a request
      AddSourceIdentifierToSubscription (..)
    , mkAddSourceIdentifierToSubscription
    -- ** Request lenses
    , asitsSubscriptionName
    , asitsSourceIdentifier

    -- * Destructuring the response
    , AddSourceIdentifierToSubscriptionResponse (..)
    , mkAddSourceIdentifierToSubscriptionResponse
    -- ** Response lenses
    , asitsrrsEventSubscription
    , asitsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkAddSourceIdentifierToSubscription' smart constructor.
data AddSourceIdentifierToSubscription = AddSourceIdentifierToSubscription'
  { subscriptionName :: Core.Text
    -- ^ The name of the RDS event notification subscription you want to add a source identifier to.
  , sourceIdentifier :: Core.Text
    -- ^ The identifier of the event source to be added.
--
-- Constraints:
--
--     * If the source type is a DB instance, a @DBInstanceIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB cluster, a @DBClusterIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB parameter group, a @DBParameterGroupName@ value must be supplied.
--
--
--     * If the source type is a DB security group, a @DBSecurityGroupName@ value must be supplied.
--
--
--     * If the source type is a DB snapshot, a @DBSnapshotIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB cluster snapshot, a @DBClusterSnapshotIdentifier@ value must be supplied.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddSourceIdentifierToSubscription' value with any optional fields omitted.
mkAddSourceIdentifierToSubscription
    :: Core.Text -- ^ 'subscriptionName'
    -> Core.Text -- ^ 'sourceIdentifier'
    -> AddSourceIdentifierToSubscription
mkAddSourceIdentifierToSubscription subscriptionName
  sourceIdentifier
  = AddSourceIdentifierToSubscription'{subscriptionName,
                                       sourceIdentifier}

-- | The name of the RDS event notification subscription you want to add a source identifier to.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asitsSubscriptionName :: Lens.Lens' AddSourceIdentifierToSubscription Core.Text
asitsSubscriptionName = Lens.field @"subscriptionName"
{-# INLINEABLE asitsSubscriptionName #-}
{-# DEPRECATED subscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead"  #-}

-- | The identifier of the event source to be added.
--
-- Constraints:
--
--     * If the source type is a DB instance, a @DBInstanceIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB cluster, a @DBClusterIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB parameter group, a @DBParameterGroupName@ value must be supplied.
--
--
--     * If the source type is a DB security group, a @DBSecurityGroupName@ value must be supplied.
--
--
--     * If the source type is a DB snapshot, a @DBSnapshotIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB cluster snapshot, a @DBClusterSnapshotIdentifier@ value must be supplied.
--
--
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asitsSourceIdentifier :: Lens.Lens' AddSourceIdentifierToSubscription Core.Text
asitsSourceIdentifier = Lens.field @"sourceIdentifier"
{-# INLINEABLE asitsSourceIdentifier #-}
{-# DEPRECATED sourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead"  #-}

instance Core.ToQuery AddSourceIdentifierToSubscription where
        toQuery AddSourceIdentifierToSubscription{..}
          = Core.toQueryPair "Action"
              ("AddSourceIdentifierToSubscription" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "SubscriptionName" subscriptionName
              Core.<> Core.toQueryPair "SourceIdentifier" sourceIdentifier

instance Core.ToHeaders AddSourceIdentifierToSubscription where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AddSourceIdentifierToSubscription where
        type Rs AddSourceIdentifierToSubscription =
             AddSourceIdentifierToSubscriptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper
              "AddSourceIdentifierToSubscriptionResult"
              (\ s h x ->
                 AddSourceIdentifierToSubscriptionResponse' Core.<$>
                   (x Core..@? "EventSubscription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAddSourceIdentifierToSubscriptionResponse' smart constructor.
data AddSourceIdentifierToSubscriptionResponse = AddSourceIdentifierToSubscriptionResponse'
  { eventSubscription :: Core.Maybe Types.EventSubscription
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddSourceIdentifierToSubscriptionResponse' value with any optional fields omitted.
mkAddSourceIdentifierToSubscriptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddSourceIdentifierToSubscriptionResponse
mkAddSourceIdentifierToSubscriptionResponse responseStatus
  = AddSourceIdentifierToSubscriptionResponse'{eventSubscription =
                                                 Core.Nothing,
                                               responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asitsrrsEventSubscription :: Lens.Lens' AddSourceIdentifierToSubscriptionResponse (Core.Maybe Types.EventSubscription)
asitsrrsEventSubscription = Lens.field @"eventSubscription"
{-# INLINEABLE asitsrrsEventSubscription #-}
{-# DEPRECATED eventSubscription "Use generic-lens or generic-optics with 'eventSubscription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asitsrrsResponseStatus :: Lens.Lens' AddSourceIdentifierToSubscriptionResponse Core.Int
asitsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE asitsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
