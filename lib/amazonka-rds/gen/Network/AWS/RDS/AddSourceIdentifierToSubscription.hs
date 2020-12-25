{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AddSourceIdentifierToSubscription (..),
    mkAddSourceIdentifierToSubscription,

    -- ** Request lenses
    asitsSubscriptionName,
    asitsSourceIdentifier,

    -- * Destructuring the response
    AddSourceIdentifierToSubscriptionResponse (..),
    mkAddSourceIdentifierToSubscriptionResponse,

    -- ** Response lenses
    asitsrrsEventSubscription,
    asitsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkAddSourceIdentifierToSubscription' smart constructor.
data AddSourceIdentifierToSubscription = AddSourceIdentifierToSubscription'
  { -- | The name of the RDS event notification subscription you want to add a source identifier to.
    subscriptionName :: Types.SubscriptionName,
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
    sourceIdentifier :: Types.SourceIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddSourceIdentifierToSubscription' value with any optional fields omitted.
mkAddSourceIdentifierToSubscription ::
  -- | 'subscriptionName'
  Types.SubscriptionName ->
  -- | 'sourceIdentifier'
  Types.SourceIdentifier ->
  AddSourceIdentifierToSubscription
mkAddSourceIdentifierToSubscription
  subscriptionName
  sourceIdentifier =
    AddSourceIdentifierToSubscription'
      { subscriptionName,
        sourceIdentifier
      }

-- | The name of the RDS event notification subscription you want to add a source identifier to.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asitsSubscriptionName :: Lens.Lens' AddSourceIdentifierToSubscription Types.SubscriptionName
asitsSubscriptionName = Lens.field @"subscriptionName"
{-# DEPRECATED asitsSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

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
asitsSourceIdentifier :: Lens.Lens' AddSourceIdentifierToSubscription Types.SourceIdentifier
asitsSourceIdentifier = Lens.field @"sourceIdentifier"
{-# DEPRECATED asitsSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

instance Core.AWSRequest AddSourceIdentifierToSubscription where
  type
    Rs AddSourceIdentifierToSubscription =
      AddSourceIdentifierToSubscriptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "AddSourceIdentifierToSubscription")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "SubscriptionName" subscriptionName)
                Core.<> (Core.toQueryValue "SourceIdentifier" sourceIdentifier)
            )
      }
  response =
    Response.receiveXMLWrapper
      "AddSourceIdentifierToSubscriptionResult"
      ( \s h x ->
          AddSourceIdentifierToSubscriptionResponse'
            Core.<$> (x Core..@? "EventSubscription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAddSourceIdentifierToSubscriptionResponse' smart constructor.
data AddSourceIdentifierToSubscriptionResponse = AddSourceIdentifierToSubscriptionResponse'
  { eventSubscription :: Core.Maybe Types.EventSubscription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddSourceIdentifierToSubscriptionResponse' value with any optional fields omitted.
mkAddSourceIdentifierToSubscriptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AddSourceIdentifierToSubscriptionResponse
mkAddSourceIdentifierToSubscriptionResponse responseStatus =
  AddSourceIdentifierToSubscriptionResponse'
    { eventSubscription =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asitsrrsEventSubscription :: Lens.Lens' AddSourceIdentifierToSubscriptionResponse (Core.Maybe Types.EventSubscription)
asitsrrsEventSubscription = Lens.field @"eventSubscription"
{-# DEPRECATED asitsrrsEventSubscription "Use generic-lens or generic-optics with 'eventSubscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asitsrrsResponseStatus :: Lens.Lens' AddSourceIdentifierToSubscriptionResponse Core.Int
asitsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED asitsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
