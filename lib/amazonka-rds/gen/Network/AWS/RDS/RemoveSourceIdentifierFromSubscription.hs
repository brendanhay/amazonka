{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a source identifier from an existing RDS event notification subscription.
module Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
  ( -- * Creating a request
    RemoveSourceIdentifierFromSubscription (..),
    mkRemoveSourceIdentifierFromSubscription,

    -- ** Request lenses
    rsifsSubscriptionName,
    rsifsSourceIdentifier,

    -- * Destructuring the response
    RemoveSourceIdentifierFromSubscriptionResponse (..),
    mkRemoveSourceIdentifierFromSubscriptionResponse,

    -- ** Response lenses
    rsifsrrsEventSubscription,
    rsifsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkRemoveSourceIdentifierFromSubscription' smart constructor.
data RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscription'
  { -- | The name of the RDS event notification subscription you want to remove a source identifier from.
    subscriptionName :: Types.String,
    -- | The source identifier to be removed from the subscription, such as the __DB instance identifier__ for a DB instance or the name of a security group.
    sourceIdentifier :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveSourceIdentifierFromSubscription' value with any optional fields omitted.
mkRemoveSourceIdentifierFromSubscription ::
  -- | 'subscriptionName'
  Types.String ->
  -- | 'sourceIdentifier'
  Types.String ->
  RemoveSourceIdentifierFromSubscription
mkRemoveSourceIdentifierFromSubscription
  subscriptionName
  sourceIdentifier =
    RemoveSourceIdentifierFromSubscription'
      { subscriptionName,
        sourceIdentifier
      }

-- | The name of the RDS event notification subscription you want to remove a source identifier from.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsifsSubscriptionName :: Lens.Lens' RemoveSourceIdentifierFromSubscription Types.String
rsifsSubscriptionName = Lens.field @"subscriptionName"
{-# DEPRECATED rsifsSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

-- | The source identifier to be removed from the subscription, such as the __DB instance identifier__ for a DB instance or the name of a security group.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsifsSourceIdentifier :: Lens.Lens' RemoveSourceIdentifierFromSubscription Types.String
rsifsSourceIdentifier = Lens.field @"sourceIdentifier"
{-# DEPRECATED rsifsSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

instance Core.AWSRequest RemoveSourceIdentifierFromSubscription where
  type
    Rs RemoveSourceIdentifierFromSubscription =
      RemoveSourceIdentifierFromSubscriptionResponse
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
            ( Core.pure ("Action", "RemoveSourceIdentifierFromSubscription")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "SubscriptionName" subscriptionName)
                Core.<> (Core.toQueryValue "SourceIdentifier" sourceIdentifier)
            )
      }
  response =
    Response.receiveXMLWrapper
      "RemoveSourceIdentifierFromSubscriptionResult"
      ( \s h x ->
          RemoveSourceIdentifierFromSubscriptionResponse'
            Core.<$> (x Core..@? "EventSubscription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRemoveSourceIdentifierFromSubscriptionResponse' smart constructor.
data RemoveSourceIdentifierFromSubscriptionResponse = RemoveSourceIdentifierFromSubscriptionResponse'
  { eventSubscription :: Core.Maybe Types.EventSubscription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveSourceIdentifierFromSubscriptionResponse' value with any optional fields omitted.
mkRemoveSourceIdentifierFromSubscriptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RemoveSourceIdentifierFromSubscriptionResponse
mkRemoveSourceIdentifierFromSubscriptionResponse responseStatus =
  RemoveSourceIdentifierFromSubscriptionResponse'
    { eventSubscription =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsifsrrsEventSubscription :: Lens.Lens' RemoveSourceIdentifierFromSubscriptionResponse (Core.Maybe Types.EventSubscription)
rsifsrrsEventSubscription = Lens.field @"eventSubscription"
{-# DEPRECATED rsifsrrsEventSubscription "Use generic-lens or generic-optics with 'eventSubscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsifsrrsResponseStatus :: Lens.Lens' RemoveSourceIdentifierFromSubscriptionResponse Core.Int
rsifsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rsifsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
