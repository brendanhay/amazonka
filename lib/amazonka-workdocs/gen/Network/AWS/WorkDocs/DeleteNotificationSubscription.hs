{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteNotificationSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified subscription from the specified organization.
module Network.AWS.WorkDocs.DeleteNotificationSubscription
    (
    -- * Creating a request
      DeleteNotificationSubscription (..)
    , mkDeleteNotificationSubscription
    -- ** Request lenses
    , dnsSubscriptionId
    , dnsOrganizationId

    -- * Destructuring the response
    , DeleteNotificationSubscriptionResponse (..)
    , mkDeleteNotificationSubscriptionResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDeleteNotificationSubscription' smart constructor.
data DeleteNotificationSubscription = DeleteNotificationSubscription'
  { subscriptionId :: Types.IdType
    -- ^ The ID of the subscription.
  , organizationId :: Types.IdType
    -- ^ The ID of the organization.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNotificationSubscription' value with any optional fields omitted.
mkDeleteNotificationSubscription
    :: Types.IdType -- ^ 'subscriptionId'
    -> Types.IdType -- ^ 'organizationId'
    -> DeleteNotificationSubscription
mkDeleteNotificationSubscription subscriptionId organizationId
  = DeleteNotificationSubscription'{subscriptionId, organizationId}

-- | The ID of the subscription.
--
-- /Note:/ Consider using 'subscriptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsSubscriptionId :: Lens.Lens' DeleteNotificationSubscription Types.IdType
dnsSubscriptionId = Lens.field @"subscriptionId"
{-# INLINEABLE dnsSubscriptionId #-}
{-# DEPRECATED subscriptionId "Use generic-lens or generic-optics with 'subscriptionId' instead"  #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsOrganizationId :: Lens.Lens' DeleteNotificationSubscription Types.IdType
dnsOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE dnsOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

instance Core.ToQuery DeleteNotificationSubscription where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteNotificationSubscription where
        toHeaders DeleteNotificationSubscription{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteNotificationSubscription where
        type Rs DeleteNotificationSubscription =
             DeleteNotificationSubscriptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/api/v1/organizations/" Core.<> Core.toText organizationId Core.<>
                             "/subscriptions/"
                             Core.<> Core.toText subscriptionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteNotificationSubscriptionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteNotificationSubscriptionResponse' smart constructor.
data DeleteNotificationSubscriptionResponse = DeleteNotificationSubscriptionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNotificationSubscriptionResponse' value with any optional fields omitted.
mkDeleteNotificationSubscriptionResponse
    :: DeleteNotificationSubscriptionResponse
mkDeleteNotificationSubscriptionResponse
  = DeleteNotificationSubscriptionResponse'
