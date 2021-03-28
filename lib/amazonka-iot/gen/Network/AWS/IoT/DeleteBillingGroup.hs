{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the billing group.
module Network.AWS.IoT.DeleteBillingGroup
    (
    -- * Creating a request
      DeleteBillingGroup (..)
    , mkDeleteBillingGroup
    -- ** Request lenses
    , dbgBillingGroupName
    , dbgExpectedVersion

    -- * Destructuring the response
    , DeleteBillingGroupResponse (..)
    , mkDeleteBillingGroupResponse
    -- ** Response lenses
    , dbgrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBillingGroup' smart constructor.
data DeleteBillingGroup = DeleteBillingGroup'
  { billingGroupName :: Types.BillingGroupName
    -- ^ The name of the billing group.
  , expectedVersion :: Core.Maybe Core.Integer
    -- ^ The expected version of the billing group. If the version of the billing group does not match the expected version specified in the request, the @DeleteBillingGroup@ request is rejected with a @VersionConflictException@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBillingGroup' value with any optional fields omitted.
mkDeleteBillingGroup
    :: Types.BillingGroupName -- ^ 'billingGroupName'
    -> DeleteBillingGroup
mkDeleteBillingGroup billingGroupName
  = DeleteBillingGroup'{billingGroupName,
                        expectedVersion = Core.Nothing}

-- | The name of the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgBillingGroupName :: Lens.Lens' DeleteBillingGroup Types.BillingGroupName
dbgBillingGroupName = Lens.field @"billingGroupName"
{-# INLINEABLE dbgBillingGroupName #-}
{-# DEPRECATED billingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead"  #-}

-- | The expected version of the billing group. If the version of the billing group does not match the expected version specified in the request, the @DeleteBillingGroup@ request is rejected with a @VersionConflictException@ .
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgExpectedVersion :: Lens.Lens' DeleteBillingGroup (Core.Maybe Core.Integer)
dbgExpectedVersion = Lens.field @"expectedVersion"
{-# INLINEABLE dbgExpectedVersion #-}
{-# DEPRECATED expectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead"  #-}

instance Core.ToQuery DeleteBillingGroup where
        toQuery DeleteBillingGroup{..}
          = Core.maybe Core.mempty (Core.toQueryPair "expectedVersion")
              expectedVersion

instance Core.ToHeaders DeleteBillingGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteBillingGroup where
        type Rs DeleteBillingGroup = DeleteBillingGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/billing-groups/" Core.<> Core.toText billingGroupName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteBillingGroupResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteBillingGroupResponse' smart constructor.
newtype DeleteBillingGroupResponse = DeleteBillingGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBillingGroupResponse' value with any optional fields omitted.
mkDeleteBillingGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteBillingGroupResponse
mkDeleteBillingGroupResponse responseStatus
  = DeleteBillingGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgrrsResponseStatus :: Lens.Lens' DeleteBillingGroupResponse Core.Int
dbgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
