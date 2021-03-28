{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetServiceLinkedRoleDeletionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the status of your service-linked role deletion. After you use the 'DeleteServiceLinkedRole' API operation to submit a service-linked role for deletion, you can use the @DeletionTaskId@ parameter in @GetServiceLinkedRoleDeletionStatus@ to check the status of the deletion. If the deletion fails, this operation returns the reason that it failed, if that information is returned by the service.
module Network.AWS.IAM.GetServiceLinkedRoleDeletionStatus
    (
    -- * Creating a request
      GetServiceLinkedRoleDeletionStatus (..)
    , mkGetServiceLinkedRoleDeletionStatus
    -- ** Request lenses
    , gslrdsDeletionTaskId

    -- * Destructuring the response
    , GetServiceLinkedRoleDeletionStatusResponse (..)
    , mkGetServiceLinkedRoleDeletionStatusResponse
    -- ** Response lenses
    , gslrdsrrsStatus
    , gslrdsrrsReason
    , gslrdsrrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetServiceLinkedRoleDeletionStatus' smart constructor.
newtype GetServiceLinkedRoleDeletionStatus = GetServiceLinkedRoleDeletionStatus'
  { deletionTaskId :: Types.DeletionTaskId
    -- ^ The deletion task identifier. This identifier is returned by the 'DeleteServiceLinkedRole' operation in the format @task/aws-service-role/<service-principal-name>/<role-name>/<task-uuid>@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetServiceLinkedRoleDeletionStatus' value with any optional fields omitted.
mkGetServiceLinkedRoleDeletionStatus
    :: Types.DeletionTaskId -- ^ 'deletionTaskId'
    -> GetServiceLinkedRoleDeletionStatus
mkGetServiceLinkedRoleDeletionStatus deletionTaskId
  = GetServiceLinkedRoleDeletionStatus'{deletionTaskId}

-- | The deletion task identifier. This identifier is returned by the 'DeleteServiceLinkedRole' operation in the format @task/aws-service-role/<service-principal-name>/<role-name>/<task-uuid>@ .
--
-- /Note:/ Consider using 'deletionTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslrdsDeletionTaskId :: Lens.Lens' GetServiceLinkedRoleDeletionStatus Types.DeletionTaskId
gslrdsDeletionTaskId = Lens.field @"deletionTaskId"
{-# INLINEABLE gslrdsDeletionTaskId #-}
{-# DEPRECATED deletionTaskId "Use generic-lens or generic-optics with 'deletionTaskId' instead"  #-}

instance Core.ToQuery GetServiceLinkedRoleDeletionStatus where
        toQuery GetServiceLinkedRoleDeletionStatus{..}
          = Core.toQueryPair "Action"
              ("GetServiceLinkedRoleDeletionStatus" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "DeletionTaskId" deletionTaskId

instance Core.ToHeaders GetServiceLinkedRoleDeletionStatus where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetServiceLinkedRoleDeletionStatus where
        type Rs GetServiceLinkedRoleDeletionStatus =
             GetServiceLinkedRoleDeletionStatusResponse
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
              "GetServiceLinkedRoleDeletionStatusResult"
              (\ s h x ->
                 GetServiceLinkedRoleDeletionStatusResponse' Core.<$>
                   (x Core..@ "Status") Core.<*> x Core..@? "Reason" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetServiceLinkedRoleDeletionStatusResponse' smart constructor.
data GetServiceLinkedRoleDeletionStatusResponse = GetServiceLinkedRoleDeletionStatusResponse'
  { status :: Types.DeletionTaskStatusType
    -- ^ The status of the deletion.
  , reason :: Core.Maybe Types.DeletionTaskFailureReasonType
    -- ^ An object that contains details about the reason the deletion failed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetServiceLinkedRoleDeletionStatusResponse' value with any optional fields omitted.
mkGetServiceLinkedRoleDeletionStatusResponse
    :: Types.DeletionTaskStatusType -- ^ 'status'
    -> Core.Int -- ^ 'responseStatus'
    -> GetServiceLinkedRoleDeletionStatusResponse
mkGetServiceLinkedRoleDeletionStatusResponse status responseStatus
  = GetServiceLinkedRoleDeletionStatusResponse'{status,
                                                reason = Core.Nothing, responseStatus}

-- | The status of the deletion.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslrdsrrsStatus :: Lens.Lens' GetServiceLinkedRoleDeletionStatusResponse Types.DeletionTaskStatusType
gslrdsrrsStatus = Lens.field @"status"
{-# INLINEABLE gslrdsrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | An object that contains details about the reason the deletion failed.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslrdsrrsReason :: Lens.Lens' GetServiceLinkedRoleDeletionStatusResponse (Core.Maybe Types.DeletionTaskFailureReasonType)
gslrdsrrsReason = Lens.field @"reason"
{-# INLINEABLE gslrdsrrsReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslrdsrrsResponseStatus :: Lens.Lens' GetServiceLinkedRoleDeletionStatusResponse Core.Int
gslrdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gslrdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
