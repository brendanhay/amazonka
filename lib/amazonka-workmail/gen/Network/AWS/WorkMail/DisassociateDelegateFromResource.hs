{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DisassociateDelegateFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a member from the resource's set of delegates.
module Network.AWS.WorkMail.DisassociateDelegateFromResource
    (
    -- * Creating a request
      DisassociateDelegateFromResource (..)
    , mkDisassociateDelegateFromResource
    -- ** Request lenses
    , ddfrOrganizationId
    , ddfrResourceId
    , ddfrEntityId

    -- * Destructuring the response
    , DisassociateDelegateFromResourceResponse (..)
    , mkDisassociateDelegateFromResourceResponse
    -- ** Response lenses
    , ddfrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDisassociateDelegateFromResource' smart constructor.
data DisassociateDelegateFromResource = DisassociateDelegateFromResource'
  { organizationId :: Types.OrganizationId
    -- ^ The identifier for the organization under which the resource exists.
  , resourceId :: Types.ResourceId
    -- ^ The identifier of the resource from which delegates' set members are removed. 
  , entityId :: Types.EntityId
    -- ^ The identifier for the member (user, group) to be removed from the resource's delegates.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateDelegateFromResource' value with any optional fields omitted.
mkDisassociateDelegateFromResource
    :: Types.OrganizationId -- ^ 'organizationId'
    -> Types.ResourceId -- ^ 'resourceId'
    -> Types.EntityId -- ^ 'entityId'
    -> DisassociateDelegateFromResource
mkDisassociateDelegateFromResource organizationId resourceId
  entityId
  = DisassociateDelegateFromResource'{organizationId, resourceId,
                                      entityId}

-- | The identifier for the organization under which the resource exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfrOrganizationId :: Lens.Lens' DisassociateDelegateFromResource Types.OrganizationId
ddfrOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE ddfrOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The identifier of the resource from which delegates' set members are removed. 
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfrResourceId :: Lens.Lens' DisassociateDelegateFromResource Types.ResourceId
ddfrResourceId = Lens.field @"resourceId"
{-# INLINEABLE ddfrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The identifier for the member (user, group) to be removed from the resource's delegates.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfrEntityId :: Lens.Lens' DisassociateDelegateFromResource Types.EntityId
ddfrEntityId = Lens.field @"entityId"
{-# INLINEABLE ddfrEntityId #-}
{-# DEPRECATED entityId "Use generic-lens or generic-optics with 'entityId' instead"  #-}

instance Core.ToQuery DisassociateDelegateFromResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateDelegateFromResource where
        toHeaders DisassociateDelegateFromResource{..}
          = Core.pure
              ("X-Amz-Target",
               "WorkMailService.DisassociateDelegateFromResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateDelegateFromResource where
        toJSON DisassociateDelegateFromResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("ResourceId" Core..= resourceId),
                  Core.Just ("EntityId" Core..= entityId)])

instance Core.AWSRequest DisassociateDelegateFromResource where
        type Rs DisassociateDelegateFromResource =
             DisassociateDelegateFromResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateDelegateFromResourceResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateDelegateFromResourceResponse' smart constructor.
newtype DisassociateDelegateFromResourceResponse = DisassociateDelegateFromResourceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateDelegateFromResourceResponse' value with any optional fields omitted.
mkDisassociateDelegateFromResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateDelegateFromResourceResponse
mkDisassociateDelegateFromResourceResponse responseStatus
  = DisassociateDelegateFromResourceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfrrrsResponseStatus :: Lens.Lens' DisassociateDelegateFromResourceResponse Core.Int
ddfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
