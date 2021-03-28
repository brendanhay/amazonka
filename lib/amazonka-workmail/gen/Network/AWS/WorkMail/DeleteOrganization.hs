{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DeleteOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon WorkMail organization and all underlying AWS resources managed by Amazon WorkMail as part of the organization. You can choose whether to delete the associated directory. For more information, see <https://docs.aws.amazon.com/workmail/latest/adminguide/remove_organization.html Removing an organization> in the /Amazon WorkMail Administrator Guide/ .
module Network.AWS.WorkMail.DeleteOrganization
    (
    -- * Creating a request
      DeleteOrganization (..)
    , mkDeleteOrganization
    -- ** Request lenses
    , doOrganizationId
    , doDeleteDirectory
    , doClientToken

    -- * Destructuring the response
    , DeleteOrganizationResponse (..)
    , mkDeleteOrganizationResponse
    -- ** Response lenses
    , dorfrsOrganizationId
    , dorfrsState
    , dorfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDeleteOrganization' smart constructor.
data DeleteOrganization = DeleteOrganization'
  { organizationId :: Types.OrganizationId
    -- ^ The organization ID.
  , deleteDirectory :: Core.Bool
    -- ^ If true, deletes the AWS Directory Service directory associated with the organization.
  , clientToken :: Core.Maybe Types.IdempotencyClientToken
    -- ^ The idempotency token associated with the request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOrganization' value with any optional fields omitted.
mkDeleteOrganization
    :: Types.OrganizationId -- ^ 'organizationId'
    -> Core.Bool -- ^ 'deleteDirectory'
    -> DeleteOrganization
mkDeleteOrganization organizationId deleteDirectory
  = DeleteOrganization'{organizationId, deleteDirectory,
                        clientToken = Core.Nothing}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doOrganizationId :: Lens.Lens' DeleteOrganization Types.OrganizationId
doOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE doOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | If true, deletes the AWS Directory Service directory associated with the organization.
--
-- /Note:/ Consider using 'deleteDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doDeleteDirectory :: Lens.Lens' DeleteOrganization Core.Bool
doDeleteDirectory = Lens.field @"deleteDirectory"
{-# INLINEABLE doDeleteDirectory #-}
{-# DEPRECATED deleteDirectory "Use generic-lens or generic-optics with 'deleteDirectory' instead"  #-}

-- | The idempotency token associated with the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doClientToken :: Lens.Lens' DeleteOrganization (Core.Maybe Types.IdempotencyClientToken)
doClientToken = Lens.field @"clientToken"
{-# INLINEABLE doClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

instance Core.ToQuery DeleteOrganization where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteOrganization where
        toHeaders DeleteOrganization{..}
          = Core.pure ("X-Amz-Target", "WorkMailService.DeleteOrganization")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteOrganization where
        toJSON DeleteOrganization{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("DeleteDirectory" Core..= deleteDirectory),
                  ("ClientToken" Core..=) Core.<$> clientToken])

instance Core.AWSRequest DeleteOrganization where
        type Rs DeleteOrganization = DeleteOrganizationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteOrganizationResponse' Core.<$>
                   (x Core..:? "OrganizationId") Core.<*> x Core..:? "State" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteOrganizationResponse' smart constructor.
data DeleteOrganizationResponse = DeleteOrganizationResponse'
  { organizationId :: Core.Maybe Types.OrganizationId
    -- ^ The organization ID.
  , state :: Core.Maybe Core.Text
    -- ^ The state of the organization.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOrganizationResponse' value with any optional fields omitted.
mkDeleteOrganizationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteOrganizationResponse
mkDeleteOrganizationResponse responseStatus
  = DeleteOrganizationResponse'{organizationId = Core.Nothing,
                                state = Core.Nothing, responseStatus}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorfrsOrganizationId :: Lens.Lens' DeleteOrganizationResponse (Core.Maybe Types.OrganizationId)
dorfrsOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE dorfrsOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The state of the organization.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorfrsState :: Lens.Lens' DeleteOrganizationResponse (Core.Maybe Core.Text)
dorfrsState = Lens.field @"state"
{-# INLINEABLE dorfrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorfrsResponseStatus :: Lens.Lens' DeleteOrganizationResponse Core.Int
dorfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dorfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
