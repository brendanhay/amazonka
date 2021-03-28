{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DeregisterFromWorkMail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Mark a user, group, or resource as no longer used in Amazon WorkMail. This action disassociates the mailbox and schedules it for clean-up. WorkMail keeps mailboxes for 30 days before they are permanently removed. The functionality in the console is /Disable/ .
module Network.AWS.WorkMail.DeregisterFromWorkMail
    (
    -- * Creating a request
      DeregisterFromWorkMail (..)
    , mkDeregisterFromWorkMail
    -- ** Request lenses
    , dfwmOrganizationId
    , dfwmEntityId

    -- * Destructuring the response
    , DeregisterFromWorkMailResponse (..)
    , mkDeregisterFromWorkMailResponse
    -- ** Response lenses
    , dfwmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDeregisterFromWorkMail' smart constructor.
data DeregisterFromWorkMail = DeregisterFromWorkMail'
  { organizationId :: Types.OrganizationId
    -- ^ The identifier for the organization under which the Amazon WorkMail entity exists.
  , entityId :: Types.EntityId
    -- ^ The identifier for the member (user or group) to be updated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterFromWorkMail' value with any optional fields omitted.
mkDeregisterFromWorkMail
    :: Types.OrganizationId -- ^ 'organizationId'
    -> Types.EntityId -- ^ 'entityId'
    -> DeregisterFromWorkMail
mkDeregisterFromWorkMail organizationId entityId
  = DeregisterFromWorkMail'{organizationId, entityId}

-- | The identifier for the organization under which the Amazon WorkMail entity exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfwmOrganizationId :: Lens.Lens' DeregisterFromWorkMail Types.OrganizationId
dfwmOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE dfwmOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The identifier for the member (user or group) to be updated.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfwmEntityId :: Lens.Lens' DeregisterFromWorkMail Types.EntityId
dfwmEntityId = Lens.field @"entityId"
{-# INLINEABLE dfwmEntityId #-}
{-# DEPRECATED entityId "Use generic-lens or generic-optics with 'entityId' instead"  #-}

instance Core.ToQuery DeregisterFromWorkMail where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeregisterFromWorkMail where
        toHeaders DeregisterFromWorkMail{..}
          = Core.pure
              ("X-Amz-Target", "WorkMailService.DeregisterFromWorkMail")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeregisterFromWorkMail where
        toJSON DeregisterFromWorkMail{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("EntityId" Core..= entityId)])

instance Core.AWSRequest DeregisterFromWorkMail where
        type Rs DeregisterFromWorkMail = DeregisterFromWorkMailResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeregisterFromWorkMailResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterFromWorkMailResponse' smart constructor.
newtype DeregisterFromWorkMailResponse = DeregisterFromWorkMailResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterFromWorkMailResponse' value with any optional fields omitted.
mkDeregisterFromWorkMailResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeregisterFromWorkMailResponse
mkDeregisterFromWorkMailResponse responseStatus
  = DeregisterFromWorkMailResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfwmrrsResponseStatus :: Lens.Lens' DeregisterFromWorkMailResponse Core.Int
dfwmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dfwmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
