{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DeleteRetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified retention policy from the specified organization.
module Network.AWS.WorkMail.DeleteRetentionPolicy
    (
    -- * Creating a request
      DeleteRetentionPolicy (..)
    , mkDeleteRetentionPolicy
    -- ** Request lenses
    , drpOrganizationId
    , drpId

    -- * Destructuring the response
    , DeleteRetentionPolicyResponse (..)
    , mkDeleteRetentionPolicyResponse
    -- ** Response lenses
    , drprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDeleteRetentionPolicy' smart constructor.
data DeleteRetentionPolicy = DeleteRetentionPolicy'
  { organizationId :: Types.OrganizationId
    -- ^ The organization ID.
  , id :: Types.Id
    -- ^ The retention policy ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRetentionPolicy' value with any optional fields omitted.
mkDeleteRetentionPolicy
    :: Types.OrganizationId -- ^ 'organizationId'
    -> Types.Id -- ^ 'id'
    -> DeleteRetentionPolicy
mkDeleteRetentionPolicy organizationId id
  = DeleteRetentionPolicy'{organizationId, id}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpOrganizationId :: Lens.Lens' DeleteRetentionPolicy Types.OrganizationId
drpOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE drpOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The retention policy ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpId :: Lens.Lens' DeleteRetentionPolicy Types.Id
drpId = Lens.field @"id"
{-# INLINEABLE drpId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DeleteRetentionPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteRetentionPolicy where
        toHeaders DeleteRetentionPolicy{..}
          = Core.pure
              ("X-Amz-Target", "WorkMailService.DeleteRetentionPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteRetentionPolicy where
        toJSON DeleteRetentionPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("Id" Core..= id)])

instance Core.AWSRequest DeleteRetentionPolicy where
        type Rs DeleteRetentionPolicy = DeleteRetentionPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteRetentionPolicyResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRetentionPolicyResponse' smart constructor.
newtype DeleteRetentionPolicyResponse = DeleteRetentionPolicyResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRetentionPolicyResponse' value with any optional fields omitted.
mkDeleteRetentionPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteRetentionPolicyResponse
mkDeleteRetentionPolicyResponse responseStatus
  = DeleteRetentionPolicyResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsResponseStatus :: Lens.Lens' DeleteRetentionPolicyResponse Core.Int
drprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
