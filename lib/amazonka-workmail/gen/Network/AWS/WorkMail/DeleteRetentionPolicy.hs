{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteRetentionPolicy (..),
    mkDeleteRetentionPolicy,

    -- ** Request lenses
    drpOrganizationId,
    drpId,

    -- * Destructuring the response
    DeleteRetentionPolicyResponse (..),
    mkDeleteRetentionPolicyResponse,

    -- ** Response lenses
    drprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDeleteRetentionPolicy' smart constructor.
data DeleteRetentionPolicy = DeleteRetentionPolicy'
  { -- | The organization ID.
    organizationId :: Types.OrganizationId,
    -- | The retention policy ID.
    id :: Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRetentionPolicy' value with any optional fields omitted.
mkDeleteRetentionPolicy ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'id'
  Types.Id ->
  DeleteRetentionPolicy
mkDeleteRetentionPolicy organizationId id =
  DeleteRetentionPolicy' {organizationId, id}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpOrganizationId :: Lens.Lens' DeleteRetentionPolicy Types.OrganizationId
drpOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED drpOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The retention policy ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpId :: Lens.Lens' DeleteRetentionPolicy Types.Id
drpId = Lens.field @"id"
{-# DEPRECATED drpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromJSON DeleteRetentionPolicy where
  toJSON DeleteRetentionPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("Id" Core..= id)
          ]
      )

instance Core.AWSRequest DeleteRetentionPolicy where
  type Rs DeleteRetentionPolicy = DeleteRetentionPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.DeleteRetentionPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRetentionPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteRetentionPolicyResponse' smart constructor.
newtype DeleteRetentionPolicyResponse = DeleteRetentionPolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRetentionPolicyResponse' value with any optional fields omitted.
mkDeleteRetentionPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteRetentionPolicyResponse
mkDeleteRetentionPolicyResponse responseStatus =
  DeleteRetentionPolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsResponseStatus :: Lens.Lens' DeleteRetentionPolicyResponse Core.Int
drprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
