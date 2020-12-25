{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteBillingGroup (..),
    mkDeleteBillingGroup,

    -- ** Request lenses
    dbgBillingGroupName,
    dbgExpectedVersion,

    -- * Destructuring the response
    DeleteBillingGroupResponse (..),
    mkDeleteBillingGroupResponse,

    -- ** Response lenses
    dbgrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBillingGroup' smart constructor.
data DeleteBillingGroup = DeleteBillingGroup'
  { -- | The name of the billing group.
    billingGroupName :: Types.BillingGroupName,
    -- | The expected version of the billing group. If the version of the billing group does not match the expected version specified in the request, the @DeleteBillingGroup@ request is rejected with a @VersionConflictException@ .
    expectedVersion :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBillingGroup' value with any optional fields omitted.
mkDeleteBillingGroup ::
  -- | 'billingGroupName'
  Types.BillingGroupName ->
  DeleteBillingGroup
mkDeleteBillingGroup billingGroupName =
  DeleteBillingGroup'
    { billingGroupName,
      expectedVersion = Core.Nothing
    }

-- | The name of the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgBillingGroupName :: Lens.Lens' DeleteBillingGroup Types.BillingGroupName
dbgBillingGroupName = Lens.field @"billingGroupName"
{-# DEPRECATED dbgBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

-- | The expected version of the billing group. If the version of the billing group does not match the expected version specified in the request, the @DeleteBillingGroup@ request is rejected with a @VersionConflictException@ .
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgExpectedVersion :: Lens.Lens' DeleteBillingGroup (Core.Maybe Core.Integer)
dbgExpectedVersion = Lens.field @"expectedVersion"
{-# DEPRECATED dbgExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

instance Core.AWSRequest DeleteBillingGroup where
  type Rs DeleteBillingGroup = DeleteBillingGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ("/billing-groups/" Core.<> (Core.toText billingGroupName)),
        Core._rqQuery =
          Core.toQueryValue "expectedVersion" Core.<$> expectedVersion,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteBillingGroupResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteBillingGroupResponse' smart constructor.
newtype DeleteBillingGroupResponse = DeleteBillingGroupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBillingGroupResponse' value with any optional fields omitted.
mkDeleteBillingGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteBillingGroupResponse
mkDeleteBillingGroupResponse responseStatus =
  DeleteBillingGroupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgrrsResponseStatus :: Lens.Lens' DeleteBillingGroupResponse Core.Int
dbgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
