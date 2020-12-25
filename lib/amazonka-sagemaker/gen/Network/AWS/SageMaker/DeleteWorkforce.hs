{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteWorkforce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to delete a workforce.
--
-- If you want to create a new workforce in an AWS Region where a workforce already exists, use this operation to delete the existing workforce and then use to create a new workforce.
-- /Important:/ If a private workforce contains one or more work teams, you must use the operation to delete all work teams before you delete the workforce. If you try to delete a workforce that contains one or more work teams, you will recieve a @ResourceInUse@ error.
module Network.AWS.SageMaker.DeleteWorkforce
  ( -- * Creating a request
    DeleteWorkforce (..),
    mkDeleteWorkforce,

    -- ** Request lenses
    dwWorkforceName,

    -- * Destructuring the response
    DeleteWorkforceResponse (..),
    mkDeleteWorkforceResponse,

    -- ** Response lenses
    dwrhrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteWorkforce' smart constructor.
newtype DeleteWorkforce = DeleteWorkforce'
  { -- | The name of the workforce.
    workforceName :: Types.WorkforceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWorkforce' value with any optional fields omitted.
mkDeleteWorkforce ::
  -- | 'workforceName'
  Types.WorkforceName ->
  DeleteWorkforce
mkDeleteWorkforce workforceName = DeleteWorkforce' {workforceName}

-- | The name of the workforce.
--
-- /Note:/ Consider using 'workforceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwWorkforceName :: Lens.Lens' DeleteWorkforce Types.WorkforceName
dwWorkforceName = Lens.field @"workforceName"
{-# DEPRECATED dwWorkforceName "Use generic-lens or generic-optics with 'workforceName' instead." #-}

instance Core.FromJSON DeleteWorkforce where
  toJSON DeleteWorkforce {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("WorkforceName" Core..= workforceName)]
      )

instance Core.AWSRequest DeleteWorkforce where
  type Rs DeleteWorkforce = DeleteWorkforceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DeleteWorkforce")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorkforceResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteWorkforceResponse' smart constructor.
newtype DeleteWorkforceResponse = DeleteWorkforceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWorkforceResponse' value with any optional fields omitted.
mkDeleteWorkforceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteWorkforceResponse
mkDeleteWorkforceResponse responseStatus =
  DeleteWorkforceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrhrsResponseStatus :: Lens.Lens' DeleteWorkforceResponse Core.Int
dwrhrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dwrhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
