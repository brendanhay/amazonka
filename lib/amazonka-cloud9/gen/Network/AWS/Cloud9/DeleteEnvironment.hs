{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.DeleteEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Cloud9 development environment. If an Amazon EC2 instance is connected to the environment, also terminates the instance.
module Network.AWS.Cloud9.DeleteEnvironment
  ( -- * Creating a request
    DeleteEnvironment (..),
    mkDeleteEnvironment,

    -- ** Request lenses
    deEnvironmentId,

    -- * Destructuring the response
    DeleteEnvironmentResponse (..),
    mkDeleteEnvironmentResponse,

    -- ** Response lenses
    derrsResponseStatus,
  )
where

import qualified Network.AWS.Cloud9.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteEnvironment' smart constructor.
newtype DeleteEnvironment = DeleteEnvironment'
  { -- | The ID of the environment to delete.
    environmentId :: Types.EnvironmentId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEnvironment' value with any optional fields omitted.
mkDeleteEnvironment ::
  -- | 'environmentId'
  Types.EnvironmentId ->
  DeleteEnvironment
mkDeleteEnvironment environmentId =
  DeleteEnvironment' {environmentId}

-- | The ID of the environment to delete.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEnvironmentId :: Lens.Lens' DeleteEnvironment Types.EnvironmentId
deEnvironmentId = Lens.field @"environmentId"
{-# DEPRECATED deEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Core.FromJSON DeleteEnvironment where
  toJSON DeleteEnvironment {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("environmentId" Core..= environmentId)]
      )

instance Core.AWSRequest DeleteEnvironment where
  type Rs DeleteEnvironment = DeleteEnvironmentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCloud9WorkspaceManagementService.DeleteEnvironment"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEnvironmentResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteEnvironmentResponse' smart constructor.
newtype DeleteEnvironmentResponse = DeleteEnvironmentResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEnvironmentResponse' value with any optional fields omitted.
mkDeleteEnvironmentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteEnvironmentResponse
mkDeleteEnvironmentResponse responseStatus =
  DeleteEnvironmentResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DeleteEnvironmentResponse Core.Int
derrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED derrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
