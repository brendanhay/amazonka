{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.DeleteEnvironmentMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an environment member from an AWS Cloud9 development environment.
module Network.AWS.Cloud9.DeleteEnvironmentMembership
  ( -- * Creating a request
    DeleteEnvironmentMembership (..),
    mkDeleteEnvironmentMembership,

    -- ** Request lenses
    demEnvironmentId,
    demUserArn,

    -- * Destructuring the response
    DeleteEnvironmentMembershipResponse (..),
    mkDeleteEnvironmentMembershipResponse,

    -- ** Response lenses
    demrrsResponseStatus,
  )
where

import qualified Network.AWS.Cloud9.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteEnvironmentMembership' smart constructor.
data DeleteEnvironmentMembership = DeleteEnvironmentMembership'
  { -- | The ID of the environment to delete the environment member from.
    environmentId :: Types.EnvironmentId,
    -- | The Amazon Resource Name (ARN) of the environment member to delete from the environment.
    userArn :: Types.UserArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEnvironmentMembership' value with any optional fields omitted.
mkDeleteEnvironmentMembership ::
  -- | 'environmentId'
  Types.EnvironmentId ->
  -- | 'userArn'
  Types.UserArn ->
  DeleteEnvironmentMembership
mkDeleteEnvironmentMembership environmentId userArn =
  DeleteEnvironmentMembership' {environmentId, userArn}

-- | The ID of the environment to delete the environment member from.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demEnvironmentId :: Lens.Lens' DeleteEnvironmentMembership Types.EnvironmentId
demEnvironmentId = Lens.field @"environmentId"
{-# DEPRECATED demEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The Amazon Resource Name (ARN) of the environment member to delete from the environment.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demUserArn :: Lens.Lens' DeleteEnvironmentMembership Types.UserArn
demUserArn = Lens.field @"userArn"
{-# DEPRECATED demUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

instance Core.FromJSON DeleteEnvironmentMembership where
  toJSON DeleteEnvironmentMembership {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("environmentId" Core..= environmentId),
            Core.Just ("userArn" Core..= userArn)
          ]
      )

instance Core.AWSRequest DeleteEnvironmentMembership where
  type
    Rs DeleteEnvironmentMembership =
      DeleteEnvironmentMembershipResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCloud9WorkspaceManagementService.DeleteEnvironmentMembership"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEnvironmentMembershipResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteEnvironmentMembershipResponse' smart constructor.
newtype DeleteEnvironmentMembershipResponse = DeleteEnvironmentMembershipResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEnvironmentMembershipResponse' value with any optional fields omitted.
mkDeleteEnvironmentMembershipResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteEnvironmentMembershipResponse
mkDeleteEnvironmentMembershipResponse responseStatus =
  DeleteEnvironmentMembershipResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demrrsResponseStatus :: Lens.Lens' DeleteEnvironmentMembershipResponse Core.Int
demrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED demrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
