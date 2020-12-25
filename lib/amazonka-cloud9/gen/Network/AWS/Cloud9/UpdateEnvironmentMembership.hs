{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.UpdateEnvironmentMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings of an existing environment member for an AWS Cloud9 development environment.
module Network.AWS.Cloud9.UpdateEnvironmentMembership
  ( -- * Creating a request
    UpdateEnvironmentMembership (..),
    mkUpdateEnvironmentMembership,

    -- ** Request lenses
    uemEnvironmentId,
    uemUserArn,
    uemPermissions,

    -- * Destructuring the response
    UpdateEnvironmentMembershipResponse (..),
    mkUpdateEnvironmentMembershipResponse,

    -- ** Response lenses
    uemrrsMembership,
    uemrrsResponseStatus,
  )
where

import qualified Network.AWS.Cloud9.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateEnvironmentMembership' smart constructor.
data UpdateEnvironmentMembership = UpdateEnvironmentMembership'
  { -- | The ID of the environment for the environment member whose settings you want to change.
    environmentId :: Types.EnvironmentId,
    -- | The Amazon Resource Name (ARN) of the environment member whose settings you want to change.
    userArn :: Types.UserArn,
    -- | The replacement type of environment member permissions you want to associate with this environment member. Available values include:
    --
    --
    --     * @read-only@ : Has read-only access to the environment.
    --
    --
    --     * @read-write@ : Has read-write access to the environment.
    permissions :: Types.MemberPermissions
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEnvironmentMembership' value with any optional fields omitted.
mkUpdateEnvironmentMembership ::
  -- | 'environmentId'
  Types.EnvironmentId ->
  -- | 'userArn'
  Types.UserArn ->
  -- | 'permissions'
  Types.MemberPermissions ->
  UpdateEnvironmentMembership
mkUpdateEnvironmentMembership environmentId userArn permissions =
  UpdateEnvironmentMembership' {environmentId, userArn, permissions}

-- | The ID of the environment for the environment member whose settings you want to change.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uemEnvironmentId :: Lens.Lens' UpdateEnvironmentMembership Types.EnvironmentId
uemEnvironmentId = Lens.field @"environmentId"
{-# DEPRECATED uemEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The Amazon Resource Name (ARN) of the environment member whose settings you want to change.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uemUserArn :: Lens.Lens' UpdateEnvironmentMembership Types.UserArn
uemUserArn = Lens.field @"userArn"
{-# DEPRECATED uemUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

-- | The replacement type of environment member permissions you want to associate with this environment member. Available values include:
--
--
--     * @read-only@ : Has read-only access to the environment.
--
--
--     * @read-write@ : Has read-write access to the environment.
--
--
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uemPermissions :: Lens.Lens' UpdateEnvironmentMembership Types.MemberPermissions
uemPermissions = Lens.field @"permissions"
{-# DEPRECATED uemPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

instance Core.FromJSON UpdateEnvironmentMembership where
  toJSON UpdateEnvironmentMembership {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("environmentId" Core..= environmentId),
            Core.Just ("userArn" Core..= userArn),
            Core.Just ("permissions" Core..= permissions)
          ]
      )

instance Core.AWSRequest UpdateEnvironmentMembership where
  type
    Rs UpdateEnvironmentMembership =
      UpdateEnvironmentMembershipResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCloud9WorkspaceManagementService.UpdateEnvironmentMembership"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEnvironmentMembershipResponse'
            Core.<$> (x Core..:? "membership") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateEnvironmentMembershipResponse' smart constructor.
data UpdateEnvironmentMembershipResponse = UpdateEnvironmentMembershipResponse'
  { -- | Information about the environment member whose settings were changed.
    membership :: Core.Maybe Types.EnvironmentMember,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateEnvironmentMembershipResponse' value with any optional fields omitted.
mkUpdateEnvironmentMembershipResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateEnvironmentMembershipResponse
mkUpdateEnvironmentMembershipResponse responseStatus =
  UpdateEnvironmentMembershipResponse'
    { membership = Core.Nothing,
      responseStatus
    }

-- | Information about the environment member whose settings were changed.
--
-- /Note:/ Consider using 'membership' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uemrrsMembership :: Lens.Lens' UpdateEnvironmentMembershipResponse (Core.Maybe Types.EnvironmentMember)
uemrrsMembership = Lens.field @"membership"
{-# DEPRECATED uemrrsMembership "Use generic-lens or generic-optics with 'membership' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uemrrsResponseStatus :: Lens.Lens' UpdateEnvironmentMembershipResponse Core.Int
uemrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uemrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
