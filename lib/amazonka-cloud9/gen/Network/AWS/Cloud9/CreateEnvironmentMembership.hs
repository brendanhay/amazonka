{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.CreateEnvironmentMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an environment member to an AWS Cloud9 development environment.
module Network.AWS.Cloud9.CreateEnvironmentMembership
  ( -- * Creating a request
    CreateEnvironmentMembership (..),
    mkCreateEnvironmentMembership,

    -- ** Request lenses
    cemEnvironmentId,
    cemUserArn,
    cemPermissions,

    -- * Destructuring the response
    CreateEnvironmentMembershipResponse (..),
    mkCreateEnvironmentMembershipResponse,

    -- ** Response lenses
    cemrrsMembership,
    cemrrsResponseStatus,
  )
where

import qualified Network.AWS.Cloud9.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateEnvironmentMembership' smart constructor.
data CreateEnvironmentMembership = CreateEnvironmentMembership'
  { -- | The ID of the environment that contains the environment member you want to add.
    environmentId :: Types.EnvironmentId,
    -- | The Amazon Resource Name (ARN) of the environment member you want to add.
    userArn :: Types.UserArn,
    -- | The type of environment member permissions you want to associate with this environment member. Available values include:
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

-- | Creates a 'CreateEnvironmentMembership' value with any optional fields omitted.
mkCreateEnvironmentMembership ::
  -- | 'environmentId'
  Types.EnvironmentId ->
  -- | 'userArn'
  Types.UserArn ->
  -- | 'permissions'
  Types.MemberPermissions ->
  CreateEnvironmentMembership
mkCreateEnvironmentMembership environmentId userArn permissions =
  CreateEnvironmentMembership' {environmentId, userArn, permissions}

-- | The ID of the environment that contains the environment member you want to add.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemEnvironmentId :: Lens.Lens' CreateEnvironmentMembership Types.EnvironmentId
cemEnvironmentId = Lens.field @"environmentId"
{-# DEPRECATED cemEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The Amazon Resource Name (ARN) of the environment member you want to add.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemUserArn :: Lens.Lens' CreateEnvironmentMembership Types.UserArn
cemUserArn = Lens.field @"userArn"
{-# DEPRECATED cemUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

-- | The type of environment member permissions you want to associate with this environment member. Available values include:
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
cemPermissions :: Lens.Lens' CreateEnvironmentMembership Types.MemberPermissions
cemPermissions = Lens.field @"permissions"
{-# DEPRECATED cemPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

instance Core.FromJSON CreateEnvironmentMembership where
  toJSON CreateEnvironmentMembership {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("environmentId" Core..= environmentId),
            Core.Just ("userArn" Core..= userArn),
            Core.Just ("permissions" Core..= permissions)
          ]
      )

instance Core.AWSRequest CreateEnvironmentMembership where
  type
    Rs CreateEnvironmentMembership =
      CreateEnvironmentMembershipResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCloud9WorkspaceManagementService.CreateEnvironmentMembership"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEnvironmentMembershipResponse'
            Core.<$> (x Core..:? "membership") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateEnvironmentMembershipResponse' smart constructor.
data CreateEnvironmentMembershipResponse = CreateEnvironmentMembershipResponse'
  { -- | Information about the environment member that was added.
    membership :: Core.Maybe Types.EnvironmentMember,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateEnvironmentMembershipResponse' value with any optional fields omitted.
mkCreateEnvironmentMembershipResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateEnvironmentMembershipResponse
mkCreateEnvironmentMembershipResponse responseStatus =
  CreateEnvironmentMembershipResponse'
    { membership = Core.Nothing,
      responseStatus
    }

-- | Information about the environment member that was added.
--
-- /Note:/ Consider using 'membership' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemrrsMembership :: Lens.Lens' CreateEnvironmentMembershipResponse (Core.Maybe Types.EnvironmentMember)
cemrrsMembership = Lens.field @"membership"
{-# DEPRECATED cemrrsMembership "Use generic-lens or generic-optics with 'membership' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemrrsResponseStatus :: Lens.Lens' CreateEnvironmentMembershipResponse Core.Int
cemrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cemrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
