{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.DisassociateTeamMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a user from a project. Removing a user from a project also removes the IAM policies from that user that allowed access to the project and its resources. Disassociating a team member does not remove that user's profile from AWS CodeStar. It does not remove the user from IAM.
module Network.AWS.CodeStar.DisassociateTeamMember
  ( -- * Creating a request
    DisassociateTeamMember (..),
    mkDisassociateTeamMember,

    -- ** Request lenses
    dtmProjectId,
    dtmUserArn,

    -- * Destructuring the response
    DisassociateTeamMemberResponse (..),
    mkDisassociateTeamMemberResponse,

    -- ** Response lenses
    dtmrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateTeamMember' smart constructor.
data DisassociateTeamMember = DisassociateTeamMember'
  { -- | The ID of the AWS CodeStar project from which you want to remove a team member.
    projectId :: Types.ProjectId,
    -- | The Amazon Resource Name (ARN) of the IAM user or group whom you want to remove from the project.
    userArn :: Types.UserArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateTeamMember' value with any optional fields omitted.
mkDisassociateTeamMember ::
  -- | 'projectId'
  Types.ProjectId ->
  -- | 'userArn'
  Types.UserArn ->
  DisassociateTeamMember
mkDisassociateTeamMember projectId userArn =
  DisassociateTeamMember' {projectId, userArn}

-- | The ID of the AWS CodeStar project from which you want to remove a team member.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmProjectId :: Lens.Lens' DisassociateTeamMember Types.ProjectId
dtmProjectId = Lens.field @"projectId"
{-# DEPRECATED dtmProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM user or group whom you want to remove from the project.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmUserArn :: Lens.Lens' DisassociateTeamMember Types.UserArn
dtmUserArn = Lens.field @"userArn"
{-# DEPRECATED dtmUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

instance Core.FromJSON DisassociateTeamMember where
  toJSON DisassociateTeamMember {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("projectId" Core..= projectId),
            Core.Just ("userArn" Core..= userArn)
          ]
      )

instance Core.AWSRequest DisassociateTeamMember where
  type Rs DisassociateTeamMember = DisassociateTeamMemberResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeStar_20170419.DisassociateTeamMember")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateTeamMemberResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateTeamMemberResponse' smart constructor.
newtype DisassociateTeamMemberResponse = DisassociateTeamMemberResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateTeamMemberResponse' value with any optional fields omitted.
mkDisassociateTeamMemberResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateTeamMemberResponse
mkDisassociateTeamMemberResponse responseStatus =
  DisassociateTeamMemberResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmrrsResponseStatus :: Lens.Lens' DisassociateTeamMemberResponse Core.Int
dtmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
