{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.AssociateTeamMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an IAM user to the team for an AWS CodeStar project.
module Network.AWS.CodeStar.AssociateTeamMember
  ( -- * Creating a request
    AssociateTeamMember (..),
    mkAssociateTeamMember,

    -- ** Request lenses
    atmProjectId,
    atmUserArn,
    atmProjectRole,
    atmClientRequestToken,
    atmRemoteAccessAllowed,

    -- * Destructuring the response
    AssociateTeamMemberResponse (..),
    mkAssociateTeamMemberResponse,

    -- ** Response lenses
    atmrrsClientRequestToken,
    atmrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateTeamMember' smart constructor.
data AssociateTeamMember = AssociateTeamMember'
  { -- | The ID of the project to which you will add the IAM user.
    projectId :: Types.ProjectId,
    -- | The Amazon Resource Name (ARN) for the IAM user you want to add to the AWS CodeStar project.
    userArn :: Types.UserArn,
    -- | The AWS CodeStar project role that will apply to this user. This role determines what actions a user can take in an AWS CodeStar project.
    projectRole :: Types.ProjectRole,
    -- | A user- or system-generated token that identifies the entity that requested the team member association to the project. This token can be used to repeat the request.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | Whether the team member is allowed to use an SSH public/private key pair to remotely access project resources, for example Amazon EC2 instances.
    remoteAccessAllowed :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateTeamMember' value with any optional fields omitted.
mkAssociateTeamMember ::
  -- | 'projectId'
  Types.ProjectId ->
  -- | 'userArn'
  Types.UserArn ->
  -- | 'projectRole'
  Types.ProjectRole ->
  AssociateTeamMember
mkAssociateTeamMember projectId userArn projectRole =
  AssociateTeamMember'
    { projectId,
      userArn,
      projectRole,
      clientRequestToken = Core.Nothing,
      remoteAccessAllowed = Core.Nothing
    }

-- | The ID of the project to which you will add the IAM user.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmProjectId :: Lens.Lens' AssociateTeamMember Types.ProjectId
atmProjectId = Lens.field @"projectId"
{-# DEPRECATED atmProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

-- | The Amazon Resource Name (ARN) for the IAM user you want to add to the AWS CodeStar project.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmUserArn :: Lens.Lens' AssociateTeamMember Types.UserArn
atmUserArn = Lens.field @"userArn"
{-# DEPRECATED atmUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

-- | The AWS CodeStar project role that will apply to this user. This role determines what actions a user can take in an AWS CodeStar project.
--
-- /Note:/ Consider using 'projectRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmProjectRole :: Lens.Lens' AssociateTeamMember Types.ProjectRole
atmProjectRole = Lens.field @"projectRole"
{-# DEPRECATED atmProjectRole "Use generic-lens or generic-optics with 'projectRole' instead." #-}

-- | A user- or system-generated token that identifies the entity that requested the team member association to the project. This token can be used to repeat the request.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmClientRequestToken :: Lens.Lens' AssociateTeamMember (Core.Maybe Types.ClientRequestToken)
atmClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED atmClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Whether the team member is allowed to use an SSH public/private key pair to remotely access project resources, for example Amazon EC2 instances.
--
-- /Note:/ Consider using 'remoteAccessAllowed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmRemoteAccessAllowed :: Lens.Lens' AssociateTeamMember (Core.Maybe Core.Bool)
atmRemoteAccessAllowed = Lens.field @"remoteAccessAllowed"
{-# DEPRECATED atmRemoteAccessAllowed "Use generic-lens or generic-optics with 'remoteAccessAllowed' instead." #-}

instance Core.FromJSON AssociateTeamMember where
  toJSON AssociateTeamMember {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("projectId" Core..= projectId),
            Core.Just ("userArn" Core..= userArn),
            Core.Just ("projectRole" Core..= projectRole),
            ("clientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("remoteAccessAllowed" Core..=) Core.<$> remoteAccessAllowed
          ]
      )

instance Core.AWSRequest AssociateTeamMember where
  type Rs AssociateTeamMember = AssociateTeamMemberResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeStar_20170419.AssociateTeamMember")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateTeamMemberResponse'
            Core.<$> (x Core..:? "clientRequestToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateTeamMemberResponse' smart constructor.
data AssociateTeamMemberResponse = AssociateTeamMemberResponse'
  { -- | The user- or system-generated token from the initial request that can be used to repeat the request.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateTeamMemberResponse' value with any optional fields omitted.
mkAssociateTeamMemberResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateTeamMemberResponse
mkAssociateTeamMemberResponse responseStatus =
  AssociateTeamMemberResponse'
    { clientRequestToken = Core.Nothing,
      responseStatus
    }

-- | The user- or system-generated token from the initial request that can be used to repeat the request.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmrrsClientRequestToken :: Lens.Lens' AssociateTeamMemberResponse (Core.Maybe Types.ClientRequestToken)
atmrrsClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED atmrrsClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmrrsResponseStatus :: Lens.Lens' AssociateTeamMemberResponse Core.Int
atmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED atmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
