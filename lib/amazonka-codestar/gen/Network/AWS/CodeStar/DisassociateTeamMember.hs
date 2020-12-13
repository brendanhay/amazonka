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
    dtmUserARN,
    dtmProjectId,

    -- * Destructuring the response
    DisassociateTeamMemberResponse (..),
    mkDisassociateTeamMemberResponse,

    -- ** Response lenses
    dtmrsResponseStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateTeamMember' smart constructor.
data DisassociateTeamMember = DisassociateTeamMember'
  { -- | The Amazon Resource Name (ARN) of the IAM user or group whom you want to remove from the project.
    userARN :: Lude.Text,
    -- | The ID of the AWS CodeStar project from which you want to remove a team member.
    projectId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateTeamMember' with the minimum fields required to make a request.
--
-- * 'userARN' - The Amazon Resource Name (ARN) of the IAM user or group whom you want to remove from the project.
-- * 'projectId' - The ID of the AWS CodeStar project from which you want to remove a team member.
mkDisassociateTeamMember ::
  -- | 'userARN'
  Lude.Text ->
  -- | 'projectId'
  Lude.Text ->
  DisassociateTeamMember
mkDisassociateTeamMember pUserARN_ pProjectId_ =
  DisassociateTeamMember'
    { userARN = pUserARN_,
      projectId = pProjectId_
    }

-- | The Amazon Resource Name (ARN) of the IAM user or group whom you want to remove from the project.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmUserARN :: Lens.Lens' DisassociateTeamMember Lude.Text
dtmUserARN = Lens.lens (userARN :: DisassociateTeamMember -> Lude.Text) (\s a -> s {userARN = a} :: DisassociateTeamMember)
{-# DEPRECATED dtmUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | The ID of the AWS CodeStar project from which you want to remove a team member.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmProjectId :: Lens.Lens' DisassociateTeamMember Lude.Text
dtmProjectId = Lens.lens (projectId :: DisassociateTeamMember -> Lude.Text) (\s a -> s {projectId = a} :: DisassociateTeamMember)
{-# DEPRECATED dtmProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

instance Lude.AWSRequest DisassociateTeamMember where
  type Rs DisassociateTeamMember = DisassociateTeamMemberResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateTeamMemberResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateTeamMember where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.DisassociateTeamMember" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateTeamMember where
  toJSON DisassociateTeamMember' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("userArn" Lude..= userARN),
            Lude.Just ("projectId" Lude..= projectId)
          ]
      )

instance Lude.ToPath DisassociateTeamMember where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateTeamMember where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateTeamMemberResponse' smart constructor.
newtype DisassociateTeamMemberResponse = DisassociateTeamMemberResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateTeamMemberResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateTeamMemberResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateTeamMemberResponse
mkDisassociateTeamMemberResponse pResponseStatus_ =
  DisassociateTeamMemberResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmrsResponseStatus :: Lens.Lens' DisassociateTeamMemberResponse Lude.Int
dtmrsResponseStatus = Lens.lens (responseStatus :: DisassociateTeamMemberResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateTeamMemberResponse)
{-# DEPRECATED dtmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
