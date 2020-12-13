{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.UpdateTeamMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a team member's attributes in an AWS CodeStar project. For example, you can change a team member's role in the project, or change whether they have remote access to project resources.
module Network.AWS.CodeStar.UpdateTeamMember
  ( -- * Creating a request
    UpdateTeamMember (..),
    mkUpdateTeamMember,

    -- ** Request lenses
    utmUserARN,
    utmProjectId,
    utmRemoteAccessAllowed,
    utmProjectRole,

    -- * Destructuring the response
    UpdateTeamMemberResponse (..),
    mkUpdateTeamMemberResponse,

    -- ** Response lenses
    utmrsUserARN,
    utmrsRemoteAccessAllowed,
    utmrsProjectRole,
    utmrsResponseStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateTeamMember' smart constructor.
data UpdateTeamMember = UpdateTeamMember'
  { -- | The Amazon Resource Name (ARN) of the user for whom you want to change team membership attributes.
    userARN :: Lude.Text,
    -- | The ID of the project.
    projectId :: Lude.Text,
    -- | Whether a team member is allowed to remotely access project resources using the SSH public key associated with the user's profile. Even if this is set to True, the user must associate a public key with their profile before the user can access resources.
    remoteAccessAllowed :: Lude.Maybe Lude.Bool,
    -- | The role assigned to the user in the project. Project roles have different levels of access. For more information, see <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams> in the /AWS CodeStar User Guide/ .
    projectRole :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTeamMember' with the minimum fields required to make a request.
--
-- * 'userARN' - The Amazon Resource Name (ARN) of the user for whom you want to change team membership attributes.
-- * 'projectId' - The ID of the project.
-- * 'remoteAccessAllowed' - Whether a team member is allowed to remotely access project resources using the SSH public key associated with the user's profile. Even if this is set to True, the user must associate a public key with their profile before the user can access resources.
-- * 'projectRole' - The role assigned to the user in the project. Project roles have different levels of access. For more information, see <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams> in the /AWS CodeStar User Guide/ .
mkUpdateTeamMember ::
  -- | 'userARN'
  Lude.Text ->
  -- | 'projectId'
  Lude.Text ->
  UpdateTeamMember
mkUpdateTeamMember pUserARN_ pProjectId_ =
  UpdateTeamMember'
    { userARN = pUserARN_,
      projectId = pProjectId_,
      remoteAccessAllowed = Lude.Nothing,
      projectRole = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the user for whom you want to change team membership attributes.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utmUserARN :: Lens.Lens' UpdateTeamMember Lude.Text
utmUserARN = Lens.lens (userARN :: UpdateTeamMember -> Lude.Text) (\s a -> s {userARN = a} :: UpdateTeamMember)
{-# DEPRECATED utmUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | The ID of the project.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utmProjectId :: Lens.Lens' UpdateTeamMember Lude.Text
utmProjectId = Lens.lens (projectId :: UpdateTeamMember -> Lude.Text) (\s a -> s {projectId = a} :: UpdateTeamMember)
{-# DEPRECATED utmProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

-- | Whether a team member is allowed to remotely access project resources using the SSH public key associated with the user's profile. Even if this is set to True, the user must associate a public key with their profile before the user can access resources.
--
-- /Note:/ Consider using 'remoteAccessAllowed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utmRemoteAccessAllowed :: Lens.Lens' UpdateTeamMember (Lude.Maybe Lude.Bool)
utmRemoteAccessAllowed = Lens.lens (remoteAccessAllowed :: UpdateTeamMember -> Lude.Maybe Lude.Bool) (\s a -> s {remoteAccessAllowed = a} :: UpdateTeamMember)
{-# DEPRECATED utmRemoteAccessAllowed "Use generic-lens or generic-optics with 'remoteAccessAllowed' instead." #-}

-- | The role assigned to the user in the project. Project roles have different levels of access. For more information, see <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams> in the /AWS CodeStar User Guide/ .
--
-- /Note:/ Consider using 'projectRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utmProjectRole :: Lens.Lens' UpdateTeamMember (Lude.Maybe Lude.Text)
utmProjectRole = Lens.lens (projectRole :: UpdateTeamMember -> Lude.Maybe Lude.Text) (\s a -> s {projectRole = a} :: UpdateTeamMember)
{-# DEPRECATED utmProjectRole "Use generic-lens or generic-optics with 'projectRole' instead." #-}

instance Lude.AWSRequest UpdateTeamMember where
  type Rs UpdateTeamMember = UpdateTeamMemberResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateTeamMemberResponse'
            Lude.<$> (x Lude..?> "userArn")
            Lude.<*> (x Lude..?> "remoteAccessAllowed")
            Lude.<*> (x Lude..?> "projectRole")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTeamMember where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.UpdateTeamMember" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateTeamMember where
  toJSON UpdateTeamMember' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("userArn" Lude..= userARN),
            Lude.Just ("projectId" Lude..= projectId),
            ("remoteAccessAllowed" Lude..=) Lude.<$> remoteAccessAllowed,
            ("projectRole" Lude..=) Lude.<$> projectRole
          ]
      )

instance Lude.ToPath UpdateTeamMember where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTeamMember where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateTeamMemberResponse' smart constructor.
data UpdateTeamMemberResponse = UpdateTeamMemberResponse'
  { -- | The Amazon Resource Name (ARN) of the user whose team membership attributes were updated.
    userARN :: Lude.Maybe Lude.Text,
    -- | Whether a team member is allowed to remotely access project resources using the SSH public key associated with the user's profile.
    remoteAccessAllowed :: Lude.Maybe Lude.Bool,
    -- | The project role granted to the user.
    projectRole :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTeamMemberResponse' with the minimum fields required to make a request.
--
-- * 'userARN' - The Amazon Resource Name (ARN) of the user whose team membership attributes were updated.
-- * 'remoteAccessAllowed' - Whether a team member is allowed to remotely access project resources using the SSH public key associated with the user's profile.
-- * 'projectRole' - The project role granted to the user.
-- * 'responseStatus' - The response status code.
mkUpdateTeamMemberResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTeamMemberResponse
mkUpdateTeamMemberResponse pResponseStatus_ =
  UpdateTeamMemberResponse'
    { userARN = Lude.Nothing,
      remoteAccessAllowed = Lude.Nothing,
      projectRole = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the user whose team membership attributes were updated.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utmrsUserARN :: Lens.Lens' UpdateTeamMemberResponse (Lude.Maybe Lude.Text)
utmrsUserARN = Lens.lens (userARN :: UpdateTeamMemberResponse -> Lude.Maybe Lude.Text) (\s a -> s {userARN = a} :: UpdateTeamMemberResponse)
{-# DEPRECATED utmrsUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | Whether a team member is allowed to remotely access project resources using the SSH public key associated with the user's profile.
--
-- /Note:/ Consider using 'remoteAccessAllowed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utmrsRemoteAccessAllowed :: Lens.Lens' UpdateTeamMemberResponse (Lude.Maybe Lude.Bool)
utmrsRemoteAccessAllowed = Lens.lens (remoteAccessAllowed :: UpdateTeamMemberResponse -> Lude.Maybe Lude.Bool) (\s a -> s {remoteAccessAllowed = a} :: UpdateTeamMemberResponse)
{-# DEPRECATED utmrsRemoteAccessAllowed "Use generic-lens or generic-optics with 'remoteAccessAllowed' instead." #-}

-- | The project role granted to the user.
--
-- /Note:/ Consider using 'projectRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utmrsProjectRole :: Lens.Lens' UpdateTeamMemberResponse (Lude.Maybe Lude.Text)
utmrsProjectRole = Lens.lens (projectRole :: UpdateTeamMemberResponse -> Lude.Maybe Lude.Text) (\s a -> s {projectRole = a} :: UpdateTeamMemberResponse)
{-# DEPRECATED utmrsProjectRole "Use generic-lens or generic-optics with 'projectRole' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utmrsResponseStatus :: Lens.Lens' UpdateTeamMemberResponse Lude.Int
utmrsResponseStatus = Lens.lens (responseStatus :: UpdateTeamMemberResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTeamMemberResponse)
{-# DEPRECATED utmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
