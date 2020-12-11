{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    atmRemoteAccessAllowed,
    atmClientRequestToken,
    atmProjectId,
    atmUserARN,
    atmProjectRole,

    -- * Destructuring the response
    AssociateTeamMemberResponse (..),
    mkAssociateTeamMemberResponse,

    -- ** Response lenses
    atmrsClientRequestToken,
    atmrsResponseStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateTeamMember' smart constructor.
data AssociateTeamMember = AssociateTeamMember'
  { remoteAccessAllowed ::
      Lude.Maybe Lude.Bool,
    clientRequestToken :: Lude.Maybe Lude.Text,
    projectId :: Lude.Text,
    userARN :: Lude.Text,
    projectRole :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateTeamMember' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - A user- or system-generated token that identifies the entity that requested the team member association to the project. This token can be used to repeat the request.
-- * 'projectId' - The ID of the project to which you will add the IAM user.
-- * 'projectRole' - The AWS CodeStar project role that will apply to this user. This role determines what actions a user can take in an AWS CodeStar project.
-- * 'remoteAccessAllowed' - Whether the team member is allowed to use an SSH public/private key pair to remotely access project resources, for example Amazon EC2 instances.
-- * 'userARN' - The Amazon Resource Name (ARN) for the IAM user you want to add to the AWS CodeStar project.
mkAssociateTeamMember ::
  -- | 'projectId'
  Lude.Text ->
  -- | 'userARN'
  Lude.Text ->
  -- | 'projectRole'
  Lude.Text ->
  AssociateTeamMember
mkAssociateTeamMember pProjectId_ pUserARN_ pProjectRole_ =
  AssociateTeamMember'
    { remoteAccessAllowed = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      projectId = pProjectId_,
      userARN = pUserARN_,
      projectRole = pProjectRole_
    }

-- | Whether the team member is allowed to use an SSH public/private key pair to remotely access project resources, for example Amazon EC2 instances.
--
-- /Note:/ Consider using 'remoteAccessAllowed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmRemoteAccessAllowed :: Lens.Lens' AssociateTeamMember (Lude.Maybe Lude.Bool)
atmRemoteAccessAllowed = Lens.lens (remoteAccessAllowed :: AssociateTeamMember -> Lude.Maybe Lude.Bool) (\s a -> s {remoteAccessAllowed = a} :: AssociateTeamMember)
{-# DEPRECATED atmRemoteAccessAllowed "Use generic-lens or generic-optics with 'remoteAccessAllowed' instead." #-}

-- | A user- or system-generated token that identifies the entity that requested the team member association to the project. This token can be used to repeat the request.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmClientRequestToken :: Lens.Lens' AssociateTeamMember (Lude.Maybe Lude.Text)
atmClientRequestToken = Lens.lens (clientRequestToken :: AssociateTeamMember -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: AssociateTeamMember)
{-# DEPRECATED atmClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The ID of the project to which you will add the IAM user.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmProjectId :: Lens.Lens' AssociateTeamMember Lude.Text
atmProjectId = Lens.lens (projectId :: AssociateTeamMember -> Lude.Text) (\s a -> s {projectId = a} :: AssociateTeamMember)
{-# DEPRECATED atmProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

-- | The Amazon Resource Name (ARN) for the IAM user you want to add to the AWS CodeStar project.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmUserARN :: Lens.Lens' AssociateTeamMember Lude.Text
atmUserARN = Lens.lens (userARN :: AssociateTeamMember -> Lude.Text) (\s a -> s {userARN = a} :: AssociateTeamMember)
{-# DEPRECATED atmUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | The AWS CodeStar project role that will apply to this user. This role determines what actions a user can take in an AWS CodeStar project.
--
-- /Note:/ Consider using 'projectRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmProjectRole :: Lens.Lens' AssociateTeamMember Lude.Text
atmProjectRole = Lens.lens (projectRole :: AssociateTeamMember -> Lude.Text) (\s a -> s {projectRole = a} :: AssociateTeamMember)
{-# DEPRECATED atmProjectRole "Use generic-lens or generic-optics with 'projectRole' instead." #-}

instance Lude.AWSRequest AssociateTeamMember where
  type Rs AssociateTeamMember = AssociateTeamMemberResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveJSON
      ( \s h x ->
          AssociateTeamMemberResponse'
            Lude.<$> (x Lude..?> "clientRequestToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateTeamMember where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.AssociateTeamMember" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateTeamMember where
  toJSON AssociateTeamMember' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("remoteAccessAllowed" Lude..=) Lude.<$> remoteAccessAllowed,
            ("clientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            Lude.Just ("projectId" Lude..= projectId),
            Lude.Just ("userArn" Lude..= userARN),
            Lude.Just ("projectRole" Lude..= projectRole)
          ]
      )

instance Lude.ToPath AssociateTeamMember where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateTeamMember where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateTeamMemberResponse' smart constructor.
data AssociateTeamMemberResponse = AssociateTeamMemberResponse'
  { clientRequestToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateTeamMemberResponse' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - The user- or system-generated token from the initial request that can be used to repeat the request.
-- * 'responseStatus' - The response status code.
mkAssociateTeamMemberResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateTeamMemberResponse
mkAssociateTeamMemberResponse pResponseStatus_ =
  AssociateTeamMemberResponse'
    { clientRequestToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The user- or system-generated token from the initial request that can be used to repeat the request.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmrsClientRequestToken :: Lens.Lens' AssociateTeamMemberResponse (Lude.Maybe Lude.Text)
atmrsClientRequestToken = Lens.lens (clientRequestToken :: AssociateTeamMemberResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: AssociateTeamMemberResponse)
{-# DEPRECATED atmrsClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atmrsResponseStatus :: Lens.Lens' AssociateTeamMemberResponse Lude.Int
atmrsResponseStatus = Lens.lens (responseStatus :: AssociateTeamMemberResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateTeamMemberResponse)
{-# DEPRECATED atmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
