{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.RemoveRoleFromInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified IAM role from the specified EC2 instance profile.
--
-- /Important:/ Make sure that you do not have any Amazon EC2 instances running with the role you are about to remove from the instance profile. Removing a role from an instance profile that is associated with a running instance might break any applications running on the instance.
-- For more information about IAM roles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles> . For more information about instance profiles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles> .
module Network.AWS.IAM.RemoveRoleFromInstanceProfile
  ( -- * Creating a request
    RemoveRoleFromInstanceProfile (..),
    mkRemoveRoleFromInstanceProfile,

    -- ** Request lenses
    rrfipInstanceProfileName,
    rrfipRoleName,

    -- * Destructuring the response
    RemoveRoleFromInstanceProfileResponse (..),
    mkRemoveRoleFromInstanceProfileResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveRoleFromInstanceProfile' smart constructor.
data RemoveRoleFromInstanceProfile = RemoveRoleFromInstanceProfile'
  { instanceProfileName ::
      Lude.Text,
    roleName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveRoleFromInstanceProfile' with the minimum fields required to make a request.
--
-- * 'instanceProfileName' - The name of the instance profile to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'roleName' - The name of the role to remove.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkRemoveRoleFromInstanceProfile ::
  -- | 'instanceProfileName'
  Lude.Text ->
  -- | 'roleName'
  Lude.Text ->
  RemoveRoleFromInstanceProfile
mkRemoveRoleFromInstanceProfile pInstanceProfileName_ pRoleName_ =
  RemoveRoleFromInstanceProfile'
    { instanceProfileName =
        pInstanceProfileName_,
      roleName = pRoleName_
    }

-- | The name of the instance profile to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'instanceProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfipInstanceProfileName :: Lens.Lens' RemoveRoleFromInstanceProfile Lude.Text
rrfipInstanceProfileName = Lens.lens (instanceProfileName :: RemoveRoleFromInstanceProfile -> Lude.Text) (\s a -> s {instanceProfileName = a} :: RemoveRoleFromInstanceProfile)
{-# DEPRECATED rrfipInstanceProfileName "Use generic-lens or generic-optics with 'instanceProfileName' instead." #-}

-- | The name of the role to remove.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfipRoleName :: Lens.Lens' RemoveRoleFromInstanceProfile Lude.Text
rrfipRoleName = Lens.lens (roleName :: RemoveRoleFromInstanceProfile -> Lude.Text) (\s a -> s {roleName = a} :: RemoveRoleFromInstanceProfile)
{-# DEPRECATED rrfipRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

instance Lude.AWSRequest RemoveRoleFromInstanceProfile where
  type
    Rs RemoveRoleFromInstanceProfile =
      RemoveRoleFromInstanceProfileResponse
  request = Req.postQuery iamService
  response = Res.receiveNull RemoveRoleFromInstanceProfileResponse'

instance Lude.ToHeaders RemoveRoleFromInstanceProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RemoveRoleFromInstanceProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveRoleFromInstanceProfile where
  toQuery RemoveRoleFromInstanceProfile' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RemoveRoleFromInstanceProfile" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "InstanceProfileName" Lude.=: instanceProfileName,
        "RoleName" Lude.=: roleName
      ]

-- | /See:/ 'mkRemoveRoleFromInstanceProfileResponse' smart constructor.
data RemoveRoleFromInstanceProfileResponse = RemoveRoleFromInstanceProfileResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveRoleFromInstanceProfileResponse' with the minimum fields required to make a request.
mkRemoveRoleFromInstanceProfileResponse ::
  RemoveRoleFromInstanceProfileResponse
mkRemoveRoleFromInstanceProfileResponse =
  RemoveRoleFromInstanceProfileResponse'
