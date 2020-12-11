{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AddRoleToInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified IAM role to the specified instance profile. An instance profile can contain only one role. (The number and size of IAM resources in an AWS account are limited. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS Quotas> in the /IAM User Guide/ .) You can remove the existing role and then add a different role to an instance profile. You must then wait for the change to appear across all of AWS because of <https://en.wikipedia.org/wiki/Eventual_consistency eventual consistency> . To force the change, you must <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DisassociateIamInstanceProfile.html disassociate the instance profile> and then <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_AssociateIamInstanceProfile.html associate the instance profile> , or you can stop your instance and then restart it.
--
-- For more information about roles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles> . For more information about instance profiles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles> .
module Network.AWS.IAM.AddRoleToInstanceProfile
  ( -- * Creating a request
    AddRoleToInstanceProfile (..),
    mkAddRoleToInstanceProfile,

    -- ** Request lenses
    artipInstanceProfileName,
    artipRoleName,

    -- * Destructuring the response
    AddRoleToInstanceProfileResponse (..),
    mkAddRoleToInstanceProfileResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddRoleToInstanceProfile' smart constructor.
data AddRoleToInstanceProfile = AddRoleToInstanceProfile'
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

-- | Creates a value of 'AddRoleToInstanceProfile' with the minimum fields required to make a request.
--
-- * 'instanceProfileName' - The name of the instance profile to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'roleName' - The name of the role to add.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkAddRoleToInstanceProfile ::
  -- | 'instanceProfileName'
  Lude.Text ->
  -- | 'roleName'
  Lude.Text ->
  AddRoleToInstanceProfile
mkAddRoleToInstanceProfile pInstanceProfileName_ pRoleName_ =
  AddRoleToInstanceProfile'
    { instanceProfileName =
        pInstanceProfileName_,
      roleName = pRoleName_
    }

-- | The name of the instance profile to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'instanceProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artipInstanceProfileName :: Lens.Lens' AddRoleToInstanceProfile Lude.Text
artipInstanceProfileName = Lens.lens (instanceProfileName :: AddRoleToInstanceProfile -> Lude.Text) (\s a -> s {instanceProfileName = a} :: AddRoleToInstanceProfile)
{-# DEPRECATED artipInstanceProfileName "Use generic-lens or generic-optics with 'instanceProfileName' instead." #-}

-- | The name of the role to add.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artipRoleName :: Lens.Lens' AddRoleToInstanceProfile Lude.Text
artipRoleName = Lens.lens (roleName :: AddRoleToInstanceProfile -> Lude.Text) (\s a -> s {roleName = a} :: AddRoleToInstanceProfile)
{-# DEPRECATED artipRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

instance Lude.AWSRequest AddRoleToInstanceProfile where
  type Rs AddRoleToInstanceProfile = AddRoleToInstanceProfileResponse
  request = Req.postQuery iamService
  response = Res.receiveNull AddRoleToInstanceProfileResponse'

instance Lude.ToHeaders AddRoleToInstanceProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AddRoleToInstanceProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery AddRoleToInstanceProfile where
  toQuery AddRoleToInstanceProfile' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AddRoleToInstanceProfile" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "InstanceProfileName" Lude.=: instanceProfileName,
        "RoleName" Lude.=: roleName
      ]

-- | /See:/ 'mkAddRoleToInstanceProfileResponse' smart constructor.
data AddRoleToInstanceProfileResponse = AddRoleToInstanceProfileResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddRoleToInstanceProfileResponse' with the minimum fields required to make a request.
mkAddRoleToInstanceProfileResponse ::
  AddRoleToInstanceProfileResponse
mkAddRoleToInstanceProfileResponse =
  AddRoleToInstanceProfileResponse'
