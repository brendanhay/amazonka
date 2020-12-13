{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified instance profile. The instance profile must not have an associated role.
--
-- /Important:/ Make sure that you do not have any Amazon EC2 instances running with the instance profile you are about to delete. Deleting a role or instance profile that is associated with a running instance will break any applications running on the instance.
-- For more information about instance profiles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles> .
module Network.AWS.IAM.DeleteInstanceProfile
  ( -- * Creating a request
    DeleteInstanceProfile (..),
    mkDeleteInstanceProfile,

    -- ** Request lenses
    dipInstanceProfileName,

    -- * Destructuring the response
    DeleteInstanceProfileResponse (..),
    mkDeleteInstanceProfileResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteInstanceProfile' smart constructor.
newtype DeleteInstanceProfile = DeleteInstanceProfile'
  { -- | The name of the instance profile to delete.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    instanceProfileName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInstanceProfile' with the minimum fields required to make a request.
--
-- * 'instanceProfileName' - The name of the instance profile to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkDeleteInstanceProfile ::
  -- | 'instanceProfileName'
  Lude.Text ->
  DeleteInstanceProfile
mkDeleteInstanceProfile pInstanceProfileName_ =
  DeleteInstanceProfile'
    { instanceProfileName =
        pInstanceProfileName_
    }

-- | The name of the instance profile to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'instanceProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipInstanceProfileName :: Lens.Lens' DeleteInstanceProfile Lude.Text
dipInstanceProfileName = Lens.lens (instanceProfileName :: DeleteInstanceProfile -> Lude.Text) (\s a -> s {instanceProfileName = a} :: DeleteInstanceProfile)
{-# DEPRECATED dipInstanceProfileName "Use generic-lens or generic-optics with 'instanceProfileName' instead." #-}

instance Lude.AWSRequest DeleteInstanceProfile where
  type Rs DeleteInstanceProfile = DeleteInstanceProfileResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteInstanceProfileResponse'

instance Lude.ToHeaders DeleteInstanceProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteInstanceProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteInstanceProfile where
  toQuery DeleteInstanceProfile' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteInstanceProfile" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "InstanceProfileName" Lude.=: instanceProfileName
      ]

-- | /See:/ 'mkDeleteInstanceProfileResponse' smart constructor.
data DeleteInstanceProfileResponse = DeleteInstanceProfileResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInstanceProfileResponse' with the minimum fields required to make a request.
mkDeleteInstanceProfileResponse ::
  DeleteInstanceProfileResponse
mkDeleteInstanceProfileResponse = DeleteInstanceProfileResponse'
