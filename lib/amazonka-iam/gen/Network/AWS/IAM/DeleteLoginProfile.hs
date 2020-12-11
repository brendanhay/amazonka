{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteLoginProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the password for the specified IAM user, which terminates the user's ability to access AWS services through the AWS Management Console.
--
-- /Important:/ Deleting a user's password does not prevent a user from accessing AWS through the command line interface or the API. To prevent all user access, you must also either make any access keys inactive or delete them. For more information about making keys inactive or deleting them, see 'UpdateAccessKey' and 'DeleteAccessKey' .
module Network.AWS.IAM.DeleteLoginProfile
  ( -- * Creating a request
    DeleteLoginProfile (..),
    mkDeleteLoginProfile,

    -- ** Request lenses
    dlpUserName,

    -- * Destructuring the response
    DeleteLoginProfileResponse (..),
    mkDeleteLoginProfileResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLoginProfile' smart constructor.
newtype DeleteLoginProfile = DeleteLoginProfile'
  { userName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLoginProfile' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the user whose password you want to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkDeleteLoginProfile ::
  -- | 'userName'
  Lude.Text ->
  DeleteLoginProfile
mkDeleteLoginProfile pUserName_ =
  DeleteLoginProfile' {userName = pUserName_}

-- | The name of the user whose password you want to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlpUserName :: Lens.Lens' DeleteLoginProfile Lude.Text
dlpUserName = Lens.lens (userName :: DeleteLoginProfile -> Lude.Text) (\s a -> s {userName = a} :: DeleteLoginProfile)
{-# DEPRECATED dlpUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest DeleteLoginProfile where
  type Rs DeleteLoginProfile = DeleteLoginProfileResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteLoginProfileResponse'

instance Lude.ToHeaders DeleteLoginProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteLoginProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLoginProfile where
  toQuery DeleteLoginProfile' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteLoginProfile" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName
      ]

-- | /See:/ 'mkDeleteLoginProfileResponse' smart constructor.
data DeleteLoginProfileResponse = DeleteLoginProfileResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLoginProfileResponse' with the minimum fields required to make a request.
mkDeleteLoginProfileResponse ::
  DeleteLoginProfileResponse
mkDeleteLoginProfileResponse = DeleteLoginProfileResponse'
