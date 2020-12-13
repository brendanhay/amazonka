{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified user by user ARN and enrollment ARN.
module Network.AWS.AlexaBusiness.DeleteUser
  ( -- * Creating a request
    DeleteUser (..),
    mkDeleteUser,

    -- ** Request lenses
    duEnrollmentId,
    duUserARN,

    -- * Destructuring the response
    DeleteUserResponse (..),
    mkDeleteUserResponse,

    -- ** Response lenses
    dursResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | The ARN of the user's enrollment in the organization. Required.
    enrollmentId :: Lude.Text,
    -- | The ARN of the user to delete in the organization. Required.
    userARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- * 'enrollmentId' - The ARN of the user's enrollment in the organization. Required.
-- * 'userARN' - The ARN of the user to delete in the organization. Required.
mkDeleteUser ::
  -- | 'enrollmentId'
  Lude.Text ->
  DeleteUser
mkDeleteUser pEnrollmentId_ =
  DeleteUser'
    { enrollmentId = pEnrollmentId_,
      userARN = Lude.Nothing
    }

-- | The ARN of the user's enrollment in the organization. Required.
--
-- /Note:/ Consider using 'enrollmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duEnrollmentId :: Lens.Lens' DeleteUser Lude.Text
duEnrollmentId = Lens.lens (enrollmentId :: DeleteUser -> Lude.Text) (\s a -> s {enrollmentId = a} :: DeleteUser)
{-# DEPRECATED duEnrollmentId "Use generic-lens or generic-optics with 'enrollmentId' instead." #-}

-- | The ARN of the user to delete in the organization. Required.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUserARN :: Lens.Lens' DeleteUser (Lude.Maybe Lude.Text)
duUserARN = Lens.lens (userARN :: DeleteUser -> Lude.Maybe Lude.Text) (\s a -> s {userARN = a} :: DeleteUser)
{-# DEPRECATED duUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

instance Lude.AWSRequest DeleteUser where
  type Rs DeleteUser = DeleteUserResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteUserResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.DeleteUser" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteUser where
  toJSON DeleteUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EnrollmentId" Lude..= enrollmentId),
            ("UserArn" Lude..=) Lude.<$> userARN
          ]
      )

instance Lude.ToPath DeleteUser where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUser where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUserResponse' smart constructor.
newtype DeleteUserResponse = DeleteUserResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteUserResponse
mkDeleteUserResponse pResponseStatus_ =
  DeleteUserResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursResponseStatus :: Lens.Lens' DeleteUserResponse Lude.Int
dursResponseStatus = Lens.lens (responseStatus :: DeleteUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteUserResponse)
{-# DEPRECATED dursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
