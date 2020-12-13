{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.DeleteUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user profile in AWS CodeStar, including all personal preference data associated with that profile, such as display name and email address. It does not delete the history of that user, for example the history of commits made by that user.
module Network.AWS.CodeStar.DeleteUserProfile
  ( -- * Creating a request
    DeleteUserProfile (..),
    mkDeleteUserProfile,

    -- ** Request lenses
    dUserARN,

    -- * Destructuring the response
    DeleteUserProfileResponse (..),
    mkDeleteUserProfileResponse,

    -- ** Response lenses
    dupfrsUserARN,
    dupfrsResponseStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUserProfile' smart constructor.
newtype DeleteUserProfile = DeleteUserProfile'
  { -- | The Amazon Resource Name (ARN) of the user to delete from AWS CodeStar.
    userARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserProfile' with the minimum fields required to make a request.
--
-- * 'userARN' - The Amazon Resource Name (ARN) of the user to delete from AWS CodeStar.
mkDeleteUserProfile ::
  -- | 'userARN'
  Lude.Text ->
  DeleteUserProfile
mkDeleteUserProfile pUserARN_ =
  DeleteUserProfile' {userARN = pUserARN_}

-- | The Amazon Resource Name (ARN) of the user to delete from AWS CodeStar.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUserARN :: Lens.Lens' DeleteUserProfile Lude.Text
dUserARN = Lens.lens (userARN :: DeleteUserProfile -> Lude.Text) (\s a -> s {userARN = a} :: DeleteUserProfile)
{-# DEPRECATED dUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

instance Lude.AWSRequest DeleteUserProfile where
  type Rs DeleteUserProfile = DeleteUserProfileResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteUserProfileResponse'
            Lude.<$> (x Lude..:> "userArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteUserProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.DeleteUserProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteUserProfile where
  toJSON DeleteUserProfile' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("userArn" Lude..= userARN)])

instance Lude.ToPath DeleteUserProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUserProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUserProfileResponse' smart constructor.
data DeleteUserProfileResponse = DeleteUserProfileResponse'
  { -- | The Amazon Resource Name (ARN) of the user deleted from AWS CodeStar.
    userARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserProfileResponse' with the minimum fields required to make a request.
--
-- * 'userARN' - The Amazon Resource Name (ARN) of the user deleted from AWS CodeStar.
-- * 'responseStatus' - The response status code.
mkDeleteUserProfileResponse ::
  -- | 'userARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteUserProfileResponse
mkDeleteUserProfileResponse pUserARN_ pResponseStatus_ =
  DeleteUserProfileResponse'
    { userARN = pUserARN_,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the user deleted from AWS CodeStar.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupfrsUserARN :: Lens.Lens' DeleteUserProfileResponse Lude.Text
dupfrsUserARN = Lens.lens (userARN :: DeleteUserProfileResponse -> Lude.Text) (\s a -> s {userARN = a} :: DeleteUserProfileResponse)
{-# DEPRECATED dupfrsUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupfrsResponseStatus :: Lens.Lens' DeleteUserProfileResponse Lude.Int
dupfrsResponseStatus = Lens.lens (responseStatus :: DeleteUserProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteUserProfileResponse)
{-# DEPRECATED dupfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
