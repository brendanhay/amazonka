{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a room profile by the profile ARN.
module Network.AWS.AlexaBusiness.DeleteProfile
  ( -- * Creating a request
    DeleteProfile (..),
    mkDeleteProfile,

    -- ** Request lenses
    dpProfileARN,

    -- * Destructuring the response
    DeleteProfileResponse (..),
    mkDeleteProfileResponse,

    -- ** Response lenses
    dprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteProfile' smart constructor.
newtype DeleteProfile = DeleteProfile'
  { profileARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProfile' with the minimum fields required to make a request.
--
-- * 'profileARN' - The ARN of the room profile to delete. Required.
mkDeleteProfile ::
  DeleteProfile
mkDeleteProfile = DeleteProfile' {profileARN = Lude.Nothing}

-- | The ARN of the room profile to delete. Required.
--
-- /Note:/ Consider using 'profileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpProfileARN :: Lens.Lens' DeleteProfile (Lude.Maybe Lude.Text)
dpProfileARN = Lens.lens (profileARN :: DeleteProfile -> Lude.Maybe Lude.Text) (\s a -> s {profileARN = a} :: DeleteProfile)
{-# DEPRECATED dpProfileARN "Use generic-lens or generic-optics with 'profileARN' instead." #-}

instance Lude.AWSRequest DeleteProfile where
  type Rs DeleteProfile = DeleteProfileResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteProfileResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.DeleteProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteProfile where
  toJSON DeleteProfile' {..} =
    Lude.object
      (Lude.catMaybes [("ProfileArn" Lude..=) Lude.<$> profileARN])

instance Lude.ToPath DeleteProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteProfileResponse' smart constructor.
newtype DeleteProfileResponse = DeleteProfileResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProfileResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteProfileResponse
mkDeleteProfileResponse pResponseStatus_ =
  DeleteProfileResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DeleteProfileResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DeleteProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteProfileResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
