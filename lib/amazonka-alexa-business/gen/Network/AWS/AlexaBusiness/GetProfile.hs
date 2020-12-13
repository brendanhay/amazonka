{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of a room profile by profile ARN.
module Network.AWS.AlexaBusiness.GetProfile
  ( -- * Creating a request
    GetProfile (..),
    mkGetProfile,

    -- ** Request lenses
    gpProfileARN,

    -- * Destructuring the response
    GetProfileResponse (..),
    mkGetProfileResponse,

    -- ** Response lenses
    gprsProfile,
    gprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetProfile' smart constructor.
newtype GetProfile = GetProfile'
  { -- | The ARN of the room profile for which to request details. Required.
    profileARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetProfile' with the minimum fields required to make a request.
--
-- * 'profileARN' - The ARN of the room profile for which to request details. Required.
mkGetProfile ::
  GetProfile
mkGetProfile = GetProfile' {profileARN = Lude.Nothing}

-- | The ARN of the room profile for which to request details. Required.
--
-- /Note:/ Consider using 'profileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpProfileARN :: Lens.Lens' GetProfile (Lude.Maybe Lude.Text)
gpProfileARN = Lens.lens (profileARN :: GetProfile -> Lude.Maybe Lude.Text) (\s a -> s {profileARN = a} :: GetProfile)
{-# DEPRECATED gpProfileARN "Use generic-lens or generic-optics with 'profileARN' instead." #-}

instance Lude.AWSRequest GetProfile where
  type Rs GetProfile = GetProfileResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetProfileResponse'
            Lude.<$> (x Lude..?> "Profile") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.GetProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetProfile where
  toJSON GetProfile' {..} =
    Lude.object
      (Lude.catMaybes [("ProfileArn" Lude..=) Lude.<$> profileARN])

instance Lude.ToPath GetProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery GetProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetProfileResponse' smart constructor.
data GetProfileResponse = GetProfileResponse'
  { -- | The details of the room profile requested. Required.
    profile :: Lude.Maybe Profile,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetProfileResponse' with the minimum fields required to make a request.
--
-- * 'profile' - The details of the room profile requested. Required.
-- * 'responseStatus' - The response status code.
mkGetProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetProfileResponse
mkGetProfileResponse pResponseStatus_ =
  GetProfileResponse'
    { profile = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The details of the room profile requested. Required.
--
-- /Note:/ Consider using 'profile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsProfile :: Lens.Lens' GetProfileResponse (Lude.Maybe Profile)
gprsProfile = Lens.lens (profile :: GetProfileResponse -> Lude.Maybe Profile) (\s a -> s {profile = a} :: GetProfileResponse)
{-# DEPRECATED gprsProfile "Use generic-lens or generic-optics with 'profile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsResponseStatus :: Lens.Lens' GetProfileResponse Lude.Int
gprsResponseStatus = Lens.lens (responseStatus :: GetProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetProfileResponse)
{-# DEPRECATED gprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
