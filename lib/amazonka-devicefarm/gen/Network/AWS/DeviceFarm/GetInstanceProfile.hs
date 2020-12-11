{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified instance profile.
module Network.AWS.DeviceFarm.GetInstanceProfile
  ( -- * Creating a request
    GetInstanceProfile (..),
    mkGetInstanceProfile,

    -- ** Request lenses
    gipArn,

    -- * Destructuring the response
    GetInstanceProfileResponse (..),
    mkGetInstanceProfileResponse,

    -- ** Response lenses
    giprsInstanceProfile,
    giprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetInstanceProfile' smart constructor.
newtype GetInstanceProfile = GetInstanceProfile' {arn :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInstanceProfile' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of an instance profile.
mkGetInstanceProfile ::
  -- | 'arn'
  Lude.Text ->
  GetInstanceProfile
mkGetInstanceProfile pArn_ = GetInstanceProfile' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of an instance profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipArn :: Lens.Lens' GetInstanceProfile Lude.Text
gipArn = Lens.lens (arn :: GetInstanceProfile -> Lude.Text) (\s a -> s {arn = a} :: GetInstanceProfile)
{-# DEPRECATED gipArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest GetInstanceProfile where
  type Rs GetInstanceProfile = GetInstanceProfileResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInstanceProfileResponse'
            Lude.<$> (x Lude..?> "instanceProfile")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInstanceProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.GetInstanceProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetInstanceProfile where
  toJSON GetInstanceProfile' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath GetInstanceProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInstanceProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInstanceProfileResponse' smart constructor.
data GetInstanceProfileResponse = GetInstanceProfileResponse'
  { instanceProfile ::
      Lude.Maybe InstanceProfile,
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

-- | Creates a value of 'GetInstanceProfileResponse' with the minimum fields required to make a request.
--
-- * 'instanceProfile' - An object that contains information about an instance profile.
-- * 'responseStatus' - The response status code.
mkGetInstanceProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInstanceProfileResponse
mkGetInstanceProfileResponse pResponseStatus_ =
  GetInstanceProfileResponse'
    { instanceProfile = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains information about an instance profile.
--
-- /Note:/ Consider using 'instanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprsInstanceProfile :: Lens.Lens' GetInstanceProfileResponse (Lude.Maybe InstanceProfile)
giprsInstanceProfile = Lens.lens (instanceProfile :: GetInstanceProfileResponse -> Lude.Maybe InstanceProfile) (\s a -> s {instanceProfile = a} :: GetInstanceProfileResponse)
{-# DEPRECATED giprsInstanceProfile "Use generic-lens or generic-optics with 'instanceProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprsResponseStatus :: Lens.Lens' GetInstanceProfileResponse Lude.Int
giprsResponseStatus = Lens.lens (responseStatus :: GetInstanceProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInstanceProfileResponse)
{-# DEPRECATED giprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
