{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a profile that can be applied to one or more private device instances.
module Network.AWS.DeviceFarm.DeleteInstanceProfile
  ( -- * Creating a request
    DeleteInstanceProfile (..),
    mkDeleteInstanceProfile,

    -- ** Request lenses
    dipArn,

    -- * Destructuring the response
    DeleteInstanceProfileResponse (..),
    mkDeleteInstanceProfileResponse,

    -- ** Response lenses
    diprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteInstanceProfile' smart constructor.
newtype DeleteInstanceProfile = DeleteInstanceProfile'
  { arn ::
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

-- | Creates a value of 'DeleteInstanceProfile' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the instance profile you are requesting to delete.
mkDeleteInstanceProfile ::
  -- | 'arn'
  Lude.Text ->
  DeleteInstanceProfile
mkDeleteInstanceProfile pArn_ = DeleteInstanceProfile' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the instance profile you are requesting to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipArn :: Lens.Lens' DeleteInstanceProfile Lude.Text
dipArn = Lens.lens (arn :: DeleteInstanceProfile -> Lude.Text) (\s a -> s {arn = a} :: DeleteInstanceProfile)
{-# DEPRECATED dipArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest DeleteInstanceProfile where
  type Rs DeleteInstanceProfile = DeleteInstanceProfileResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteInstanceProfileResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteInstanceProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.DeleteInstanceProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteInstanceProfile where
  toJSON DeleteInstanceProfile' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath DeleteInstanceProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteInstanceProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteInstanceProfileResponse' smart constructor.
newtype DeleteInstanceProfileResponse = DeleteInstanceProfileResponse'
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

-- | Creates a value of 'DeleteInstanceProfileResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteInstanceProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteInstanceProfileResponse
mkDeleteInstanceProfileResponse pResponseStatus_ =
  DeleteInstanceProfileResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprsResponseStatus :: Lens.Lens' DeleteInstanceProfileResponse Lude.Int
diprsResponseStatus = Lens.lens (responseStatus :: DeleteInstanceProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteInstanceProfileResponse)
{-# DEPRECATED diprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
