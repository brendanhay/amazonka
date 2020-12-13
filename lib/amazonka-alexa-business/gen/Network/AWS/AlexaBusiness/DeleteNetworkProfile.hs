{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a network profile by the network profile ARN.
module Network.AWS.AlexaBusiness.DeleteNetworkProfile
  ( -- * Creating a request
    DeleteNetworkProfile (..),
    mkDeleteNetworkProfile,

    -- ** Request lenses
    dnpNetworkProfileARN,

    -- * Destructuring the response
    DeleteNetworkProfileResponse (..),
    mkDeleteNetworkProfileResponse,

    -- ** Response lenses
    dnprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteNetworkProfile' smart constructor.
newtype DeleteNetworkProfile = DeleteNetworkProfile'
  { -- | The ARN of the network profile associated with a device.
    networkProfileARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNetworkProfile' with the minimum fields required to make a request.
--
-- * 'networkProfileARN' - The ARN of the network profile associated with a device.
mkDeleteNetworkProfile ::
  -- | 'networkProfileARN'
  Lude.Text ->
  DeleteNetworkProfile
mkDeleteNetworkProfile pNetworkProfileARN_ =
  DeleteNetworkProfile' {networkProfileARN = pNetworkProfileARN_}

-- | The ARN of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnpNetworkProfileARN :: Lens.Lens' DeleteNetworkProfile Lude.Text
dnpNetworkProfileARN = Lens.lens (networkProfileARN :: DeleteNetworkProfile -> Lude.Text) (\s a -> s {networkProfileARN = a} :: DeleteNetworkProfile)
{-# DEPRECATED dnpNetworkProfileARN "Use generic-lens or generic-optics with 'networkProfileARN' instead." #-}

instance Lude.AWSRequest DeleteNetworkProfile where
  type Rs DeleteNetworkProfile = DeleteNetworkProfileResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteNetworkProfileResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteNetworkProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.DeleteNetworkProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteNetworkProfile where
  toJSON DeleteNetworkProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("NetworkProfileArn" Lude..= networkProfileARN)]
      )

instance Lude.ToPath DeleteNetworkProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteNetworkProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteNetworkProfileResponse' smart constructor.
newtype DeleteNetworkProfileResponse = DeleteNetworkProfileResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNetworkProfileResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteNetworkProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteNetworkProfileResponse
mkDeleteNetworkProfileResponse pResponseStatus_ =
  DeleteNetworkProfileResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnprsResponseStatus :: Lens.Lens' DeleteNetworkProfileResponse Lude.Int
dnprsResponseStatus = Lens.lens (responseStatus :: DeleteNetworkProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteNetworkProfileResponse)
{-# DEPRECATED dnprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
