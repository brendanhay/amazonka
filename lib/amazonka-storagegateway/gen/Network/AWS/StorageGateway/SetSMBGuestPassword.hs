{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.SetSMBGuestPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the password for the guest user @smbguest@ . The @smbguest@ user is the user when the authentication method for the file share is set to @GuestAccess@ .
module Network.AWS.StorageGateway.SetSMBGuestPassword
  ( -- * Creating a request
    SetSMBGuestPassword (..),
    mkSetSMBGuestPassword,

    -- ** Request lenses
    ssmbgpGatewayARN,
    ssmbgpPassword,

    -- * Destructuring the response
    SetSMBGuestPasswordResponse (..),
    mkSetSMBGuestPasswordResponse,

    -- ** Response lenses
    ssmbgprsGatewayARN,
    ssmbgprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | SetSMBGuestPasswordInput
--
-- /See:/ 'mkSetSMBGuestPassword' smart constructor.
data SetSMBGuestPassword = SetSMBGuestPassword'
  { gatewayARN ::
      Lude.Text,
    password :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetSMBGuestPassword' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - The Amazon Resource Name (ARN) of the file gateway the SMB file share is associated with.
-- * 'password' - The password that you want to set for your SMB server.
mkSetSMBGuestPassword ::
  -- | 'gatewayARN'
  Lude.Text ->
  -- | 'password'
  Lude.Sensitive Lude.Text ->
  SetSMBGuestPassword
mkSetSMBGuestPassword pGatewayARN_ pPassword_ =
  SetSMBGuestPassword'
    { gatewayARN = pGatewayARN_,
      password = pPassword_
    }

-- | The Amazon Resource Name (ARN) of the file gateway the SMB file share is associated with.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmbgpGatewayARN :: Lens.Lens' SetSMBGuestPassword Lude.Text
ssmbgpGatewayARN = Lens.lens (gatewayARN :: SetSMBGuestPassword -> Lude.Text) (\s a -> s {gatewayARN = a} :: SetSMBGuestPassword)
{-# DEPRECATED ssmbgpGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The password that you want to set for your SMB server.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmbgpPassword :: Lens.Lens' SetSMBGuestPassword (Lude.Sensitive Lude.Text)
ssmbgpPassword = Lens.lens (password :: SetSMBGuestPassword -> Lude.Sensitive Lude.Text) (\s a -> s {password = a} :: SetSMBGuestPassword)
{-# DEPRECATED ssmbgpPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Lude.AWSRequest SetSMBGuestPassword where
  type Rs SetSMBGuestPassword = SetSMBGuestPasswordResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          SetSMBGuestPasswordResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetSMBGuestPassword where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.SetSMBGuestPassword" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetSMBGuestPassword where
  toJSON SetSMBGuestPassword' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("Password" Lude..= password)
          ]
      )

instance Lude.ToPath SetSMBGuestPassword where
  toPath = Lude.const "/"

instance Lude.ToQuery SetSMBGuestPassword where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetSMBGuestPasswordResponse' smart constructor.
data SetSMBGuestPasswordResponse = SetSMBGuestPasswordResponse'
  { gatewayARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'SetSMBGuestPasswordResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkSetSMBGuestPasswordResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetSMBGuestPasswordResponse
mkSetSMBGuestPasswordResponse pResponseStatus_ =
  SetSMBGuestPasswordResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmbgprsGatewayARN :: Lens.Lens' SetSMBGuestPasswordResponse (Lude.Maybe Lude.Text)
ssmbgprsGatewayARN = Lens.lens (gatewayARN :: SetSMBGuestPasswordResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: SetSMBGuestPasswordResponse)
{-# DEPRECATED ssmbgprsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmbgprsResponseStatus :: Lens.Lens' SetSMBGuestPasswordResponse Lude.Int
ssmbgprsResponseStatus = Lens.lens (responseStatus :: SetSMBGuestPasswordResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetSMBGuestPasswordResponse)
{-# DEPRECATED ssmbgprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
