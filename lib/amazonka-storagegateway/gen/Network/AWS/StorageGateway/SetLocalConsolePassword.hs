{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.SetLocalConsolePassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the password for your VM local console. When you log in to the local console for the first time, you log in to the VM with the default credentials. We recommend that you set a new password. You don't need to know the default password to set a new password.
module Network.AWS.StorageGateway.SetLocalConsolePassword
  ( -- * Creating a request
    SetLocalConsolePassword (..),
    mkSetLocalConsolePassword,

    -- ** Request lenses
    slcpLocalConsolePassword,
    slcpGatewayARN,

    -- * Destructuring the response
    SetLocalConsolePasswordResponse (..),
    mkSetLocalConsolePasswordResponse,

    -- ** Response lenses
    slcprsGatewayARN,
    slcprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | SetLocalConsolePasswordInput
--
-- /See:/ 'mkSetLocalConsolePassword' smart constructor.
data SetLocalConsolePassword = SetLocalConsolePassword'
  { -- | The password you want to set for your VM local console.
    localConsolePassword :: Lude.Sensitive Lude.Text,
    gatewayARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetLocalConsolePassword' with the minimum fields required to make a request.
--
-- * 'localConsolePassword' - The password you want to set for your VM local console.
-- * 'gatewayARN' -
mkSetLocalConsolePassword ::
  -- | 'localConsolePassword'
  Lude.Sensitive Lude.Text ->
  -- | 'gatewayARN'
  Lude.Text ->
  SetLocalConsolePassword
mkSetLocalConsolePassword pLocalConsolePassword_ pGatewayARN_ =
  SetLocalConsolePassword'
    { localConsolePassword =
        pLocalConsolePassword_,
      gatewayARN = pGatewayARN_
    }

-- | The password you want to set for your VM local console.
--
-- /Note:/ Consider using 'localConsolePassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcpLocalConsolePassword :: Lens.Lens' SetLocalConsolePassword (Lude.Sensitive Lude.Text)
slcpLocalConsolePassword = Lens.lens (localConsolePassword :: SetLocalConsolePassword -> Lude.Sensitive Lude.Text) (\s a -> s {localConsolePassword = a} :: SetLocalConsolePassword)
{-# DEPRECATED slcpLocalConsolePassword "Use generic-lens or generic-optics with 'localConsolePassword' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcpGatewayARN :: Lens.Lens' SetLocalConsolePassword Lude.Text
slcpGatewayARN = Lens.lens (gatewayARN :: SetLocalConsolePassword -> Lude.Text) (\s a -> s {gatewayARN = a} :: SetLocalConsolePassword)
{-# DEPRECATED slcpGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest SetLocalConsolePassword where
  type Rs SetLocalConsolePassword = SetLocalConsolePasswordResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          SetLocalConsolePasswordResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetLocalConsolePassword where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.SetLocalConsolePassword" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetLocalConsolePassword where
  toJSON SetLocalConsolePassword' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("LocalConsolePassword" Lude..= localConsolePassword),
            Lude.Just ("GatewayARN" Lude..= gatewayARN)
          ]
      )

instance Lude.ToPath SetLocalConsolePassword where
  toPath = Lude.const "/"

instance Lude.ToQuery SetLocalConsolePassword where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetLocalConsolePasswordResponse' smart constructor.
data SetLocalConsolePasswordResponse = SetLocalConsolePasswordResponse'
  { gatewayARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetLocalConsolePasswordResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'responseStatus' - The response status code.
mkSetLocalConsolePasswordResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetLocalConsolePasswordResponse
mkSetLocalConsolePasswordResponse pResponseStatus_ =
  SetLocalConsolePasswordResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcprsGatewayARN :: Lens.Lens' SetLocalConsolePasswordResponse (Lude.Maybe Lude.Text)
slcprsGatewayARN = Lens.lens (gatewayARN :: SetLocalConsolePasswordResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: SetLocalConsolePasswordResponse)
{-# DEPRECATED slcprsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcprsResponseStatus :: Lens.Lens' SetLocalConsolePasswordResponse Lude.Int
slcprsResponseStatus = Lens.lens (responseStatus :: SetLocalConsolePasswordResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetLocalConsolePasswordResponse)
{-# DEPRECATED slcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
