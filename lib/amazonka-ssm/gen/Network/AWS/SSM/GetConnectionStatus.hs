{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the Session Manager connection status for an instance to determine whether it is running and ready to receive Session Manager connections.
module Network.AWS.SSM.GetConnectionStatus
  ( -- * Creating a request
    GetConnectionStatus (..),
    mkGetConnectionStatus,

    -- ** Request lenses
    gcsTarget,

    -- * Destructuring the response
    GetConnectionStatusResponse (..),
    mkGetConnectionStatusResponse,

    -- ** Response lenses
    gcsrsStatus,
    gcsrsTarget,
    gcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetConnectionStatus' smart constructor.
newtype GetConnectionStatus = GetConnectionStatus'
  { -- | The ID of the instance.
    target :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConnectionStatus' with the minimum fields required to make a request.
--
-- * 'target' - The ID of the instance.
mkGetConnectionStatus ::
  -- | 'target'
  Lude.Text ->
  GetConnectionStatus
mkGetConnectionStatus pTarget_ =
  GetConnectionStatus' {target = pTarget_}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsTarget :: Lens.Lens' GetConnectionStatus Lude.Text
gcsTarget = Lens.lens (target :: GetConnectionStatus -> Lude.Text) (\s a -> s {target = a} :: GetConnectionStatus)
{-# DEPRECATED gcsTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Lude.AWSRequest GetConnectionStatus where
  type Rs GetConnectionStatus = GetConnectionStatusResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetConnectionStatusResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "Target")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetConnectionStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetConnectionStatus" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetConnectionStatus where
  toJSON GetConnectionStatus' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Target" Lude..= target)])

instance Lude.ToPath GetConnectionStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery GetConnectionStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetConnectionStatusResponse' smart constructor.
data GetConnectionStatusResponse = GetConnectionStatusResponse'
  { -- | The status of the connection to the instance. For example, 'Connected' or 'Not Connected'.
    status :: Lude.Maybe ConnectionStatus,
    -- | The ID of the instance to check connection status.
    target :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConnectionStatusResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the connection to the instance. For example, 'Connected' or 'Not Connected'.
-- * 'target' - The ID of the instance to check connection status.
-- * 'responseStatus' - The response status code.
mkGetConnectionStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetConnectionStatusResponse
mkGetConnectionStatusResponse pResponseStatus_ =
  GetConnectionStatusResponse'
    { status = Lude.Nothing,
      target = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the connection to the instance. For example, 'Connected' or 'Not Connected'.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsStatus :: Lens.Lens' GetConnectionStatusResponse (Lude.Maybe ConnectionStatus)
gcsrsStatus = Lens.lens (status :: GetConnectionStatusResponse -> Lude.Maybe ConnectionStatus) (\s a -> s {status = a} :: GetConnectionStatusResponse)
{-# DEPRECATED gcsrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the instance to check connection status.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsTarget :: Lens.Lens' GetConnectionStatusResponse (Lude.Maybe Lude.Text)
gcsrsTarget = Lens.lens (target :: GetConnectionStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {target = a} :: GetConnectionStatusResponse)
{-# DEPRECATED gcsrsTarget "Use generic-lens or generic-optics with 'target' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrsResponseStatus :: Lens.Lens' GetConnectionStatusResponse Lude.Int
gcsrsResponseStatus = Lens.lens (responseStatus :: GetConnectionStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetConnectionStatusResponse)
{-# DEPRECATED gcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
