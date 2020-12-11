{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetRemoteAccessSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a link to a currently running remote access session.
module Network.AWS.DeviceFarm.GetRemoteAccessSession
  ( -- * Creating a request
    GetRemoteAccessSession (..),
    mkGetRemoteAccessSession,

    -- ** Request lenses
    grasArn,

    -- * Destructuring the response
    GetRemoteAccessSessionResponse (..),
    mkGetRemoteAccessSessionResponse,

    -- ** Response lenses
    grasrsRemoteAccessSession,
    grasrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to get information about the specified remote access session.
--
-- /See:/ 'mkGetRemoteAccessSession' smart constructor.
newtype GetRemoteAccessSession = GetRemoteAccessSession'
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

-- | Creates a value of 'GetRemoteAccessSession' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the remote access session about which you want to get session information.
mkGetRemoteAccessSession ::
  -- | 'arn'
  Lude.Text ->
  GetRemoteAccessSession
mkGetRemoteAccessSession pArn_ =
  GetRemoteAccessSession' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the remote access session about which you want to get session information.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grasArn :: Lens.Lens' GetRemoteAccessSession Lude.Text
grasArn = Lens.lens (arn :: GetRemoteAccessSession -> Lude.Text) (\s a -> s {arn = a} :: GetRemoteAccessSession)
{-# DEPRECATED grasArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest GetRemoteAccessSession where
  type Rs GetRemoteAccessSession = GetRemoteAccessSessionResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRemoteAccessSessionResponse'
            Lude.<$> (x Lude..?> "remoteAccessSession")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRemoteAccessSession where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.GetRemoteAccessSession" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRemoteAccessSession where
  toJSON GetRemoteAccessSession' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath GetRemoteAccessSession where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRemoteAccessSession where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server that lists detailed information about the remote access session.
--
-- /See:/ 'mkGetRemoteAccessSessionResponse' smart constructor.
data GetRemoteAccessSessionResponse = GetRemoteAccessSessionResponse'
  { remoteAccessSession ::
      Lude.Maybe
        RemoteAccessSession,
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

-- | Creates a value of 'GetRemoteAccessSessionResponse' with the minimum fields required to make a request.
--
-- * 'remoteAccessSession' - A container that lists detailed information about the remote access session.
-- * 'responseStatus' - The response status code.
mkGetRemoteAccessSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRemoteAccessSessionResponse
mkGetRemoteAccessSessionResponse pResponseStatus_ =
  GetRemoteAccessSessionResponse'
    { remoteAccessSession =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A container that lists detailed information about the remote access session.
--
-- /Note:/ Consider using 'remoteAccessSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grasrsRemoteAccessSession :: Lens.Lens' GetRemoteAccessSessionResponse (Lude.Maybe RemoteAccessSession)
grasrsRemoteAccessSession = Lens.lens (remoteAccessSession :: GetRemoteAccessSessionResponse -> Lude.Maybe RemoteAccessSession) (\s a -> s {remoteAccessSession = a} :: GetRemoteAccessSessionResponse)
{-# DEPRECATED grasrsRemoteAccessSession "Use generic-lens or generic-optics with 'remoteAccessSession' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grasrsResponseStatus :: Lens.Lens' GetRemoteAccessSessionResponse Lude.Int
grasrsResponseStatus = Lens.lens (responseStatus :: GetRemoteAccessSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRemoteAccessSessionResponse)
{-# DEPRECATED grasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
