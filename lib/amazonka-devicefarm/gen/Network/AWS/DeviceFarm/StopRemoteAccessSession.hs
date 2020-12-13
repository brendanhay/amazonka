{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.StopRemoteAccessSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Ends a specified remote access session.
module Network.AWS.DeviceFarm.StopRemoteAccessSession
  ( -- * Creating a request
    StopRemoteAccessSession (..),
    mkStopRemoteAccessSession,

    -- ** Request lenses
    srasArn,

    -- * Destructuring the response
    StopRemoteAccessSessionResponse (..),
    mkStopRemoteAccessSessionResponse,

    -- ** Response lenses
    srasrsRemoteAccessSession,
    srasrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to stop the remote access session.
--
-- /See:/ 'mkStopRemoteAccessSession' smart constructor.
newtype StopRemoteAccessSession = StopRemoteAccessSession'
  { -- | The Amazon Resource Name (ARN) of the remote access session to stop.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopRemoteAccessSession' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the remote access session to stop.
mkStopRemoteAccessSession ::
  -- | 'arn'
  Lude.Text ->
  StopRemoteAccessSession
mkStopRemoteAccessSession pArn_ =
  StopRemoteAccessSession' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the remote access session to stop.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srasArn :: Lens.Lens' StopRemoteAccessSession Lude.Text
srasArn = Lens.lens (arn :: StopRemoteAccessSession -> Lude.Text) (\s a -> s {arn = a} :: StopRemoteAccessSession)
{-# DEPRECATED srasArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest StopRemoteAccessSession where
  type Rs StopRemoteAccessSession = StopRemoteAccessSessionResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopRemoteAccessSessionResponse'
            Lude.<$> (x Lude..?> "remoteAccessSession")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopRemoteAccessSession where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.StopRemoteAccessSession" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopRemoteAccessSession where
  toJSON StopRemoteAccessSession' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath StopRemoteAccessSession where
  toPath = Lude.const "/"

instance Lude.ToQuery StopRemoteAccessSession where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server that describes the remote access session when AWS Device Farm stops the session.
--
-- /See:/ 'mkStopRemoteAccessSessionResponse' smart constructor.
data StopRemoteAccessSessionResponse = StopRemoteAccessSessionResponse'
  { -- | A container that represents the metadata from the service about the remote access session you are stopping.
    remoteAccessSession :: Lude.Maybe RemoteAccessSession,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopRemoteAccessSessionResponse' with the minimum fields required to make a request.
--
-- * 'remoteAccessSession' - A container that represents the metadata from the service about the remote access session you are stopping.
-- * 'responseStatus' - The response status code.
mkStopRemoteAccessSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopRemoteAccessSessionResponse
mkStopRemoteAccessSessionResponse pResponseStatus_ =
  StopRemoteAccessSessionResponse'
    { remoteAccessSession =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A container that represents the metadata from the service about the remote access session you are stopping.
--
-- /Note:/ Consider using 'remoteAccessSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srasrsRemoteAccessSession :: Lens.Lens' StopRemoteAccessSessionResponse (Lude.Maybe RemoteAccessSession)
srasrsRemoteAccessSession = Lens.lens (remoteAccessSession :: StopRemoteAccessSessionResponse -> Lude.Maybe RemoteAccessSession) (\s a -> s {remoteAccessSession = a} :: StopRemoteAccessSessionResponse)
{-# DEPRECATED srasrsRemoteAccessSession "Use generic-lens or generic-optics with 'remoteAccessSession' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srasrsResponseStatus :: Lens.Lens' StopRemoteAccessSessionResponse Lude.Int
srasrsResponseStatus = Lens.lens (responseStatus :: StopRemoteAccessSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopRemoteAccessSessionResponse)
{-# DEPRECATED srasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
