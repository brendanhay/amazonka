{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteRemoteAccessSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a completed remote access session and its results.
module Network.AWS.DeviceFarm.DeleteRemoteAccessSession
  ( -- * Creating a request
    DeleteRemoteAccessSession (..),
    mkDeleteRemoteAccessSession,

    -- ** Request lenses
    drasArn,

    -- * Destructuring the response
    DeleteRemoteAccessSessionResponse (..),
    mkDeleteRemoteAccessSessionResponse,

    -- ** Response lenses
    drasrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to delete the specified remote access session.
--
-- /See:/ 'mkDeleteRemoteAccessSession' smart constructor.
newtype DeleteRemoteAccessSession = DeleteRemoteAccessSession'
  { -- | The Amazon Resource Name (ARN) of the session for which you want to delete remote access.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRemoteAccessSession' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the session for which you want to delete remote access.
mkDeleteRemoteAccessSession ::
  -- | 'arn'
  Lude.Text ->
  DeleteRemoteAccessSession
mkDeleteRemoteAccessSession pArn_ =
  DeleteRemoteAccessSession' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the session for which you want to delete remote access.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drasArn :: Lens.Lens' DeleteRemoteAccessSession Lude.Text
drasArn = Lens.lens (arn :: DeleteRemoteAccessSession -> Lude.Text) (\s a -> s {arn = a} :: DeleteRemoteAccessSession)
{-# DEPRECATED drasArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest DeleteRemoteAccessSession where
  type
    Rs DeleteRemoteAccessSession =
      DeleteRemoteAccessSessionResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteRemoteAccessSessionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRemoteAccessSession where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DeviceFarm_20150623.DeleteRemoteAccessSession" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRemoteAccessSession where
  toJSON DeleteRemoteAccessSession' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath DeleteRemoteAccessSession where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRemoteAccessSession where
  toQuery = Lude.const Lude.mempty

-- | The response from the server when a request is made to delete the remote access session.
--
-- /See:/ 'mkDeleteRemoteAccessSessionResponse' smart constructor.
newtype DeleteRemoteAccessSessionResponse = DeleteRemoteAccessSessionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRemoteAccessSessionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteRemoteAccessSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRemoteAccessSessionResponse
mkDeleteRemoteAccessSessionResponse pResponseStatus_ =
  DeleteRemoteAccessSessionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drasrsResponseStatus :: Lens.Lens' DeleteRemoteAccessSessionResponse Lude.Int
drasrsResponseStatus = Lens.lens (responseStatus :: DeleteRemoteAccessSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRemoteAccessSessionResponse)
{-# DEPRECATED drasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
