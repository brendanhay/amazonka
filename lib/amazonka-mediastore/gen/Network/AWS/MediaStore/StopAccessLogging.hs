{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.StopAccessLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops access logging on the specified container. When you stop access logging on a container, MediaStore stops sending access logs to Amazon CloudWatch Logs. These access logs are not saved and are not retrievable.
module Network.AWS.MediaStore.StopAccessLogging
  ( -- * Creating a request
    StopAccessLogging (..),
    mkStopAccessLogging,

    -- ** Request lenses
    salContainerName,

    -- * Destructuring the response
    StopAccessLoggingResponse (..),
    mkStopAccessLoggingResponse,

    -- ** Response lenses
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopAccessLogging' smart constructor.
newtype StopAccessLogging = StopAccessLogging'
  { -- | The name of the container that you want to stop access logging on.
    containerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopAccessLogging' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container that you want to stop access logging on.
mkStopAccessLogging ::
  -- | 'containerName'
  Lude.Text ->
  StopAccessLogging
mkStopAccessLogging pContainerName_ =
  StopAccessLogging' {containerName = pContainerName_}

-- | The name of the container that you want to stop access logging on.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
salContainerName :: Lens.Lens' StopAccessLogging Lude.Text
salContainerName = Lens.lens (containerName :: StopAccessLogging -> Lude.Text) (\s a -> s {containerName = a} :: StopAccessLogging)
{-# DEPRECATED salContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Lude.AWSRequest StopAccessLogging where
  type Rs StopAccessLogging = StopAccessLoggingResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopAccessLoggingResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopAccessLogging where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.StopAccessLogging" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopAccessLogging where
  toJSON StopAccessLogging' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ContainerName" Lude..= containerName)]
      )

instance Lude.ToPath StopAccessLogging where
  toPath = Lude.const "/"

instance Lude.ToQuery StopAccessLogging where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopAccessLoggingResponse' smart constructor.
newtype StopAccessLoggingResponse = StopAccessLoggingResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopAccessLoggingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopAccessLoggingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopAccessLoggingResponse
mkStopAccessLoggingResponse pResponseStatus_ =
  StopAccessLoggingResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopAccessLoggingResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StopAccessLoggingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopAccessLoggingResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
