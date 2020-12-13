{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.StartAccessLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts access logging on the specified container. When you enable access logging on a container, MediaStore delivers access logs for objects stored in that container to Amazon CloudWatch Logs.
module Network.AWS.MediaStore.StartAccessLogging
  ( -- * Creating a request
    StartAccessLogging (..),
    mkStartAccessLogging,

    -- ** Request lenses
    sContainerName,

    -- * Destructuring the response
    StartAccessLoggingResponse (..),
    mkStartAccessLoggingResponse,

    -- ** Response lenses
    salrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartAccessLogging' smart constructor.
newtype StartAccessLogging = StartAccessLogging'
  { -- | The name of the container that you want to start access logging on.
    containerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartAccessLogging' with the minimum fields required to make a request.
--
-- * 'containerName' - The name of the container that you want to start access logging on.
mkStartAccessLogging ::
  -- | 'containerName'
  Lude.Text ->
  StartAccessLogging
mkStartAccessLogging pContainerName_ =
  StartAccessLogging' {containerName = pContainerName_}

-- | The name of the container that you want to start access logging on.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sContainerName :: Lens.Lens' StartAccessLogging Lude.Text
sContainerName = Lens.lens (containerName :: StartAccessLogging -> Lude.Text) (\s a -> s {containerName = a} :: StartAccessLogging)
{-# DEPRECATED sContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Lude.AWSRequest StartAccessLogging where
  type Rs StartAccessLogging = StartAccessLoggingResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartAccessLoggingResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartAccessLogging where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.StartAccessLogging" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartAccessLogging where
  toJSON StartAccessLogging' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ContainerName" Lude..= containerName)]
      )

instance Lude.ToPath StartAccessLogging where
  toPath = Lude.const "/"

instance Lude.ToQuery StartAccessLogging where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartAccessLoggingResponse' smart constructor.
newtype StartAccessLoggingResponse = StartAccessLoggingResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartAccessLoggingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartAccessLoggingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartAccessLoggingResponse
mkStartAccessLoggingResponse pResponseStatus_ =
  StartAccessLoggingResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
salrsResponseStatus :: Lens.Lens' StartAccessLoggingResponse Lude.Int
salrsResponseStatus = Lens.lens (responseStatus :: StartAccessLoggingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartAccessLoggingResponse)
{-# DEPRECATED salrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
