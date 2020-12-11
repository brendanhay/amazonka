{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.AddUploadBuffer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures one or more gateway local disks as upload buffer for a specified gateway. This operation is supported for the stored volume, cached volume and tape gateway types.
--
-- In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add upload buffer, and one or more disk IDs that you want to configure as upload buffer.
module Network.AWS.StorageGateway.AddUploadBuffer
  ( -- * Creating a request
    AddUploadBuffer (..),
    mkAddUploadBuffer,

    -- ** Request lenses
    aubGatewayARN,
    aubDiskIds,

    -- * Destructuring the response
    AddUploadBufferResponse (..),
    mkAddUploadBufferResponse,

    -- ** Response lenses
    aubrsGatewayARN,
    aubrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkAddUploadBuffer' smart constructor.
data AddUploadBuffer = AddUploadBuffer'
  { gatewayARN :: Lude.Text,
    diskIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddUploadBuffer' with the minimum fields required to make a request.
--
-- * 'diskIds' - An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
-- * 'gatewayARN' - Undocumented field.
mkAddUploadBuffer ::
  -- | 'gatewayARN'
  Lude.Text ->
  AddUploadBuffer
mkAddUploadBuffer pGatewayARN_ =
  AddUploadBuffer'
    { gatewayARN = pGatewayARN_,
      diskIds = Lude.mempty
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aubGatewayARN :: Lens.Lens' AddUploadBuffer Lude.Text
aubGatewayARN = Lens.lens (gatewayARN :: AddUploadBuffer -> Lude.Text) (\s a -> s {gatewayARN = a} :: AddUploadBuffer)
{-# DEPRECATED aubGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
--
-- /Note:/ Consider using 'diskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aubDiskIds :: Lens.Lens' AddUploadBuffer [Lude.Text]
aubDiskIds = Lens.lens (diskIds :: AddUploadBuffer -> [Lude.Text]) (\s a -> s {diskIds = a} :: AddUploadBuffer)
{-# DEPRECATED aubDiskIds "Use generic-lens or generic-optics with 'diskIds' instead." #-}

instance Lude.AWSRequest AddUploadBuffer where
  type Rs AddUploadBuffer = AddUploadBufferResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          AddUploadBufferResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddUploadBuffer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.AddUploadBuffer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddUploadBuffer where
  toJSON AddUploadBuffer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("DiskIds" Lude..= diskIds)
          ]
      )

instance Lude.ToPath AddUploadBuffer where
  toPath = Lude.const "/"

instance Lude.ToQuery AddUploadBuffer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddUploadBufferResponse' smart constructor.
data AddUploadBufferResponse = AddUploadBufferResponse'
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

-- | Creates a value of 'AddUploadBufferResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkAddUploadBufferResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddUploadBufferResponse
mkAddUploadBufferResponse pResponseStatus_ =
  AddUploadBufferResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aubrsGatewayARN :: Lens.Lens' AddUploadBufferResponse (Lude.Maybe Lude.Text)
aubrsGatewayARN = Lens.lens (gatewayARN :: AddUploadBufferResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: AddUploadBufferResponse)
{-# DEPRECATED aubrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aubrsResponseStatus :: Lens.Lens' AddUploadBufferResponse Lude.Int
aubrsResponseStatus = Lens.lens (responseStatus :: AddUploadBufferResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddUploadBufferResponse)
{-# DEPRECATED aubrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
