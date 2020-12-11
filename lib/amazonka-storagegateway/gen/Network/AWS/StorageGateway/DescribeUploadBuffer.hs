{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeUploadBuffer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the upload buffer of a gateway. This operation is supported for the stored volume, cached volume, and tape gateway types.
--
-- The response includes disk IDs that are configured as upload buffer space, and it includes the amount of upload buffer space allocated and used.
module Network.AWS.StorageGateway.DescribeUploadBuffer
  ( -- * Creating a request
    DescribeUploadBuffer (..),
    mkDescribeUploadBuffer,

    -- ** Request lenses
    dubGatewayARN,

    -- * Destructuring the response
    DescribeUploadBufferResponse (..),
    mkDescribeUploadBufferResponse,

    -- ** Response lenses
    dubrsUploadBufferAllocatedInBytes,
    dubrsGatewayARN,
    dubrsDiskIds,
    dubrsUploadBufferUsedInBytes,
    dubrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkDescribeUploadBuffer' smart constructor.
newtype DescribeUploadBuffer = DescribeUploadBuffer'
  { gatewayARN ::
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

-- | Creates a value of 'DescribeUploadBuffer' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
mkDescribeUploadBuffer ::
  -- | 'gatewayARN'
  Lude.Text ->
  DescribeUploadBuffer
mkDescribeUploadBuffer pGatewayARN_ =
  DescribeUploadBuffer' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dubGatewayARN :: Lens.Lens' DescribeUploadBuffer Lude.Text
dubGatewayARN = Lens.lens (gatewayARN :: DescribeUploadBuffer -> Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeUploadBuffer)
{-# DEPRECATED dubGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest DescribeUploadBuffer where
  type Rs DescribeUploadBuffer = DescribeUploadBufferResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUploadBufferResponse'
            Lude.<$> (x Lude..?> "UploadBufferAllocatedInBytes")
            Lude.<*> (x Lude..?> "GatewayARN")
            Lude.<*> (x Lude..?> "DiskIds" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "UploadBufferUsedInBytes")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUploadBuffer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DescribeUploadBuffer" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeUploadBuffer where
  toJSON DescribeUploadBuffer' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath DescribeUploadBuffer where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeUploadBuffer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeUploadBufferResponse' smart constructor.
data DescribeUploadBufferResponse = DescribeUploadBufferResponse'
  { uploadBufferAllocatedInBytes ::
      Lude.Maybe Lude.Integer,
    gatewayARN ::
      Lude.Maybe Lude.Text,
    diskIds :: Lude.Maybe [Lude.Text],
    uploadBufferUsedInBytes ::
      Lude.Maybe Lude.Integer,
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

-- | Creates a value of 'DescribeUploadBufferResponse' with the minimum fields required to make a request.
--
-- * 'diskIds' - An array of the gateway's local disk IDs that are configured as working storage. Each local disk ID is specified as a string (minimum length of 1 and maximum length of 300). If no local disks are configured as working storage, then the DiskIds array is empty.
-- * 'gatewayARN' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'uploadBufferAllocatedInBytes' - The total number of bytes allocated in the gateway's as upload buffer.
-- * 'uploadBufferUsedInBytes' - The total number of bytes being used in the gateway's upload buffer.
mkDescribeUploadBufferResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUploadBufferResponse
mkDescribeUploadBufferResponse pResponseStatus_ =
  DescribeUploadBufferResponse'
    { uploadBufferAllocatedInBytes =
        Lude.Nothing,
      gatewayARN = Lude.Nothing,
      diskIds = Lude.Nothing,
      uploadBufferUsedInBytes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The total number of bytes allocated in the gateway's as upload buffer.
--
-- /Note:/ Consider using 'uploadBufferAllocatedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dubrsUploadBufferAllocatedInBytes :: Lens.Lens' DescribeUploadBufferResponse (Lude.Maybe Lude.Integer)
dubrsUploadBufferAllocatedInBytes = Lens.lens (uploadBufferAllocatedInBytes :: DescribeUploadBufferResponse -> Lude.Maybe Lude.Integer) (\s a -> s {uploadBufferAllocatedInBytes = a} :: DescribeUploadBufferResponse)
{-# DEPRECATED dubrsUploadBufferAllocatedInBytes "Use generic-lens or generic-optics with 'uploadBufferAllocatedInBytes' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dubrsGatewayARN :: Lens.Lens' DescribeUploadBufferResponse (Lude.Maybe Lude.Text)
dubrsGatewayARN = Lens.lens (gatewayARN :: DescribeUploadBufferResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeUploadBufferResponse)
{-# DEPRECATED dubrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | An array of the gateway's local disk IDs that are configured as working storage. Each local disk ID is specified as a string (minimum length of 1 and maximum length of 300). If no local disks are configured as working storage, then the DiskIds array is empty.
--
-- /Note:/ Consider using 'diskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dubrsDiskIds :: Lens.Lens' DescribeUploadBufferResponse (Lude.Maybe [Lude.Text])
dubrsDiskIds = Lens.lens (diskIds :: DescribeUploadBufferResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {diskIds = a} :: DescribeUploadBufferResponse)
{-# DEPRECATED dubrsDiskIds "Use generic-lens or generic-optics with 'diskIds' instead." #-}

-- | The total number of bytes being used in the gateway's upload buffer.
--
-- /Note:/ Consider using 'uploadBufferUsedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dubrsUploadBufferUsedInBytes :: Lens.Lens' DescribeUploadBufferResponse (Lude.Maybe Lude.Integer)
dubrsUploadBufferUsedInBytes = Lens.lens (uploadBufferUsedInBytes :: DescribeUploadBufferResponse -> Lude.Maybe Lude.Integer) (\s a -> s {uploadBufferUsedInBytes = a} :: DescribeUploadBufferResponse)
{-# DEPRECATED dubrsUploadBufferUsedInBytes "Use generic-lens or generic-optics with 'uploadBufferUsedInBytes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dubrsResponseStatus :: Lens.Lens' DescribeUploadBufferResponse Lude.Int
dubrsResponseStatus = Lens.lens (responseStatus :: DescribeUploadBufferResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUploadBufferResponse)
{-# DEPRECATED dubrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
