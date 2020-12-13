{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeWorkingStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the working storage of a gateway. This operation is only supported in the stored volumes gateway type. This operation is deprecated in cached volumes API version (20120630). Use DescribeUploadBuffer instead.
--
-- The response includes disk IDs that are configured as working storage, and it includes the amount of working storage allocated and used.
module Network.AWS.StorageGateway.DescribeWorkingStorage
  ( -- * Creating a request
    DescribeWorkingStorage (..),
    mkDescribeWorkingStorage,

    -- ** Request lenses
    dwsGatewayARN,

    -- * Destructuring the response
    DescribeWorkingStorageResponse (..),
    mkDescribeWorkingStorageResponse,

    -- ** Response lenses
    dwsrsGatewayARN,
    dwsrsDiskIds,
    dwsrsWorkingStorageAllocatedInBytes,
    dwsrsWorkingStorageUsedInBytes,
    dwsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway.
--
-- /See:/ 'mkDescribeWorkingStorage' smart constructor.
newtype DescribeWorkingStorage = DescribeWorkingStorage'
  { gatewayARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkingStorage' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
mkDescribeWorkingStorage ::
  -- | 'gatewayARN'
  Lude.Text ->
  DescribeWorkingStorage
mkDescribeWorkingStorage pGatewayARN_ =
  DescribeWorkingStorage' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsGatewayARN :: Lens.Lens' DescribeWorkingStorage Lude.Text
dwsGatewayARN = Lens.lens (gatewayARN :: DescribeWorkingStorage -> Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeWorkingStorage)
{-# DEPRECATED dwsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest DescribeWorkingStorage where
  type Rs DescribeWorkingStorage = DescribeWorkingStorageResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeWorkingStorageResponse'
            Lude.<$> (x Lude..?> "GatewayARN")
            Lude.<*> (x Lude..?> "DiskIds" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "WorkingStorageAllocatedInBytes")
            Lude.<*> (x Lude..?> "WorkingStorageUsedInBytes")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeWorkingStorage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DescribeWorkingStorage" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeWorkingStorage where
  toJSON DescribeWorkingStorage' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath DescribeWorkingStorage where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeWorkingStorage where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkDescribeWorkingStorageResponse' smart constructor.
data DescribeWorkingStorageResponse = DescribeWorkingStorageResponse'
  { gatewayARN :: Lude.Maybe Lude.Text,
    -- | An array of the gateway's local disk IDs that are configured as working storage. Each local disk ID is specified as a string (minimum length of 1 and maximum length of 300). If no local disks are configured as working storage, then the DiskIds array is empty.
    diskIds :: Lude.Maybe [Lude.Text],
    -- | The total working storage in bytes allocated for the gateway. If no working storage is configured for the gateway, this field returns 0.
    workingStorageAllocatedInBytes :: Lude.Maybe Lude.Integer,
    -- | The total working storage in bytes in use by the gateway. If no working storage is configured for the gateway, this field returns 0.
    workingStorageUsedInBytes :: Lude.Maybe Lude.Integer,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkingStorageResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'diskIds' - An array of the gateway's local disk IDs that are configured as working storage. Each local disk ID is specified as a string (minimum length of 1 and maximum length of 300). If no local disks are configured as working storage, then the DiskIds array is empty.
-- * 'workingStorageAllocatedInBytes' - The total working storage in bytes allocated for the gateway. If no working storage is configured for the gateway, this field returns 0.
-- * 'workingStorageUsedInBytes' - The total working storage in bytes in use by the gateway. If no working storage is configured for the gateway, this field returns 0.
-- * 'responseStatus' - The response status code.
mkDescribeWorkingStorageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeWorkingStorageResponse
mkDescribeWorkingStorageResponse pResponseStatus_ =
  DescribeWorkingStorageResponse'
    { gatewayARN = Lude.Nothing,
      diskIds = Lude.Nothing,
      workingStorageAllocatedInBytes = Lude.Nothing,
      workingStorageUsedInBytes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrsGatewayARN :: Lens.Lens' DescribeWorkingStorageResponse (Lude.Maybe Lude.Text)
dwsrsGatewayARN = Lens.lens (gatewayARN :: DescribeWorkingStorageResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeWorkingStorageResponse)
{-# DEPRECATED dwsrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | An array of the gateway's local disk IDs that are configured as working storage. Each local disk ID is specified as a string (minimum length of 1 and maximum length of 300). If no local disks are configured as working storage, then the DiskIds array is empty.
--
-- /Note:/ Consider using 'diskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrsDiskIds :: Lens.Lens' DescribeWorkingStorageResponse (Lude.Maybe [Lude.Text])
dwsrsDiskIds = Lens.lens (diskIds :: DescribeWorkingStorageResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {diskIds = a} :: DescribeWorkingStorageResponse)
{-# DEPRECATED dwsrsDiskIds "Use generic-lens or generic-optics with 'diskIds' instead." #-}

-- | The total working storage in bytes allocated for the gateway. If no working storage is configured for the gateway, this field returns 0.
--
-- /Note:/ Consider using 'workingStorageAllocatedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrsWorkingStorageAllocatedInBytes :: Lens.Lens' DescribeWorkingStorageResponse (Lude.Maybe Lude.Integer)
dwsrsWorkingStorageAllocatedInBytes = Lens.lens (workingStorageAllocatedInBytes :: DescribeWorkingStorageResponse -> Lude.Maybe Lude.Integer) (\s a -> s {workingStorageAllocatedInBytes = a} :: DescribeWorkingStorageResponse)
{-# DEPRECATED dwsrsWorkingStorageAllocatedInBytes "Use generic-lens or generic-optics with 'workingStorageAllocatedInBytes' instead." #-}

-- | The total working storage in bytes in use by the gateway. If no working storage is configured for the gateway, this field returns 0.
--
-- /Note:/ Consider using 'workingStorageUsedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrsWorkingStorageUsedInBytes :: Lens.Lens' DescribeWorkingStorageResponse (Lude.Maybe Lude.Integer)
dwsrsWorkingStorageUsedInBytes = Lens.lens (workingStorageUsedInBytes :: DescribeWorkingStorageResponse -> Lude.Maybe Lude.Integer) (\s a -> s {workingStorageUsedInBytes = a} :: DescribeWorkingStorageResponse)
{-# DEPRECATED dwsrsWorkingStorageUsedInBytes "Use generic-lens or generic-optics with 'workingStorageUsedInBytes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrsResponseStatus :: Lens.Lens' DescribeWorkingStorageResponse Lude.Int
dwsrsResponseStatus = Lens.lens (responseStatus :: DescribeWorkingStorageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeWorkingStorageResponse)
{-# DEPRECATED dwsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
