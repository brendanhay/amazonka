{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.AddWorkingStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures one or more gateway local disks as working storage for a gateway. This operation is only supported in the stored volume gateway type. This operation is deprecated in cached volume API version 20120630. Use 'AddUploadBuffer' instead.
--
-- In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add working storage, and one or more disk IDs that you want to configure as working storage.
module Network.AWS.StorageGateway.AddWorkingStorage
  ( -- * Creating a request
    AddWorkingStorage (..),
    mkAddWorkingStorage,

    -- ** Request lenses
    awsGatewayARN,
    awsDiskIds,

    -- * Destructuring the response
    AddWorkingStorageResponse (..),
    mkAddWorkingStorageResponse,

    -- ** Response lenses
    awsrsGatewayARN,
    awsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
--
--     * 'AddWorkingStorageInput$DiskIds'
--
--
--
-- /See:/ 'mkAddWorkingStorage' smart constructor.
data AddWorkingStorage = AddWorkingStorage'
  { gatewayARN :: Lude.Text,
    -- | An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
    diskIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddWorkingStorage' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'diskIds' - An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
mkAddWorkingStorage ::
  -- | 'gatewayARN'
  Lude.Text ->
  AddWorkingStorage
mkAddWorkingStorage pGatewayARN_ =
  AddWorkingStorage'
    { gatewayARN = pGatewayARN_,
      diskIds = Lude.mempty
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awsGatewayARN :: Lens.Lens' AddWorkingStorage Lude.Text
awsGatewayARN = Lens.lens (gatewayARN :: AddWorkingStorage -> Lude.Text) (\s a -> s {gatewayARN = a} :: AddWorkingStorage)
{-# DEPRECATED awsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
--
-- /Note:/ Consider using 'diskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awsDiskIds :: Lens.Lens' AddWorkingStorage [Lude.Text]
awsDiskIds = Lens.lens (diskIds :: AddWorkingStorage -> [Lude.Text]) (\s a -> s {diskIds = a} :: AddWorkingStorage)
{-# DEPRECATED awsDiskIds "Use generic-lens or generic-optics with 'diskIds' instead." #-}

instance Lude.AWSRequest AddWorkingStorage where
  type Rs AddWorkingStorage = AddWorkingStorageResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          AddWorkingStorageResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddWorkingStorage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.AddWorkingStorage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddWorkingStorage where
  toJSON AddWorkingStorage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("DiskIds" Lude..= diskIds)
          ]
      )

instance Lude.ToPath AddWorkingStorage where
  toPath = Lude.const "/"

instance Lude.ToQuery AddWorkingStorage where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway for which working storage was configured.
--
-- /See:/ 'mkAddWorkingStorageResponse' smart constructor.
data AddWorkingStorageResponse = AddWorkingStorageResponse'
  { gatewayARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddWorkingStorageResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'responseStatus' - The response status code.
mkAddWorkingStorageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddWorkingStorageResponse
mkAddWorkingStorageResponse pResponseStatus_ =
  AddWorkingStorageResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awsrsGatewayARN :: Lens.Lens' AddWorkingStorageResponse (Lude.Maybe Lude.Text)
awsrsGatewayARN = Lens.lens (gatewayARN :: AddWorkingStorageResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: AddWorkingStorageResponse)
{-# DEPRECATED awsrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awsrsResponseStatus :: Lens.Lens' AddWorkingStorageResponse Lude.Int
awsrsResponseStatus = Lens.lens (responseStatus :: AddWorkingStorageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddWorkingStorageResponse)
{-# DEPRECATED awsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
