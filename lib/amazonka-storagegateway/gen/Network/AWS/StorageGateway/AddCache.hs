{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.AddCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures one or more gateway local disks as cache for a gateway. This operation is only supported in the cached volume, tape, and file gateway type (see <https://docs.aws.amazon.com/storagegateway/latest/userguide/StorageGatewayConcepts.html How AWS Storage Gateway works (architecture)> .
--
-- In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add cache, and one or more disk IDs that you want to configure as cache.
module Network.AWS.StorageGateway.AddCache
  ( -- * Creating a request
    AddCache (..),
    mkAddCache,

    -- ** Request lenses
    acGatewayARN,
    acDiskIds,

    -- * Destructuring the response
    AddCacheResponse (..),
    mkAddCacheResponse,

    -- ** Response lenses
    acrsGatewayARN,
    acrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkAddCache' smart constructor.
data AddCache = AddCache'
  { gatewayARN :: Lude.Text,
    -- | An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
    diskIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddCache' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'diskIds' - An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
mkAddCache ::
  -- | 'gatewayARN'
  Lude.Text ->
  AddCache
mkAddCache pGatewayARN_ =
  AddCache' {gatewayARN = pGatewayARN_, diskIds = Lude.mempty}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acGatewayARN :: Lens.Lens' AddCache Lude.Text
acGatewayARN = Lens.lens (gatewayARN :: AddCache -> Lude.Text) (\s a -> s {gatewayARN = a} :: AddCache)
{-# DEPRECATED acGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | An array of strings that identify disks that are to be configured as working storage. Each string has a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
--
-- /Note:/ Consider using 'diskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acDiskIds :: Lens.Lens' AddCache [Lude.Text]
acDiskIds = Lens.lens (diskIds :: AddCache -> [Lude.Text]) (\s a -> s {diskIds = a} :: AddCache)
{-# DEPRECATED acDiskIds "Use generic-lens or generic-optics with 'diskIds' instead." #-}

instance Lude.AWSRequest AddCache where
  type Rs AddCache = AddCacheResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          AddCacheResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddCache where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.AddCache" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddCache where
  toJSON AddCache' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("DiskIds" Lude..= diskIds)
          ]
      )

instance Lude.ToPath AddCache where
  toPath = Lude.const "/"

instance Lude.ToQuery AddCache where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddCacheResponse' smart constructor.
data AddCacheResponse = AddCacheResponse'
  { gatewayARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddCacheResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'responseStatus' - The response status code.
mkAddCacheResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddCacheResponse
mkAddCacheResponse pResponseStatus_ =
  AddCacheResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrsGatewayARN :: Lens.Lens' AddCacheResponse (Lude.Maybe Lude.Text)
acrsGatewayARN = Lens.lens (gatewayARN :: AddCacheResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: AddCacheResponse)
{-# DEPRECATED acrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acrsResponseStatus :: Lens.Lens' AddCacheResponse Lude.Int
acrsResponseStatus = Lens.lens (responseStatus :: AddCacheResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddCacheResponse)
{-# DEPRECATED acrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
