{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ResetCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets all cache disks that have encountered an error and makes the disks available for reconfiguration as cache storage. If your cache disk encounters an error, the gateway prevents read and write operations on virtual tapes in the gateway. For example, an error can occur when a disk is corrupted or removed from the gateway. When a cache is reset, the gateway loses its cache storage. At this point, you can reconfigure the disks as cache disks. This operation is only supported in the cached volume and tape types.
--
-- /Important:/ If the cache disk you are resetting contains data that has not been uploaded to Amazon S3 yet, that data can be lost. After you reset cache disks, there will be no configured cache disks left in the gateway, so you must configure at least one new cache disk for your gateway to function properly.
module Network.AWS.StorageGateway.ResetCache
  ( -- * Creating a request
    ResetCache (..),
    mkResetCache,

    -- ** Request lenses
    rcGatewayARN,

    -- * Destructuring the response
    ResetCacheResponse (..),
    mkResetCacheResponse,

    -- ** Response lenses
    rrsGatewayARN,
    rrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkResetCache' smart constructor.
newtype ResetCache = ResetCache' {gatewayARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetCache' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
mkResetCache ::
  -- | 'gatewayARN'
  Lude.Text ->
  ResetCache
mkResetCache pGatewayARN_ = ResetCache' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcGatewayARN :: Lens.Lens' ResetCache Lude.Text
rcGatewayARN = Lens.lens (gatewayARN :: ResetCache -> Lude.Text) (\s a -> s {gatewayARN = a} :: ResetCache)
{-# DEPRECATED rcGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest ResetCache where
  type Rs ResetCache = ResetCacheResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          ResetCacheResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResetCache where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.ResetCache" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResetCache where
  toJSON ResetCache' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath ResetCache where
  toPath = Lude.const "/"

instance Lude.ToQuery ResetCache where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkResetCacheResponse' smart constructor.
data ResetCacheResponse = ResetCacheResponse'
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

-- | Creates a value of 'ResetCacheResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkResetCacheResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResetCacheResponse
mkResetCacheResponse pResponseStatus_ =
  ResetCacheResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsGatewayARN :: Lens.Lens' ResetCacheResponse (Lude.Maybe Lude.Text)
rrsGatewayARN = Lens.lens (gatewayARN :: ResetCacheResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: ResetCacheResponse)
{-# DEPRECATED rrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' ResetCacheResponse Lude.Int
rrsResponseStatus = Lens.lens (responseStatus :: ResetCacheResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResetCacheResponse)
{-# DEPRECATED rrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
