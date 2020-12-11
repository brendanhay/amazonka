{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteBandwidthRateLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the bandwidth rate limits of a gateway. You can delete either the upload and download bandwidth rate limit, or you can delete both. If you delete only one of the limits, the other limit remains unchanged. To specify which gateway to work with, use the Amazon Resource Name (ARN) of the gateway in your request. This operation is supported for the stored volume, cached volume and tape gateway types.
module Network.AWS.StorageGateway.DeleteBandwidthRateLimit
  ( -- * Creating a request
    DeleteBandwidthRateLimit (..),
    mkDeleteBandwidthRateLimit,

    -- ** Request lenses
    dbrlbGatewayARN,
    dbrlbBandwidthType,

    -- * Destructuring the response
    DeleteBandwidthRateLimitResponse (..),
    mkDeleteBandwidthRateLimitResponse,

    -- ** Response lenses
    delrsGatewayARN,
    delrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the following fields:
--
--
--     * 'DeleteBandwidthRateLimitInput$BandwidthType'
--
--
--
-- /See:/ 'mkDeleteBandwidthRateLimit' smart constructor.
data DeleteBandwidthRateLimit = DeleteBandwidthRateLimit'
  { gatewayARN ::
      Lude.Text,
    bandwidthType :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBandwidthRateLimit' with the minimum fields required to make a request.
--
-- * 'bandwidthType' - One of the BandwidthType values that indicates the gateway bandwidth rate limit to delete.
--
-- Valid Values: @UPLOAD@ | @DOWNLOAD@ | @ALL@
-- * 'gatewayARN' - Undocumented field.
mkDeleteBandwidthRateLimit ::
  -- | 'gatewayARN'
  Lude.Text ->
  -- | 'bandwidthType'
  Lude.Text ->
  DeleteBandwidthRateLimit
mkDeleteBandwidthRateLimit pGatewayARN_ pBandwidthType_ =
  DeleteBandwidthRateLimit'
    { gatewayARN = pGatewayARN_,
      bandwidthType = pBandwidthType_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlbGatewayARN :: Lens.Lens' DeleteBandwidthRateLimit Lude.Text
dbrlbGatewayARN = Lens.lens (gatewayARN :: DeleteBandwidthRateLimit -> Lude.Text) (\s a -> s {gatewayARN = a} :: DeleteBandwidthRateLimit)
{-# DEPRECATED dbrlbGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | One of the BandwidthType values that indicates the gateway bandwidth rate limit to delete.
--
-- Valid Values: @UPLOAD@ | @DOWNLOAD@ | @ALL@
--
-- /Note:/ Consider using 'bandwidthType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlbBandwidthType :: Lens.Lens' DeleteBandwidthRateLimit Lude.Text
dbrlbBandwidthType = Lens.lens (bandwidthType :: DeleteBandwidthRateLimit -> Lude.Text) (\s a -> s {bandwidthType = a} :: DeleteBandwidthRateLimit)
{-# DEPRECATED dbrlbBandwidthType "Use generic-lens or generic-optics with 'bandwidthType' instead." #-}

instance Lude.AWSRequest DeleteBandwidthRateLimit where
  type Rs DeleteBandwidthRateLimit = DeleteBandwidthRateLimitResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteBandwidthRateLimitResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteBandwidthRateLimit where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DeleteBandwidthRateLimit" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteBandwidthRateLimit where
  toJSON DeleteBandwidthRateLimit' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("BandwidthType" Lude..= bandwidthType)
          ]
      )

instance Lude.ToPath DeleteBandwidthRateLimit where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteBandwidthRateLimit where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway whose bandwidth rate information was deleted.
--
-- /See:/ 'mkDeleteBandwidthRateLimitResponse' smart constructor.
data DeleteBandwidthRateLimitResponse = DeleteBandwidthRateLimitResponse'
  { gatewayARN ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBandwidthRateLimitResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteBandwidthRateLimitResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteBandwidthRateLimitResponse
mkDeleteBandwidthRateLimitResponse pResponseStatus_ =
  DeleteBandwidthRateLimitResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsGatewayARN :: Lens.Lens' DeleteBandwidthRateLimitResponse (Lude.Maybe Lude.Text)
delrsGatewayARN = Lens.lens (gatewayARN :: DeleteBandwidthRateLimitResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: DeleteBandwidthRateLimitResponse)
{-# DEPRECATED delrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsResponseStatus :: Lens.Lens' DeleteBandwidthRateLimitResponse Lude.Int
delrsResponseStatus = Lens.lens (responseStatus :: DeleteBandwidthRateLimitResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteBandwidthRateLimitResponse)
{-# DEPRECATED delrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
