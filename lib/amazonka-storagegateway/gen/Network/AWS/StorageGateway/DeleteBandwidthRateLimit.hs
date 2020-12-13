{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dbrlBandwidthType,
    dbrlGatewayARN,

    -- * Destructuring the response
    DeleteBandwidthRateLimitResponse (..),
    mkDeleteBandwidthRateLimitResponse,

    -- ** Response lenses
    dbrlfrsGatewayARN,
    dbrlfrsResponseStatus,
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
  { -- | One of the BandwidthType values that indicates the gateway bandwidth rate limit to delete.
    --
    -- Valid Values: @UPLOAD@ | @DOWNLOAD@ | @ALL@
    bandwidthType :: Lude.Text,
    gatewayARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBandwidthRateLimit' with the minimum fields required to make a request.
--
-- * 'bandwidthType' - One of the BandwidthType values that indicates the gateway bandwidth rate limit to delete.
--
-- Valid Values: @UPLOAD@ | @DOWNLOAD@ | @ALL@
-- * 'gatewayARN' -
mkDeleteBandwidthRateLimit ::
  -- | 'bandwidthType'
  Lude.Text ->
  -- | 'gatewayARN'
  Lude.Text ->
  DeleteBandwidthRateLimit
mkDeleteBandwidthRateLimit pBandwidthType_ pGatewayARN_ =
  DeleteBandwidthRateLimit'
    { bandwidthType = pBandwidthType_,
      gatewayARN = pGatewayARN_
    }

-- | One of the BandwidthType values that indicates the gateway bandwidth rate limit to delete.
--
-- Valid Values: @UPLOAD@ | @DOWNLOAD@ | @ALL@
--
-- /Note:/ Consider using 'bandwidthType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlBandwidthType :: Lens.Lens' DeleteBandwidthRateLimit Lude.Text
dbrlBandwidthType = Lens.lens (bandwidthType :: DeleteBandwidthRateLimit -> Lude.Text) (\s a -> s {bandwidthType = a} :: DeleteBandwidthRateLimit)
{-# DEPRECATED dbrlBandwidthType "Use generic-lens or generic-optics with 'bandwidthType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlGatewayARN :: Lens.Lens' DeleteBandwidthRateLimit Lude.Text
dbrlGatewayARN = Lens.lens (gatewayARN :: DeleteBandwidthRateLimit -> Lude.Text) (\s a -> s {gatewayARN = a} :: DeleteBandwidthRateLimit)
{-# DEPRECATED dbrlGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

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
          [ Lude.Just ("BandwidthType" Lude..= bandwidthType),
            Lude.Just ("GatewayARN" Lude..= gatewayARN)
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
  { gatewayARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBandwidthRateLimitResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
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
dbrlfrsGatewayARN :: Lens.Lens' DeleteBandwidthRateLimitResponse (Lude.Maybe Lude.Text)
dbrlfrsGatewayARN = Lens.lens (gatewayARN :: DeleteBandwidthRateLimitResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: DeleteBandwidthRateLimitResponse)
{-# DEPRECATED dbrlfrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrlfrsResponseStatus :: Lens.Lens' DeleteBandwidthRateLimitResponse Lude.Int
dbrlfrsResponseStatus = Lens.lens (responseStatus :: DeleteBandwidthRateLimitResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteBandwidthRateLimitResponse)
{-# DEPRECATED dbrlfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
