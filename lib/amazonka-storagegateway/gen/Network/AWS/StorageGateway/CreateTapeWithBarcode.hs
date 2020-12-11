{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateTapeWithBarcode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a virtual tape by using your own barcode. You write data to the virtual tape and then archive the tape. A barcode is unique and cannot be reused if it has already been used on a tape. This applies to barcodes used on deleted tapes. This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.CreateTapeWithBarcode
  ( -- * Creating a request
    CreateTapeWithBarcode (..),
    mkCreateTapeWithBarcode,

    -- ** Request lenses
    ctwbKMSKey,
    ctwbKMSEncrypted,
    ctwbPoolId,
    ctwbWorm,
    ctwbTags,
    ctwbGatewayARN,
    ctwbTapeSizeInBytes,
    ctwbTapeBarcode,

    -- * Destructuring the response
    CreateTapeWithBarcodeResponse (..),
    mkCreateTapeWithBarcodeResponse,

    -- ** Response lenses
    ctwbrsTapeARN,
    ctwbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | CreateTapeWithBarcodeInput
--
-- /See:/ 'mkCreateTapeWithBarcode' smart constructor.
data CreateTapeWithBarcode = CreateTapeWithBarcode'
  { kmsKey ::
      Lude.Maybe Lude.Text,
    kmsEncrypted :: Lude.Maybe Lude.Bool,
    poolId :: Lude.Maybe Lude.Text,
    worm :: Lude.Maybe Lude.Bool,
    tags :: Lude.Maybe [Tag],
    gatewayARN :: Lude.Text,
    tapeSizeInBytes :: Lude.Integer,
    tapeBarcode :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTapeWithBarcode' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - The unique Amazon Resource Name (ARN) that represents the gateway to associate the virtual tape with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
-- * 'kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
-- * 'kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
-- * 'poolId' - The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
-- * 'tags' - A list of up to 50 tags that can be assigned to a virtual tape that has a barcode. Each tag is a key-value pair.
-- * 'tapeBarcode' - The barcode that you want to assign to the tape.
-- * 'tapeSizeInBytes' - The size, in bytes, of the virtual tape that you want to create.
-- * 'worm' - Set to @TRUE@ if the tape you are creating is to be configured as a write-once-read-many (WORM) tape.
mkCreateTapeWithBarcode ::
  -- | 'gatewayARN'
  Lude.Text ->
  -- | 'tapeSizeInBytes'
  Lude.Integer ->
  -- | 'tapeBarcode'
  Lude.Text ->
  CreateTapeWithBarcode
mkCreateTapeWithBarcode
  pGatewayARN_
  pTapeSizeInBytes_
  pTapeBarcode_ =
    CreateTapeWithBarcode'
      { kmsKey = Lude.Nothing,
        kmsEncrypted = Lude.Nothing,
        poolId = Lude.Nothing,
        worm = Lude.Nothing,
        tags = Lude.Nothing,
        gatewayARN = pGatewayARN_,
        tapeSizeInBytes = pTapeSizeInBytes_,
        tapeBarcode = pTapeBarcode_
      }

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbKMSKey :: Lens.Lens' CreateTapeWithBarcode (Lude.Maybe Lude.Text)
ctwbKMSKey = Lens.lens (kmsKey :: CreateTapeWithBarcode -> Lude.Maybe Lude.Text) (\s a -> s {kmsKey = a} :: CreateTapeWithBarcode)
{-# DEPRECATED ctwbKMSKey "Use generic-lens or generic-optics with 'kmsKey' instead." #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'kmsEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbKMSEncrypted :: Lens.Lens' CreateTapeWithBarcode (Lude.Maybe Lude.Bool)
ctwbKMSEncrypted = Lens.lens (kmsEncrypted :: CreateTapeWithBarcode -> Lude.Maybe Lude.Bool) (\s a -> s {kmsEncrypted = a} :: CreateTapeWithBarcode)
{-# DEPRECATED ctwbKMSEncrypted "Use generic-lens or generic-optics with 'kmsEncrypted' instead." #-}

-- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbPoolId :: Lens.Lens' CreateTapeWithBarcode (Lude.Maybe Lude.Text)
ctwbPoolId = Lens.lens (poolId :: CreateTapeWithBarcode -> Lude.Maybe Lude.Text) (\s a -> s {poolId = a} :: CreateTapeWithBarcode)
{-# DEPRECATED ctwbPoolId "Use generic-lens or generic-optics with 'poolId' instead." #-}

-- | Set to @TRUE@ if the tape you are creating is to be configured as a write-once-read-many (WORM) tape.
--
-- /Note:/ Consider using 'worm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbWorm :: Lens.Lens' CreateTapeWithBarcode (Lude.Maybe Lude.Bool)
ctwbWorm = Lens.lens (worm :: CreateTapeWithBarcode -> Lude.Maybe Lude.Bool) (\s a -> s {worm = a} :: CreateTapeWithBarcode)
{-# DEPRECATED ctwbWorm "Use generic-lens or generic-optics with 'worm' instead." #-}

-- | A list of up to 50 tags that can be assigned to a virtual tape that has a barcode. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbTags :: Lens.Lens' CreateTapeWithBarcode (Lude.Maybe [Tag])
ctwbTags = Lens.lens (tags :: CreateTapeWithBarcode -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateTapeWithBarcode)
{-# DEPRECATED ctwbTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The unique Amazon Resource Name (ARN) that represents the gateway to associate the virtual tape with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbGatewayARN :: Lens.Lens' CreateTapeWithBarcode Lude.Text
ctwbGatewayARN = Lens.lens (gatewayARN :: CreateTapeWithBarcode -> Lude.Text) (\s a -> s {gatewayARN = a} :: CreateTapeWithBarcode)
{-# DEPRECATED ctwbGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The size, in bytes, of the virtual tape that you want to create.
--
-- /Note:/ Consider using 'tapeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbTapeSizeInBytes :: Lens.Lens' CreateTapeWithBarcode Lude.Integer
ctwbTapeSizeInBytes = Lens.lens (tapeSizeInBytes :: CreateTapeWithBarcode -> Lude.Integer) (\s a -> s {tapeSizeInBytes = a} :: CreateTapeWithBarcode)
{-# DEPRECATED ctwbTapeSizeInBytes "Use generic-lens or generic-optics with 'tapeSizeInBytes' instead." #-}

-- | The barcode that you want to assign to the tape.
--
-- /Note:/ Consider using 'tapeBarcode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbTapeBarcode :: Lens.Lens' CreateTapeWithBarcode Lude.Text
ctwbTapeBarcode = Lens.lens (tapeBarcode :: CreateTapeWithBarcode -> Lude.Text) (\s a -> s {tapeBarcode = a} :: CreateTapeWithBarcode)
{-# DEPRECATED ctwbTapeBarcode "Use generic-lens or generic-optics with 'tapeBarcode' instead." #-}

instance Lude.AWSRequest CreateTapeWithBarcode where
  type Rs CreateTapeWithBarcode = CreateTapeWithBarcodeResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTapeWithBarcodeResponse'
            Lude.<$> (x Lude..?> "TapeARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTapeWithBarcode where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.CreateTapeWithBarcode" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTapeWithBarcode where
  toJSON CreateTapeWithBarcode' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KMSKey" Lude..=) Lude.<$> kmsKey,
            ("KMSEncrypted" Lude..=) Lude.<$> kmsEncrypted,
            ("PoolId" Lude..=) Lude.<$> poolId,
            ("Worm" Lude..=) Lude.<$> worm,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("TapeSizeInBytes" Lude..= tapeSizeInBytes),
            Lude.Just ("TapeBarcode" Lude..= tapeBarcode)
          ]
      )

instance Lude.ToPath CreateTapeWithBarcode where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTapeWithBarcode where
  toQuery = Lude.const Lude.mempty

-- | CreateTapeOutput
--
-- /See:/ 'mkCreateTapeWithBarcodeResponse' smart constructor.
data CreateTapeWithBarcodeResponse = CreateTapeWithBarcodeResponse'
  { tapeARN ::
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

-- | Creates a value of 'CreateTapeWithBarcodeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tapeARN' - A unique Amazon Resource Name (ARN) that represents the virtual tape that was created.
mkCreateTapeWithBarcodeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTapeWithBarcodeResponse
mkCreateTapeWithBarcodeResponse pResponseStatus_ =
  CreateTapeWithBarcodeResponse'
    { tapeARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique Amazon Resource Name (ARN) that represents the virtual tape that was created.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbrsTapeARN :: Lens.Lens' CreateTapeWithBarcodeResponse (Lude.Maybe Lude.Text)
ctwbrsTapeARN = Lens.lens (tapeARN :: CreateTapeWithBarcodeResponse -> Lude.Maybe Lude.Text) (\s a -> s {tapeARN = a} :: CreateTapeWithBarcodeResponse)
{-# DEPRECATED ctwbrsTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbrsResponseStatus :: Lens.Lens' CreateTapeWithBarcodeResponse Lude.Int
ctwbrsResponseStatus = Lens.lens (responseStatus :: CreateTapeWithBarcodeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTapeWithBarcodeResponse)
{-# DEPRECATED ctwbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
