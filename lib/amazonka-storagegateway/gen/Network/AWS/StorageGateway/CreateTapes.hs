{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateTapes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more virtual tapes. You write data to the virtual tapes and then archive the tapes. This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.CreateTapes
  ( -- * Creating a request
    CreateTapes (..),
    mkCreateTapes,

    -- ** Request lenses
    ctKMSKey,
    ctKMSEncrypted,
    ctPoolId,
    ctWorm,
    ctTags,
    ctGatewayARN,
    ctTapeSizeInBytes,
    ctClientToken,
    ctNumTapesToCreate,
    ctTapeBarcodePrefix,

    -- * Destructuring the response
    CreateTapesResponse (..),
    mkCreateTapesResponse,

    -- ** Response lenses
    ctrsTapeARNs,
    ctrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | CreateTapesInput
--
-- /See:/ 'mkCreateTapes' smart constructor.
data CreateTapes = CreateTapes'
  { kmsKey :: Lude.Maybe Lude.Text,
    kmsEncrypted :: Lude.Maybe Lude.Bool,
    poolId :: Lude.Maybe Lude.Text,
    worm :: Lude.Maybe Lude.Bool,
    tags :: Lude.Maybe [Tag],
    gatewayARN :: Lude.Text,
    tapeSizeInBytes :: Lude.Integer,
    clientToken :: Lude.Text,
    numTapesToCreate :: Lude.Natural,
    tapeBarcodePrefix :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTapes' with the minimum fields required to make a request.
--
-- * 'clientToken' - A unique identifier that you use to retry a request. If you retry a request, use the same @ClientToken@ you specified in the initial request.
-- * 'gatewayARN' - The unique Amazon Resource Name (ARN) that represents the gateway to associate the virtual tapes with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
-- * 'kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
-- * 'kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
-- * 'numTapesToCreate' - The number of virtual tapes that you want to create.
-- * 'poolId' - The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
-- * 'tags' - A list of up to 50 tags that can be assigned to a virtual tape. Each tag is a key-value pair.
-- * 'tapeBarcodePrefix' - A prefix that you append to the barcode of the virtual tape you are creating. This prefix makes the barcode unique.
-- * 'tapeSizeInBytes' - The size, in bytes, of the virtual tapes that you want to create.
-- * 'worm' - Set to @TRUE@ if the tape you are creating is to be configured as a write-once-read-many (WORM) tape.
mkCreateTapes ::
  -- | 'gatewayARN'
  Lude.Text ->
  -- | 'tapeSizeInBytes'
  Lude.Integer ->
  -- | 'clientToken'
  Lude.Text ->
  -- | 'numTapesToCreate'
  Lude.Natural ->
  -- | 'tapeBarcodePrefix'
  Lude.Text ->
  CreateTapes
mkCreateTapes
  pGatewayARN_
  pTapeSizeInBytes_
  pClientToken_
  pNumTapesToCreate_
  pTapeBarcodePrefix_ =
    CreateTapes'
      { kmsKey = Lude.Nothing,
        kmsEncrypted = Lude.Nothing,
        poolId = Lude.Nothing,
        worm = Lude.Nothing,
        tags = Lude.Nothing,
        gatewayARN = pGatewayARN_,
        tapeSizeInBytes = pTapeSizeInBytes_,
        clientToken = pClientToken_,
        numTapesToCreate = pNumTapesToCreate_,
        tapeBarcodePrefix = pTapeBarcodePrefix_
      }

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctKMSKey :: Lens.Lens' CreateTapes (Lude.Maybe Lude.Text)
ctKMSKey = Lens.lens (kmsKey :: CreateTapes -> Lude.Maybe Lude.Text) (\s a -> s {kmsKey = a} :: CreateTapes)
{-# DEPRECATED ctKMSKey "Use generic-lens or generic-optics with 'kmsKey' instead." #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'kmsEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctKMSEncrypted :: Lens.Lens' CreateTapes (Lude.Maybe Lude.Bool)
ctKMSEncrypted = Lens.lens (kmsEncrypted :: CreateTapes -> Lude.Maybe Lude.Bool) (\s a -> s {kmsEncrypted = a} :: CreateTapes)
{-# DEPRECATED ctKMSEncrypted "Use generic-lens or generic-optics with 'kmsEncrypted' instead." #-}

-- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctPoolId :: Lens.Lens' CreateTapes (Lude.Maybe Lude.Text)
ctPoolId = Lens.lens (poolId :: CreateTapes -> Lude.Maybe Lude.Text) (\s a -> s {poolId = a} :: CreateTapes)
{-# DEPRECATED ctPoolId "Use generic-lens or generic-optics with 'poolId' instead." #-}

-- | Set to @TRUE@ if the tape you are creating is to be configured as a write-once-read-many (WORM) tape.
--
-- /Note:/ Consider using 'worm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctWorm :: Lens.Lens' CreateTapes (Lude.Maybe Lude.Bool)
ctWorm = Lens.lens (worm :: CreateTapes -> Lude.Maybe Lude.Bool) (\s a -> s {worm = a} :: CreateTapes)
{-# DEPRECATED ctWorm "Use generic-lens or generic-optics with 'worm' instead." #-}

-- | A list of up to 50 tags that can be assigned to a virtual tape. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTapes (Lude.Maybe [Tag])
ctTags = Lens.lens (tags :: CreateTapes -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateTapes)
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The unique Amazon Resource Name (ARN) that represents the gateway to associate the virtual tapes with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctGatewayARN :: Lens.Lens' CreateTapes Lude.Text
ctGatewayARN = Lens.lens (gatewayARN :: CreateTapes -> Lude.Text) (\s a -> s {gatewayARN = a} :: CreateTapes)
{-# DEPRECATED ctGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The size, in bytes, of the virtual tapes that you want to create.
--
-- /Note:/ Consider using 'tapeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTapeSizeInBytes :: Lens.Lens' CreateTapes Lude.Integer
ctTapeSizeInBytes = Lens.lens (tapeSizeInBytes :: CreateTapes -> Lude.Integer) (\s a -> s {tapeSizeInBytes = a} :: CreateTapes)
{-# DEPRECATED ctTapeSizeInBytes "Use generic-lens or generic-optics with 'tapeSizeInBytes' instead." #-}

-- | A unique identifier that you use to retry a request. If you retry a request, use the same @ClientToken@ you specified in the initial request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctClientToken :: Lens.Lens' CreateTapes Lude.Text
ctClientToken = Lens.lens (clientToken :: CreateTapes -> Lude.Text) (\s a -> s {clientToken = a} :: CreateTapes)
{-# DEPRECATED ctClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The number of virtual tapes that you want to create.
--
-- /Note:/ Consider using 'numTapesToCreate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctNumTapesToCreate :: Lens.Lens' CreateTapes Lude.Natural
ctNumTapesToCreate = Lens.lens (numTapesToCreate :: CreateTapes -> Lude.Natural) (\s a -> s {numTapesToCreate = a} :: CreateTapes)
{-# DEPRECATED ctNumTapesToCreate "Use generic-lens or generic-optics with 'numTapesToCreate' instead." #-}

-- | A prefix that you append to the barcode of the virtual tape you are creating. This prefix makes the barcode unique.
--
-- /Note:/ Consider using 'tapeBarcodePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTapeBarcodePrefix :: Lens.Lens' CreateTapes Lude.Text
ctTapeBarcodePrefix = Lens.lens (tapeBarcodePrefix :: CreateTapes -> Lude.Text) (\s a -> s {tapeBarcodePrefix = a} :: CreateTapes)
{-# DEPRECATED ctTapeBarcodePrefix "Use generic-lens or generic-optics with 'tapeBarcodePrefix' instead." #-}

instance Lude.AWSRequest CreateTapes where
  type Rs CreateTapes = CreateTapesResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTapesResponse'
            Lude.<$> (x Lude..?> "TapeARNs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTapes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.CreateTapes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTapes where
  toJSON CreateTapes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KMSKey" Lude..=) Lude.<$> kmsKey,
            ("KMSEncrypted" Lude..=) Lude.<$> kmsEncrypted,
            ("PoolId" Lude..=) Lude.<$> poolId,
            ("Worm" Lude..=) Lude.<$> worm,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("TapeSizeInBytes" Lude..= tapeSizeInBytes),
            Lude.Just ("ClientToken" Lude..= clientToken),
            Lude.Just ("NumTapesToCreate" Lude..= numTapesToCreate),
            Lude.Just ("TapeBarcodePrefix" Lude..= tapeBarcodePrefix)
          ]
      )

instance Lude.ToPath CreateTapes where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTapes where
  toQuery = Lude.const Lude.mempty

-- | CreateTapeOutput
--
-- /See:/ 'mkCreateTapesResponse' smart constructor.
data CreateTapesResponse = CreateTapesResponse'
  { tapeARNs ::
      Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'CreateTapesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tapeARNs' - A list of unique Amazon Resource Names (ARNs) that represents the virtual tapes that were created.
mkCreateTapesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTapesResponse
mkCreateTapesResponse pResponseStatus_ =
  CreateTapesResponse'
    { tapeARNs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of unique Amazon Resource Names (ARNs) that represents the virtual tapes that were created.
--
-- /Note:/ Consider using 'tapeARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsTapeARNs :: Lens.Lens' CreateTapesResponse (Lude.Maybe [Lude.Text])
ctrsTapeARNs = Lens.lens (tapeARNs :: CreateTapesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {tapeARNs = a} :: CreateTapesResponse)
{-# DEPRECATED ctrsTapeARNs "Use generic-lens or generic-optics with 'tapeARNs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsResponseStatus :: Lens.Lens' CreateTapesResponse Lude.Int
ctrsResponseStatus = Lens.lens (responseStatus :: CreateTapesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTapesResponse)
{-# DEPRECATED ctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
