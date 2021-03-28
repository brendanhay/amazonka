{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateTapeWithBarcode (..)
    , mkCreateTapeWithBarcode
    -- ** Request lenses
    , ctwbGatewayARN
    , ctwbTapeSizeInBytes
    , ctwbTapeBarcode
    , ctwbKMSEncrypted
    , ctwbKMSKey
    , ctwbPoolId
    , ctwbTags
    , ctwbWorm

    -- * Destructuring the response
    , CreateTapeWithBarcodeResponse (..)
    , mkCreateTapeWithBarcodeResponse
    -- ** Response lenses
    , ctwbrrsTapeARN
    , ctwbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | CreateTapeWithBarcodeInput
--
-- /See:/ 'mkCreateTapeWithBarcode' smart constructor.
data CreateTapeWithBarcode = CreateTapeWithBarcode'
  { gatewayARN :: Types.GatewayARN
    -- ^ The unique Amazon Resource Name (ARN) that represents the gateway to associate the virtual tape with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
  , tapeSizeInBytes :: Core.Integer
    -- ^ The size, in bytes, of the virtual tape that you want to create.
  , tapeBarcode :: Types.TapeBarcode
    -- ^ The barcode that you want to assign to the tape.
  , kMSEncrypted :: Core.Maybe Core.Bool
    -- ^ Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@ 
  , kMSKey :: Core.Maybe Types.KMSKey
    -- ^ The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
  , poolId :: Core.Maybe Types.PoolId
    -- ^ The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@ 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of up to 50 tags that can be assigned to a virtual tape that has a barcode. Each tag is a key-value pair.
  , worm :: Core.Maybe Core.Bool
    -- ^ Set to @TRUE@ if the tape you are creating is to be configured as a write-once-read-many (WORM) tape.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTapeWithBarcode' value with any optional fields omitted.
mkCreateTapeWithBarcode
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> Core.Integer -- ^ 'tapeSizeInBytes'
    -> Types.TapeBarcode -- ^ 'tapeBarcode'
    -> CreateTapeWithBarcode
mkCreateTapeWithBarcode gatewayARN tapeSizeInBytes tapeBarcode
  = CreateTapeWithBarcode'{gatewayARN, tapeSizeInBytes, tapeBarcode,
                           kMSEncrypted = Core.Nothing, kMSKey = Core.Nothing,
                           poolId = Core.Nothing, tags = Core.Nothing, worm = Core.Nothing}

-- | The unique Amazon Resource Name (ARN) that represents the gateway to associate the virtual tape with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbGatewayARN :: Lens.Lens' CreateTapeWithBarcode Types.GatewayARN
ctwbGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE ctwbGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The size, in bytes, of the virtual tape that you want to create.
--
-- /Note:/ Consider using 'tapeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbTapeSizeInBytes :: Lens.Lens' CreateTapeWithBarcode Core.Integer
ctwbTapeSizeInBytes = Lens.field @"tapeSizeInBytes"
{-# INLINEABLE ctwbTapeSizeInBytes #-}
{-# DEPRECATED tapeSizeInBytes "Use generic-lens or generic-optics with 'tapeSizeInBytes' instead"  #-}

-- | The barcode that you want to assign to the tape.
--
-- /Note:/ Consider using 'tapeBarcode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbTapeBarcode :: Lens.Lens' CreateTapeWithBarcode Types.TapeBarcode
ctwbTapeBarcode = Lens.field @"tapeBarcode"
{-# INLINEABLE ctwbTapeBarcode #-}
{-# DEPRECATED tapeBarcode "Use generic-lens or generic-optics with 'tapeBarcode' instead"  #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'kMSEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbKMSEncrypted :: Lens.Lens' CreateTapeWithBarcode (Core.Maybe Core.Bool)
ctwbKMSEncrypted = Lens.field @"kMSEncrypted"
{-# INLINEABLE ctwbKMSEncrypted #-}
{-# DEPRECATED kMSEncrypted "Use generic-lens or generic-optics with 'kMSEncrypted' instead"  #-}

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kMSKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbKMSKey :: Lens.Lens' CreateTapeWithBarcode (Core.Maybe Types.KMSKey)
ctwbKMSKey = Lens.field @"kMSKey"
{-# INLINEABLE ctwbKMSKey #-}
{-# DEPRECATED kMSKey "Use generic-lens or generic-optics with 'kMSKey' instead"  #-}

-- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@ 
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbPoolId :: Lens.Lens' CreateTapeWithBarcode (Core.Maybe Types.PoolId)
ctwbPoolId = Lens.field @"poolId"
{-# INLINEABLE ctwbPoolId #-}
{-# DEPRECATED poolId "Use generic-lens or generic-optics with 'poolId' instead"  #-}

-- | A list of up to 50 tags that can be assigned to a virtual tape that has a barcode. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbTags :: Lens.Lens' CreateTapeWithBarcode (Core.Maybe [Types.Tag])
ctwbTags = Lens.field @"tags"
{-# INLINEABLE ctwbTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Set to @TRUE@ if the tape you are creating is to be configured as a write-once-read-many (WORM) tape.
--
-- /Note:/ Consider using 'worm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbWorm :: Lens.Lens' CreateTapeWithBarcode (Core.Maybe Core.Bool)
ctwbWorm = Lens.field @"worm"
{-# INLINEABLE ctwbWorm #-}
{-# DEPRECATED worm "Use generic-lens or generic-optics with 'worm' instead"  #-}

instance Core.ToQuery CreateTapeWithBarcode where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateTapeWithBarcode where
        toHeaders CreateTapeWithBarcode{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.CreateTapeWithBarcode")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateTapeWithBarcode where
        toJSON CreateTapeWithBarcode{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GatewayARN" Core..= gatewayARN),
                  Core.Just ("TapeSizeInBytes" Core..= tapeSizeInBytes),
                  Core.Just ("TapeBarcode" Core..= tapeBarcode),
                  ("KMSEncrypted" Core..=) Core.<$> kMSEncrypted,
                  ("KMSKey" Core..=) Core.<$> kMSKey,
                  ("PoolId" Core..=) Core.<$> poolId, ("Tags" Core..=) Core.<$> tags,
                  ("Worm" Core..=) Core.<$> worm])

instance Core.AWSRequest CreateTapeWithBarcode where
        type Rs CreateTapeWithBarcode = CreateTapeWithBarcodeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateTapeWithBarcodeResponse' Core.<$>
                   (x Core..:? "TapeARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | CreateTapeOutput
--
-- /See:/ 'mkCreateTapeWithBarcodeResponse' smart constructor.
data CreateTapeWithBarcodeResponse = CreateTapeWithBarcodeResponse'
  { tapeARN :: Core.Maybe Types.TapeARN
    -- ^ A unique Amazon Resource Name (ARN) that represents the virtual tape that was created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTapeWithBarcodeResponse' value with any optional fields omitted.
mkCreateTapeWithBarcodeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTapeWithBarcodeResponse
mkCreateTapeWithBarcodeResponse responseStatus
  = CreateTapeWithBarcodeResponse'{tapeARN = Core.Nothing,
                                   responseStatus}

-- | A unique Amazon Resource Name (ARN) that represents the virtual tape that was created.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbrrsTapeARN :: Lens.Lens' CreateTapeWithBarcodeResponse (Core.Maybe Types.TapeARN)
ctwbrrsTapeARN = Lens.field @"tapeARN"
{-# INLINEABLE ctwbrrsTapeARN #-}
{-# DEPRECATED tapeARN "Use generic-lens or generic-optics with 'tapeARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctwbrrsResponseStatus :: Lens.Lens' CreateTapeWithBarcodeResponse Core.Int
ctwbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctwbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
