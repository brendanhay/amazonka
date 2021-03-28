{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateTapes (..)
    , mkCreateTapes
    -- ** Request lenses
    , ctGatewayARN
    , ctTapeSizeInBytes
    , ctClientToken
    , ctNumTapesToCreate
    , ctTapeBarcodePrefix
    , ctKMSEncrypted
    , ctKMSKey
    , ctPoolId
    , ctTags
    , ctWorm

    -- * Destructuring the response
    , CreateTapesResponse (..)
    , mkCreateTapesResponse
    -- ** Response lenses
    , ctrrsTapeARNs
    , ctrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | CreateTapesInput
--
-- /See:/ 'mkCreateTapes' smart constructor.
data CreateTapes = CreateTapes'
  { gatewayARN :: Types.GatewayARN
    -- ^ The unique Amazon Resource Name (ARN) that represents the gateway to associate the virtual tapes with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
  , tapeSizeInBytes :: Core.Integer
    -- ^ The size, in bytes, of the virtual tapes that you want to create.
  , clientToken :: Types.ClientToken
    -- ^ A unique identifier that you use to retry a request. If you retry a request, use the same @ClientToken@ you specified in the initial request.
  , numTapesToCreate :: Core.Natural
    -- ^ The number of virtual tapes that you want to create.
  , tapeBarcodePrefix :: Types.TapeBarcodePrefix
    -- ^ A prefix that you append to the barcode of the virtual tape you are creating. This prefix makes the barcode unique.
  , kMSEncrypted :: Core.Maybe Core.Bool
    -- ^ Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@ 
  , kMSKey :: Core.Maybe Types.KMSKey
    -- ^ The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
  , poolId :: Core.Maybe Types.PoolId
    -- ^ The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@ 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of up to 50 tags that can be assigned to a virtual tape. Each tag is a key-value pair.
  , worm :: Core.Maybe Core.Bool
    -- ^ Set to @TRUE@ if the tape you are creating is to be configured as a write-once-read-many (WORM) tape.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTapes' value with any optional fields omitted.
mkCreateTapes
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> Core.Integer -- ^ 'tapeSizeInBytes'
    -> Types.ClientToken -- ^ 'clientToken'
    -> Core.Natural -- ^ 'numTapesToCreate'
    -> Types.TapeBarcodePrefix -- ^ 'tapeBarcodePrefix'
    -> CreateTapes
mkCreateTapes gatewayARN tapeSizeInBytes clientToken
  numTapesToCreate tapeBarcodePrefix
  = CreateTapes'{gatewayARN, tapeSizeInBytes, clientToken,
                 numTapesToCreate, tapeBarcodePrefix, kMSEncrypted = Core.Nothing,
                 kMSKey = Core.Nothing, poolId = Core.Nothing, tags = Core.Nothing,
                 worm = Core.Nothing}

-- | The unique Amazon Resource Name (ARN) that represents the gateway to associate the virtual tapes with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctGatewayARN :: Lens.Lens' CreateTapes Types.GatewayARN
ctGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE ctGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The size, in bytes, of the virtual tapes that you want to create.
--
-- /Note:/ Consider using 'tapeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTapeSizeInBytes :: Lens.Lens' CreateTapes Core.Integer
ctTapeSizeInBytes = Lens.field @"tapeSizeInBytes"
{-# INLINEABLE ctTapeSizeInBytes #-}
{-# DEPRECATED tapeSizeInBytes "Use generic-lens or generic-optics with 'tapeSizeInBytes' instead"  #-}

-- | A unique identifier that you use to retry a request. If you retry a request, use the same @ClientToken@ you specified in the initial request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctClientToken :: Lens.Lens' CreateTapes Types.ClientToken
ctClientToken = Lens.field @"clientToken"
{-# INLINEABLE ctClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The number of virtual tapes that you want to create.
--
-- /Note:/ Consider using 'numTapesToCreate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctNumTapesToCreate :: Lens.Lens' CreateTapes Core.Natural
ctNumTapesToCreate = Lens.field @"numTapesToCreate"
{-# INLINEABLE ctNumTapesToCreate #-}
{-# DEPRECATED numTapesToCreate "Use generic-lens or generic-optics with 'numTapesToCreate' instead"  #-}

-- | A prefix that you append to the barcode of the virtual tape you are creating. This prefix makes the barcode unique.
--
-- /Note:/ Consider using 'tapeBarcodePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTapeBarcodePrefix :: Lens.Lens' CreateTapes Types.TapeBarcodePrefix
ctTapeBarcodePrefix = Lens.field @"tapeBarcodePrefix"
{-# INLINEABLE ctTapeBarcodePrefix #-}
{-# DEPRECATED tapeBarcodePrefix "Use generic-lens or generic-optics with 'tapeBarcodePrefix' instead"  #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'kMSEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctKMSEncrypted :: Lens.Lens' CreateTapes (Core.Maybe Core.Bool)
ctKMSEncrypted = Lens.field @"kMSEncrypted"
{-# INLINEABLE ctKMSEncrypted #-}
{-# DEPRECATED kMSEncrypted "Use generic-lens or generic-optics with 'kMSEncrypted' instead"  #-}

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kMSKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctKMSKey :: Lens.Lens' CreateTapes (Core.Maybe Types.KMSKey)
ctKMSKey = Lens.field @"kMSKey"
{-# INLINEABLE ctKMSKey #-}
{-# DEPRECATED kMSKey "Use generic-lens or generic-optics with 'kMSKey' instead"  #-}

-- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@ 
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctPoolId :: Lens.Lens' CreateTapes (Core.Maybe Types.PoolId)
ctPoolId = Lens.field @"poolId"
{-# INLINEABLE ctPoolId #-}
{-# DEPRECATED poolId "Use generic-lens or generic-optics with 'poolId' instead"  #-}

-- | A list of up to 50 tags that can be assigned to a virtual tape. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTapes (Core.Maybe [Types.Tag])
ctTags = Lens.field @"tags"
{-# INLINEABLE ctTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Set to @TRUE@ if the tape you are creating is to be configured as a write-once-read-many (WORM) tape.
--
-- /Note:/ Consider using 'worm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctWorm :: Lens.Lens' CreateTapes (Core.Maybe Core.Bool)
ctWorm = Lens.field @"worm"
{-# INLINEABLE ctWorm #-}
{-# DEPRECATED worm "Use generic-lens or generic-optics with 'worm' instead"  #-}

instance Core.ToQuery CreateTapes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateTapes where
        toHeaders CreateTapes{..}
          = Core.pure ("X-Amz-Target", "StorageGateway_20130630.CreateTapes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateTapes where
        toJSON CreateTapes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GatewayARN" Core..= gatewayARN),
                  Core.Just ("TapeSizeInBytes" Core..= tapeSizeInBytes),
                  Core.Just ("ClientToken" Core..= clientToken),
                  Core.Just ("NumTapesToCreate" Core..= numTapesToCreate),
                  Core.Just ("TapeBarcodePrefix" Core..= tapeBarcodePrefix),
                  ("KMSEncrypted" Core..=) Core.<$> kMSEncrypted,
                  ("KMSKey" Core..=) Core.<$> kMSKey,
                  ("PoolId" Core..=) Core.<$> poolId, ("Tags" Core..=) Core.<$> tags,
                  ("Worm" Core..=) Core.<$> worm])

instance Core.AWSRequest CreateTapes where
        type Rs CreateTapes = CreateTapesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateTapesResponse' Core.<$>
                   (x Core..:? "TapeARNs") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | CreateTapeOutput
--
-- /See:/ 'mkCreateTapesResponse' smart constructor.
data CreateTapesResponse = CreateTapesResponse'
  { tapeARNs :: Core.Maybe [Types.TapeARN]
    -- ^ A list of unique Amazon Resource Names (ARNs) that represents the virtual tapes that were created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTapesResponse' value with any optional fields omitted.
mkCreateTapesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTapesResponse
mkCreateTapesResponse responseStatus
  = CreateTapesResponse'{tapeARNs = Core.Nothing, responseStatus}

-- | A list of unique Amazon Resource Names (ARNs) that represents the virtual tapes that were created.
--
-- /Note:/ Consider using 'tapeARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsTapeARNs :: Lens.Lens' CreateTapesResponse (Core.Maybe [Types.TapeARN])
ctrrsTapeARNs = Lens.field @"tapeARNs"
{-# INLINEABLE ctrrsTapeARNs #-}
{-# DEPRECATED tapeARNs "Use generic-lens or generic-optics with 'tapeARNs' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsResponseStatus :: Lens.Lens' CreateTapesResponse Core.Int
ctrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
