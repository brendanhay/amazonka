{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.PutEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the encryption configuration for X-Ray data.
module Network.AWS.XRay.PutEncryptionConfig
    (
    -- * Creating a request
      PutEncryptionConfig (..)
    , mkPutEncryptionConfig
    -- ** Request lenses
    , pecType
    , pecKeyId

    -- * Destructuring the response
    , PutEncryptionConfigResponse (..)
    , mkPutEncryptionConfigResponse
    -- ** Response lenses
    , pecrrsEncryptionConfig
    , pecrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkPutEncryptionConfig' smart constructor.
data PutEncryptionConfig = PutEncryptionConfig'
  { type' :: Types.EncryptionType
    -- ^ The type of encryption. Set to @KMS@ to use your own key for encryption. Set to @NONE@ for default encryption.
  , keyId :: Core.Maybe Types.EncryptionKeyId
    -- ^ An AWS KMS customer master key (CMK) in one of the following formats:
--
--
--     * __Alias__ - The name of the key. For example, @alias/MyKey@ .
--
--
--     * __Key ID__ - The KMS key ID of the key. For example, @ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@ . AWS X-Ray does not support asymmetric CMKs.
--
--
--     * __ARN__ - The full Amazon Resource Name of the key ID or alias. For example, @arn:aws:kms:us-east-2:123456789012:key/ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@ . Use this format to specify a key in a different account.
--
--
-- Omit this key if you set @Type@ to @NONE@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutEncryptionConfig' value with any optional fields omitted.
mkPutEncryptionConfig
    :: Types.EncryptionType -- ^ 'type\''
    -> PutEncryptionConfig
mkPutEncryptionConfig type'
  = PutEncryptionConfig'{type', keyId = Core.Nothing}

-- | The type of encryption. Set to @KMS@ to use your own key for encryption. Set to @NONE@ for default encryption.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pecType :: Lens.Lens' PutEncryptionConfig Types.EncryptionType
pecType = Lens.field @"type'"
{-# INLINEABLE pecType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | An AWS KMS customer master key (CMK) in one of the following formats:
--
--
--     * __Alias__ - The name of the key. For example, @alias/MyKey@ .
--
--
--     * __Key ID__ - The KMS key ID of the key. For example, @ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@ . AWS X-Ray does not support asymmetric CMKs.
--
--
--     * __ARN__ - The full Amazon Resource Name of the key ID or alias. For example, @arn:aws:kms:us-east-2:123456789012:key/ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@ . Use this format to specify a key in a different account.
--
--
-- Omit this key if you set @Type@ to @NONE@ .
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pecKeyId :: Lens.Lens' PutEncryptionConfig (Core.Maybe Types.EncryptionKeyId)
pecKeyId = Lens.field @"keyId"
{-# INLINEABLE pecKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

instance Core.ToQuery PutEncryptionConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutEncryptionConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON PutEncryptionConfig where
        toJSON PutEncryptionConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Type" Core..= type'),
                  ("KeyId" Core..=) Core.<$> keyId])

instance Core.AWSRequest PutEncryptionConfig where
        type Rs PutEncryptionConfig = PutEncryptionConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/PutEncryptionConfig",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutEncryptionConfigResponse' Core.<$>
                   (x Core..:? "EncryptionConfig") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutEncryptionConfigResponse' smart constructor.
data PutEncryptionConfigResponse = PutEncryptionConfigResponse'
  { encryptionConfig :: Core.Maybe Types.EncryptionConfig
    -- ^ The new encryption configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutEncryptionConfigResponse' value with any optional fields omitted.
mkPutEncryptionConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutEncryptionConfigResponse
mkPutEncryptionConfigResponse responseStatus
  = PutEncryptionConfigResponse'{encryptionConfig = Core.Nothing,
                                 responseStatus}

-- | The new encryption configuration.
--
-- /Note:/ Consider using 'encryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pecrrsEncryptionConfig :: Lens.Lens' PutEncryptionConfigResponse (Core.Maybe Types.EncryptionConfig)
pecrrsEncryptionConfig = Lens.field @"encryptionConfig"
{-# INLINEABLE pecrrsEncryptionConfig #-}
{-# DEPRECATED encryptionConfig "Use generic-lens or generic-optics with 'encryptionConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pecrrsResponseStatus :: Lens.Lens' PutEncryptionConfigResponse Core.Int
pecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
