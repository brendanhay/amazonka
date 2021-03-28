{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a field-level encryption configuration. 
module Network.AWS.CloudFront.UpdateFieldLevelEncryptionConfig
    (
    -- * Creating a request
      UpdateFieldLevelEncryptionConfig (..)
    , mkUpdateFieldLevelEncryptionConfig
    -- ** Request lenses
    , uflecFieldLevelEncryptionConfig
    , uflecId
    , uflecIfMatch

    -- * Destructuring the response
    , UpdateFieldLevelEncryptionConfigResponse (..)
    , mkUpdateFieldLevelEncryptionConfigResponse
    -- ** Response lenses
    , uflecrrsETag
    , uflecrrsFieldLevelEncryption
    , uflecrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateFieldLevelEncryptionConfig' smart constructor.
data UpdateFieldLevelEncryptionConfig = UpdateFieldLevelEncryptionConfig'
  { fieldLevelEncryptionConfig :: Types.FieldLevelEncryptionConfig
    -- ^ Request to update a field-level encryption configuration. 
  , id :: Core.Text
    -- ^ The ID of the configuration you want to update.
  , ifMatch :: Core.Maybe Core.Text
    -- ^ The value of the @ETag@ header that you received when retrieving the configuration identity to update. For example: @E2QWRUHAPOMQZL@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFieldLevelEncryptionConfig' value with any optional fields omitted.
mkUpdateFieldLevelEncryptionConfig
    :: Types.FieldLevelEncryptionConfig -- ^ 'fieldLevelEncryptionConfig'
    -> Core.Text -- ^ 'id'
    -> UpdateFieldLevelEncryptionConfig
mkUpdateFieldLevelEncryptionConfig fieldLevelEncryptionConfig id
  = UpdateFieldLevelEncryptionConfig'{fieldLevelEncryptionConfig, id,
                                      ifMatch = Core.Nothing}

-- | Request to update a field-level encryption configuration. 
--
-- /Note:/ Consider using 'fieldLevelEncryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflecFieldLevelEncryptionConfig :: Lens.Lens' UpdateFieldLevelEncryptionConfig Types.FieldLevelEncryptionConfig
uflecFieldLevelEncryptionConfig = Lens.field @"fieldLevelEncryptionConfig"
{-# INLINEABLE uflecFieldLevelEncryptionConfig #-}
{-# DEPRECATED fieldLevelEncryptionConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionConfig' instead"  #-}

-- | The ID of the configuration you want to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflecId :: Lens.Lens' UpdateFieldLevelEncryptionConfig Core.Text
uflecId = Lens.field @"id"
{-# INLINEABLE uflecId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The value of the @ETag@ header that you received when retrieving the configuration identity to update. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflecIfMatch :: Lens.Lens' UpdateFieldLevelEncryptionConfig (Core.Maybe Core.Text)
uflecIfMatch = Lens.field @"ifMatch"
{-# INLINEABLE uflecIfMatch #-}
{-# DEPRECATED ifMatch "Use generic-lens or generic-optics with 'ifMatch' instead"  #-}

instance Core.ToQuery UpdateFieldLevelEncryptionConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateFieldLevelEncryptionConfig where
        toHeaders UpdateFieldLevelEncryptionConfig{..}
          = Core.toHeaders "If-Match" ifMatch

instance Core.AWSRequest UpdateFieldLevelEncryptionConfig where
        type Rs UpdateFieldLevelEncryptionConfig =
             UpdateFieldLevelEncryptionConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/2020-05-31/field-level-encryption/" Core.<> Core.toText id
                             Core.<> "/config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 UpdateFieldLevelEncryptionConfigResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*> Core.parseXML x Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateFieldLevelEncryptionConfigResponse' smart constructor.
data UpdateFieldLevelEncryptionConfigResponse = UpdateFieldLevelEncryptionConfigResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The value of the @ETag@ header that you received when updating the configuration. For example: @E2QWRUHAPOMQZL@ .
  , fieldLevelEncryption :: Core.Maybe Types.FieldLevelEncryption
    -- ^ Return the results of updating the configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateFieldLevelEncryptionConfigResponse' value with any optional fields omitted.
mkUpdateFieldLevelEncryptionConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateFieldLevelEncryptionConfigResponse
mkUpdateFieldLevelEncryptionConfigResponse responseStatus
  = UpdateFieldLevelEncryptionConfigResponse'{eTag = Core.Nothing,
                                              fieldLevelEncryption = Core.Nothing, responseStatus}

-- | The value of the @ETag@ header that you received when updating the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflecrrsETag :: Lens.Lens' UpdateFieldLevelEncryptionConfigResponse (Core.Maybe Core.Text)
uflecrrsETag = Lens.field @"eTag"
{-# INLINEABLE uflecrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | Return the results of updating the configuration.
--
-- /Note:/ Consider using 'fieldLevelEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflecrrsFieldLevelEncryption :: Lens.Lens' UpdateFieldLevelEncryptionConfigResponse (Core.Maybe Types.FieldLevelEncryption)
uflecrrsFieldLevelEncryption = Lens.field @"fieldLevelEncryption"
{-# INLINEABLE uflecrrsFieldLevelEncryption #-}
{-# DEPRECATED fieldLevelEncryption "Use generic-lens or generic-optics with 'fieldLevelEncryption' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflecrrsResponseStatus :: Lens.Lens' UpdateFieldLevelEncryptionConfigResponse Core.Int
uflecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uflecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
