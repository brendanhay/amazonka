{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new field-level encryption configuration.
module Network.AWS.CloudFront.CreateFieldLevelEncryptionConfig
    (
    -- * Creating a request
      CreateFieldLevelEncryptionConfig (..)
    , mkCreateFieldLevelEncryptionConfig
    -- ** Request lenses
    , cflecFieldLevelEncryptionConfig

    -- * Destructuring the response
    , CreateFieldLevelEncryptionConfigResponse (..)
    , mkCreateFieldLevelEncryptionConfigResponse
    -- ** Response lenses
    , cflecrrsETag
    , cflecrrsFieldLevelEncryption
    , cflecrrsLocation
    , cflecrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateFieldLevelEncryptionConfig' smart constructor.
newtype CreateFieldLevelEncryptionConfig = CreateFieldLevelEncryptionConfig'
  { fieldLevelEncryptionConfig :: Types.FieldLevelEncryptionConfig
    -- ^ The request to create a new field-level encryption configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFieldLevelEncryptionConfig' value with any optional fields omitted.
mkCreateFieldLevelEncryptionConfig
    :: Types.FieldLevelEncryptionConfig -- ^ 'fieldLevelEncryptionConfig'
    -> CreateFieldLevelEncryptionConfig
mkCreateFieldLevelEncryptionConfig fieldLevelEncryptionConfig
  = CreateFieldLevelEncryptionConfig'{fieldLevelEncryptionConfig}

-- | The request to create a new field-level encryption configuration.
--
-- /Note:/ Consider using 'fieldLevelEncryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflecFieldLevelEncryptionConfig :: Lens.Lens' CreateFieldLevelEncryptionConfig Types.FieldLevelEncryptionConfig
cflecFieldLevelEncryptionConfig = Lens.field @"fieldLevelEncryptionConfig"
{-# INLINEABLE cflecFieldLevelEncryptionConfig #-}
{-# DEPRECATED fieldLevelEncryptionConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionConfig' instead"  #-}

instance Core.ToQuery CreateFieldLevelEncryptionConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateFieldLevelEncryptionConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateFieldLevelEncryptionConfig where
        type Rs CreateFieldLevelEncryptionConfig =
             CreateFieldLevelEncryptionConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2020-05-31/field-level-encryption",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateFieldLevelEncryptionConfigResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*> Core.parseXML x Core.<*>
                     Core.parseHeaderMaybe "Location" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateFieldLevelEncryptionConfigResponse' smart constructor.
data CreateFieldLevelEncryptionConfigResponse = CreateFieldLevelEncryptionConfigResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
  , fieldLevelEncryption :: Core.Maybe Types.FieldLevelEncryption
    -- ^ Returned when you create a new field-level encryption configuration.
  , location :: Core.Maybe Core.Text
    -- ^ The fully qualified URI of the new configuration resource just created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateFieldLevelEncryptionConfigResponse' value with any optional fields omitted.
mkCreateFieldLevelEncryptionConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateFieldLevelEncryptionConfigResponse
mkCreateFieldLevelEncryptionConfigResponse responseStatus
  = CreateFieldLevelEncryptionConfigResponse'{eTag = Core.Nothing,
                                              fieldLevelEncryption = Core.Nothing,
                                              location = Core.Nothing, responseStatus}

-- | The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflecrrsETag :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse (Core.Maybe Core.Text)
cflecrrsETag = Lens.field @"eTag"
{-# INLINEABLE cflecrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | Returned when you create a new field-level encryption configuration.
--
-- /Note:/ Consider using 'fieldLevelEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflecrrsFieldLevelEncryption :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse (Core.Maybe Types.FieldLevelEncryption)
cflecrrsFieldLevelEncryption = Lens.field @"fieldLevelEncryption"
{-# INLINEABLE cflecrrsFieldLevelEncryption #-}
{-# DEPRECATED fieldLevelEncryption "Use generic-lens or generic-optics with 'fieldLevelEncryption' instead"  #-}

-- | The fully qualified URI of the new configuration resource just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflecrrsLocation :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse (Core.Maybe Core.Text)
cflecrrsLocation = Lens.field @"location"
{-# INLINEABLE cflecrrsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflecrrsResponseStatus :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse Core.Int
cflecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cflecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
