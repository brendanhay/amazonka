{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption configuration information.
module Network.AWS.CloudFront.GetFieldLevelEncryptionConfig
    (
    -- * Creating a request
      GetFieldLevelEncryptionConfig (..)
    , mkGetFieldLevelEncryptionConfig
    -- ** Request lenses
    , gflecId

    -- * Destructuring the response
    , GetFieldLevelEncryptionConfigResponse (..)
    , mkGetFieldLevelEncryptionConfigResponse
    -- ** Response lenses
    , gflecrrsETag
    , gflecrrsFieldLevelEncryptionConfig
    , gflecrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFieldLevelEncryptionConfig' smart constructor.
newtype GetFieldLevelEncryptionConfig = GetFieldLevelEncryptionConfig'
  { id :: Core.Text
    -- ^ Request the ID for the field-level encryption configuration information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetFieldLevelEncryptionConfig' value with any optional fields omitted.
mkGetFieldLevelEncryptionConfig
    :: Core.Text -- ^ 'id'
    -> GetFieldLevelEncryptionConfig
mkGetFieldLevelEncryptionConfig id
  = GetFieldLevelEncryptionConfig'{id}

-- | Request the ID for the field-level encryption configuration information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflecId :: Lens.Lens' GetFieldLevelEncryptionConfig Core.Text
gflecId = Lens.field @"id"
{-# INLINEABLE gflecId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetFieldLevelEncryptionConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetFieldLevelEncryptionConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetFieldLevelEncryptionConfig where
        type Rs GetFieldLevelEncryptionConfig =
             GetFieldLevelEncryptionConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-05-31/field-level-encryption/" Core.<> Core.toText id
                             Core.<> "/config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetFieldLevelEncryptionConfigResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*> Core.parseXML x Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetFieldLevelEncryptionConfigResponse' smart constructor.
data GetFieldLevelEncryptionConfigResponse = GetFieldLevelEncryptionConfigResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
  , fieldLevelEncryptionConfig :: Core.Maybe Types.FieldLevelEncryptionConfig
    -- ^ Return the field-level encryption configuration information.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFieldLevelEncryptionConfigResponse' value with any optional fields omitted.
mkGetFieldLevelEncryptionConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetFieldLevelEncryptionConfigResponse
mkGetFieldLevelEncryptionConfigResponse responseStatus
  = GetFieldLevelEncryptionConfigResponse'{eTag = Core.Nothing,
                                           fieldLevelEncryptionConfig = Core.Nothing,
                                           responseStatus}

-- | The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflecrrsETag :: Lens.Lens' GetFieldLevelEncryptionConfigResponse (Core.Maybe Core.Text)
gflecrrsETag = Lens.field @"eTag"
{-# INLINEABLE gflecrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | Return the field-level encryption configuration information.
--
-- /Note:/ Consider using 'fieldLevelEncryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflecrrsFieldLevelEncryptionConfig :: Lens.Lens' GetFieldLevelEncryptionConfigResponse (Core.Maybe Types.FieldLevelEncryptionConfig)
gflecrrsFieldLevelEncryptionConfig = Lens.field @"fieldLevelEncryptionConfig"
{-# INLINEABLE gflecrrsFieldLevelEncryptionConfig #-}
{-# DEPRECATED fieldLevelEncryptionConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflecrrsResponseStatus :: Lens.Lens' GetFieldLevelEncryptionConfigResponse Core.Int
gflecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gflecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
