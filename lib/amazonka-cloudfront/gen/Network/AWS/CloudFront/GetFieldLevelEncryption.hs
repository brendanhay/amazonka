{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetFieldLevelEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption configuration information.
module Network.AWS.CloudFront.GetFieldLevelEncryption
    (
    -- * Creating a request
      GetFieldLevelEncryption (..)
    , mkGetFieldLevelEncryption
    -- ** Request lenses
    , gfleId

    -- * Destructuring the response
    , GetFieldLevelEncryptionResponse (..)
    , mkGetFieldLevelEncryptionResponse
    -- ** Response lenses
    , gflerrsETag
    , gflerrsFieldLevelEncryption
    , gflerrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFieldLevelEncryption' smart constructor.
newtype GetFieldLevelEncryption = GetFieldLevelEncryption'
  { id :: Core.Text
    -- ^ Request the ID for the field-level encryption configuration information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetFieldLevelEncryption' value with any optional fields omitted.
mkGetFieldLevelEncryption
    :: Core.Text -- ^ 'id'
    -> GetFieldLevelEncryption
mkGetFieldLevelEncryption id = GetFieldLevelEncryption'{id}

-- | Request the ID for the field-level encryption configuration information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfleId :: Lens.Lens' GetFieldLevelEncryption Core.Text
gfleId = Lens.field @"id"
{-# INLINEABLE gfleId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetFieldLevelEncryption where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetFieldLevelEncryption where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetFieldLevelEncryption where
        type Rs GetFieldLevelEncryption = GetFieldLevelEncryptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-05-31/field-level-encryption/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetFieldLevelEncryptionResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*> Core.parseXML x Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetFieldLevelEncryptionResponse' smart constructor.
data GetFieldLevelEncryptionResponse = GetFieldLevelEncryptionResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
  , fieldLevelEncryption :: Core.Maybe Types.FieldLevelEncryption
    -- ^ Return the field-level encryption configuration information.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetFieldLevelEncryptionResponse' value with any optional fields omitted.
mkGetFieldLevelEncryptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetFieldLevelEncryptionResponse
mkGetFieldLevelEncryptionResponse responseStatus
  = GetFieldLevelEncryptionResponse'{eTag = Core.Nothing,
                                     fieldLevelEncryption = Core.Nothing, responseStatus}

-- | The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflerrsETag :: Lens.Lens' GetFieldLevelEncryptionResponse (Core.Maybe Core.Text)
gflerrsETag = Lens.field @"eTag"
{-# INLINEABLE gflerrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | Return the field-level encryption configuration information.
--
-- /Note:/ Consider using 'fieldLevelEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflerrsFieldLevelEncryption :: Lens.Lens' GetFieldLevelEncryptionResponse (Core.Maybe Types.FieldLevelEncryption)
gflerrsFieldLevelEncryption = Lens.field @"fieldLevelEncryption"
{-# INLINEABLE gflerrsFieldLevelEncryption #-}
{-# DEPRECATED fieldLevelEncryption "Use generic-lens or generic-optics with 'fieldLevelEncryption' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflerrsResponseStatus :: Lens.Lens' GetFieldLevelEncryptionResponse Core.Int
gflerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gflerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
