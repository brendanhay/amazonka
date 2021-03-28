{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption profile information.
module Network.AWS.CloudFront.GetFieldLevelEncryptionProfile
    (
    -- * Creating a request
      GetFieldLevelEncryptionProfile (..)
    , mkGetFieldLevelEncryptionProfile
    -- ** Request lenses
    , gflepId

    -- * Destructuring the response
    , GetFieldLevelEncryptionProfileResponse (..)
    , mkGetFieldLevelEncryptionProfileResponse
    -- ** Response lenses
    , gfleprrsETag
    , gfleprrsFieldLevelEncryptionProfile
    , gfleprrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFieldLevelEncryptionProfile' smart constructor.
newtype GetFieldLevelEncryptionProfile = GetFieldLevelEncryptionProfile'
  { id :: Core.Text
    -- ^ Get the ID for the field-level encryption profile information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetFieldLevelEncryptionProfile' value with any optional fields omitted.
mkGetFieldLevelEncryptionProfile
    :: Core.Text -- ^ 'id'
    -> GetFieldLevelEncryptionProfile
mkGetFieldLevelEncryptionProfile id
  = GetFieldLevelEncryptionProfile'{id}

-- | Get the ID for the field-level encryption profile information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflepId :: Lens.Lens' GetFieldLevelEncryptionProfile Core.Text
gflepId = Lens.field @"id"
{-# INLINEABLE gflepId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetFieldLevelEncryptionProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetFieldLevelEncryptionProfile where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetFieldLevelEncryptionProfile where
        type Rs GetFieldLevelEncryptionProfile =
             GetFieldLevelEncryptionProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-05-31/field-level-encryption-profile/" Core.<>
                             Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetFieldLevelEncryptionProfileResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*> Core.parseXML x Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetFieldLevelEncryptionProfileResponse' smart constructor.
data GetFieldLevelEncryptionProfileResponse = GetFieldLevelEncryptionProfileResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The current version of the field level encryption profile. For example: @E2QWRUHAPOMQZL@ .
  , fieldLevelEncryptionProfile :: Core.Maybe Types.FieldLevelEncryptionProfile
    -- ^ Return the field-level encryption profile information.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetFieldLevelEncryptionProfileResponse' value with any optional fields omitted.
mkGetFieldLevelEncryptionProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetFieldLevelEncryptionProfileResponse
mkGetFieldLevelEncryptionProfileResponse responseStatus
  = GetFieldLevelEncryptionProfileResponse'{eTag = Core.Nothing,
                                            fieldLevelEncryptionProfile = Core.Nothing,
                                            responseStatus}

-- | The current version of the field level encryption profile. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfleprrsETag :: Lens.Lens' GetFieldLevelEncryptionProfileResponse (Core.Maybe Core.Text)
gfleprrsETag = Lens.field @"eTag"
{-# INLINEABLE gfleprrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | Return the field-level encryption profile information.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfleprrsFieldLevelEncryptionProfile :: Lens.Lens' GetFieldLevelEncryptionProfileResponse (Core.Maybe Types.FieldLevelEncryptionProfile)
gfleprrsFieldLevelEncryptionProfile = Lens.field @"fieldLevelEncryptionProfile"
{-# INLINEABLE gfleprrsFieldLevelEncryptionProfile #-}
{-# DEPRECATED fieldLevelEncryptionProfile "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfile' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfleprrsResponseStatus :: Lens.Lens' GetFieldLevelEncryptionProfileResponse Core.Int
gfleprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gfleprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
