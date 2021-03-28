{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a field-level encryption profile. 
module Network.AWS.CloudFront.UpdateFieldLevelEncryptionProfile
    (
    -- * Creating a request
      UpdateFieldLevelEncryptionProfile (..)
    , mkUpdateFieldLevelEncryptionProfile
    -- ** Request lenses
    , uflepFieldLevelEncryptionProfileConfig
    , uflepId
    , uflepIfMatch

    -- * Destructuring the response
    , UpdateFieldLevelEncryptionProfileResponse (..)
    , mkUpdateFieldLevelEncryptionProfileResponse
    -- ** Response lenses
    , ufleprrsETag
    , ufleprrsFieldLevelEncryptionProfile
    , ufleprrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateFieldLevelEncryptionProfile' smart constructor.
data UpdateFieldLevelEncryptionProfile = UpdateFieldLevelEncryptionProfile'
  { fieldLevelEncryptionProfileConfig :: Types.FieldLevelEncryptionProfileConfig
    -- ^ Request to update a field-level encryption profile. 
  , id :: Core.Text
    -- ^ The ID of the field-level encryption profile request. 
  , ifMatch :: Core.Maybe Core.Text
    -- ^ The value of the @ETag@ header that you received when retrieving the profile identity to update. For example: @E2QWRUHAPOMQZL@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFieldLevelEncryptionProfile' value with any optional fields omitted.
mkUpdateFieldLevelEncryptionProfile
    :: Types.FieldLevelEncryptionProfileConfig -- ^ 'fieldLevelEncryptionProfileConfig'
    -> Core.Text -- ^ 'id'
    -> UpdateFieldLevelEncryptionProfile
mkUpdateFieldLevelEncryptionProfile
  fieldLevelEncryptionProfileConfig id
  = UpdateFieldLevelEncryptionProfile'{fieldLevelEncryptionProfileConfig,
                                       id, ifMatch = Core.Nothing}

-- | Request to update a field-level encryption profile. 
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflepFieldLevelEncryptionProfileConfig :: Lens.Lens' UpdateFieldLevelEncryptionProfile Types.FieldLevelEncryptionProfileConfig
uflepFieldLevelEncryptionProfileConfig = Lens.field @"fieldLevelEncryptionProfileConfig"
{-# INLINEABLE uflepFieldLevelEncryptionProfileConfig #-}
{-# DEPRECATED fieldLevelEncryptionProfileConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfileConfig' instead"  #-}

-- | The ID of the field-level encryption profile request. 
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflepId :: Lens.Lens' UpdateFieldLevelEncryptionProfile Core.Text
uflepId = Lens.field @"id"
{-# INLINEABLE uflepId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The value of the @ETag@ header that you received when retrieving the profile identity to update. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflepIfMatch :: Lens.Lens' UpdateFieldLevelEncryptionProfile (Core.Maybe Core.Text)
uflepIfMatch = Lens.field @"ifMatch"
{-# INLINEABLE uflepIfMatch #-}
{-# DEPRECATED ifMatch "Use generic-lens or generic-optics with 'ifMatch' instead"  #-}

instance Core.ToQuery UpdateFieldLevelEncryptionProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateFieldLevelEncryptionProfile where
        toHeaders UpdateFieldLevelEncryptionProfile{..}
          = Core.toHeaders "If-Match" ifMatch

instance Core.AWSRequest UpdateFieldLevelEncryptionProfile where
        type Rs UpdateFieldLevelEncryptionProfile =
             UpdateFieldLevelEncryptionProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/2020-05-31/field-level-encryption-profile/" Core.<>
                             Core.toText id
                             Core.<> "/config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 UpdateFieldLevelEncryptionProfileResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*> Core.parseXML x Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateFieldLevelEncryptionProfileResponse' smart constructor.
data UpdateFieldLevelEncryptionProfileResponse = UpdateFieldLevelEncryptionProfileResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The result of the field-level encryption profile request. 
  , fieldLevelEncryptionProfile :: Core.Maybe Types.FieldLevelEncryptionProfile
    -- ^ Return the results of updating the profile.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateFieldLevelEncryptionProfileResponse' value with any optional fields omitted.
mkUpdateFieldLevelEncryptionProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateFieldLevelEncryptionProfileResponse
mkUpdateFieldLevelEncryptionProfileResponse responseStatus
  = UpdateFieldLevelEncryptionProfileResponse'{eTag = Core.Nothing,
                                               fieldLevelEncryptionProfile = Core.Nothing,
                                               responseStatus}

-- | The result of the field-level encryption profile request. 
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufleprrsETag :: Lens.Lens' UpdateFieldLevelEncryptionProfileResponse (Core.Maybe Core.Text)
ufleprrsETag = Lens.field @"eTag"
{-# INLINEABLE ufleprrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | Return the results of updating the profile.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufleprrsFieldLevelEncryptionProfile :: Lens.Lens' UpdateFieldLevelEncryptionProfileResponse (Core.Maybe Types.FieldLevelEncryptionProfile)
ufleprrsFieldLevelEncryptionProfile = Lens.field @"fieldLevelEncryptionProfile"
{-# INLINEABLE ufleprrsFieldLevelEncryptionProfile #-}
{-# DEPRECATED fieldLevelEncryptionProfile "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfile' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufleprrsResponseStatus :: Lens.Lens' UpdateFieldLevelEncryptionProfileResponse Core.Int
ufleprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ufleprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
