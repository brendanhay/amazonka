{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateFieldLevelEncryptionProfile (..),
    mkUpdateFieldLevelEncryptionProfile,

    -- ** Request lenses
    uflepFieldLevelEncryptionProfileConfig,
    uflepId,
    uflepIfMatch,

    -- * Destructuring the response
    UpdateFieldLevelEncryptionProfileResponse (..),
    mkUpdateFieldLevelEncryptionProfileResponse,

    -- ** Response lenses
    ufleprrsETag,
    ufleprrsFieldLevelEncryptionProfile,
    ufleprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateFieldLevelEncryptionProfile' smart constructor.
data UpdateFieldLevelEncryptionProfile = UpdateFieldLevelEncryptionProfile'
  { -- | Request to update a field-level encryption profile.
    fieldLevelEncryptionProfileConfig :: Types.FieldLevelEncryptionProfileConfig,
    -- | The ID of the field-level encryption profile request.
    id :: Types.String,
    -- | The value of the @ETag@ header that you received when retrieving the profile identity to update. For example: @E2QWRUHAPOMQZL@ .
    ifMatch :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFieldLevelEncryptionProfile' value with any optional fields omitted.
mkUpdateFieldLevelEncryptionProfile ::
  -- | 'fieldLevelEncryptionProfileConfig'
  Types.FieldLevelEncryptionProfileConfig ->
  -- | 'id'
  Types.String ->
  UpdateFieldLevelEncryptionProfile
mkUpdateFieldLevelEncryptionProfile
  fieldLevelEncryptionProfileConfig
  id =
    UpdateFieldLevelEncryptionProfile'
      { fieldLevelEncryptionProfileConfig,
        id,
        ifMatch = Core.Nothing
      }

-- | Request to update a field-level encryption profile.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflepFieldLevelEncryptionProfileConfig :: Lens.Lens' UpdateFieldLevelEncryptionProfile Types.FieldLevelEncryptionProfileConfig
uflepFieldLevelEncryptionProfileConfig = Lens.field @"fieldLevelEncryptionProfileConfig"
{-# DEPRECATED uflepFieldLevelEncryptionProfileConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfileConfig' instead." #-}

-- | The ID of the field-level encryption profile request.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflepId :: Lens.Lens' UpdateFieldLevelEncryptionProfile Types.String
uflepId = Lens.field @"id"
{-# DEPRECATED uflepId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The value of the @ETag@ header that you received when retrieving the profile identity to update. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uflepIfMatch :: Lens.Lens' UpdateFieldLevelEncryptionProfile (Core.Maybe Types.String)
uflepIfMatch = Lens.field @"ifMatch"
{-# DEPRECATED uflepIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

instance Core.AWSRequest UpdateFieldLevelEncryptionProfile where
  type
    Rs UpdateFieldLevelEncryptionProfile =
      UpdateFieldLevelEncryptionProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/2020-05-31/field-level-encryption-profile/"
                Core.<> (Core.toText id)
                Core.<> ("/config")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "If-Match" ifMatch,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateFieldLevelEncryptionProfileResponse'
            Core.<$> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateFieldLevelEncryptionProfileResponse' smart constructor.
data UpdateFieldLevelEncryptionProfileResponse = UpdateFieldLevelEncryptionProfileResponse'
  { -- | The result of the field-level encryption profile request.
    eTag :: Core.Maybe Types.String,
    -- | Return the results of updating the profile.
    fieldLevelEncryptionProfile :: Core.Maybe Types.FieldLevelEncryptionProfile,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateFieldLevelEncryptionProfileResponse' value with any optional fields omitted.
mkUpdateFieldLevelEncryptionProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateFieldLevelEncryptionProfileResponse
mkUpdateFieldLevelEncryptionProfileResponse responseStatus =
  UpdateFieldLevelEncryptionProfileResponse'
    { eTag = Core.Nothing,
      fieldLevelEncryptionProfile = Core.Nothing,
      responseStatus
    }

-- | The result of the field-level encryption profile request.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufleprrsETag :: Lens.Lens' UpdateFieldLevelEncryptionProfileResponse (Core.Maybe Types.String)
ufleprrsETag = Lens.field @"eTag"
{-# DEPRECATED ufleprrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Return the results of updating the profile.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufleprrsFieldLevelEncryptionProfile :: Lens.Lens' UpdateFieldLevelEncryptionProfileResponse (Core.Maybe Types.FieldLevelEncryptionProfile)
ufleprrsFieldLevelEncryptionProfile = Lens.field @"fieldLevelEncryptionProfile"
{-# DEPRECATED ufleprrsFieldLevelEncryptionProfile "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufleprrsResponseStatus :: Lens.Lens' UpdateFieldLevelEncryptionProfileResponse Core.Int
ufleprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ufleprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
