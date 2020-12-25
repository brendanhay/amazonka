{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a field-level encryption profile.
module Network.AWS.CloudFront.CreateFieldLevelEncryptionProfile
  ( -- * Creating a request
    CreateFieldLevelEncryptionProfile (..),
    mkCreateFieldLevelEncryptionProfile,

    -- ** Request lenses
    cflepFieldLevelEncryptionProfileConfig,

    -- * Destructuring the response
    CreateFieldLevelEncryptionProfileResponse (..),
    mkCreateFieldLevelEncryptionProfileResponse,

    -- ** Response lenses
    cfleprrsETag,
    cfleprrsFieldLevelEncryptionProfile,
    cfleprrsLocation,
    cfleprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateFieldLevelEncryptionProfile' smart constructor.
newtype CreateFieldLevelEncryptionProfile = CreateFieldLevelEncryptionProfile'
  { -- | The request to create a field-level encryption profile.
    fieldLevelEncryptionProfileConfig :: Types.FieldLevelEncryptionProfileConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFieldLevelEncryptionProfile' value with any optional fields omitted.
mkCreateFieldLevelEncryptionProfile ::
  -- | 'fieldLevelEncryptionProfileConfig'
  Types.FieldLevelEncryptionProfileConfig ->
  CreateFieldLevelEncryptionProfile
mkCreateFieldLevelEncryptionProfile
  fieldLevelEncryptionProfileConfig =
    CreateFieldLevelEncryptionProfile' {fieldLevelEncryptionProfileConfig}

-- | The request to create a field-level encryption profile.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflepFieldLevelEncryptionProfileConfig :: Lens.Lens' CreateFieldLevelEncryptionProfile Types.FieldLevelEncryptionProfileConfig
cflepFieldLevelEncryptionProfileConfig = Lens.field @"fieldLevelEncryptionProfileConfig"
{-# DEPRECATED cflepFieldLevelEncryptionProfileConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfileConfig' instead." #-}

instance Core.AWSRequest CreateFieldLevelEncryptionProfile where
  type
    Rs CreateFieldLevelEncryptionProfile =
      CreateFieldLevelEncryptionProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/2020-05-31/field-level-encryption-profile",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateFieldLevelEncryptionProfileResponse'
            Core.<$> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.parseHeaderMaybe "Location" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateFieldLevelEncryptionProfileResponse' smart constructor.
data CreateFieldLevelEncryptionProfileResponse = CreateFieldLevelEncryptionProfileResponse'
  { -- | The current version of the field level encryption profile. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Core.Maybe Types.String,
    -- | Returned when you create a new field-level encryption profile.
    fieldLevelEncryptionProfile :: Core.Maybe Types.FieldLevelEncryptionProfile,
    -- | The fully qualified URI of the new profile resource just created.
    location :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateFieldLevelEncryptionProfileResponse' value with any optional fields omitted.
mkCreateFieldLevelEncryptionProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateFieldLevelEncryptionProfileResponse
mkCreateFieldLevelEncryptionProfileResponse responseStatus =
  CreateFieldLevelEncryptionProfileResponse'
    { eTag = Core.Nothing,
      fieldLevelEncryptionProfile = Core.Nothing,
      location = Core.Nothing,
      responseStatus
    }

-- | The current version of the field level encryption profile. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfleprrsETag :: Lens.Lens' CreateFieldLevelEncryptionProfileResponse (Core.Maybe Types.String)
cfleprrsETag = Lens.field @"eTag"
{-# DEPRECATED cfleprrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Returned when you create a new field-level encryption profile.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfleprrsFieldLevelEncryptionProfile :: Lens.Lens' CreateFieldLevelEncryptionProfileResponse (Core.Maybe Types.FieldLevelEncryptionProfile)
cfleprrsFieldLevelEncryptionProfile = Lens.field @"fieldLevelEncryptionProfile"
{-# DEPRECATED cfleprrsFieldLevelEncryptionProfile "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfile' instead." #-}

-- | The fully qualified URI of the new profile resource just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfleprrsLocation :: Lens.Lens' CreateFieldLevelEncryptionProfileResponse (Core.Maybe Types.String)
cfleprrsLocation = Lens.field @"location"
{-# DEPRECATED cfleprrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfleprrsResponseStatus :: Lens.Lens' CreateFieldLevelEncryptionProfileResponse Core.Int
cfleprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cfleprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
