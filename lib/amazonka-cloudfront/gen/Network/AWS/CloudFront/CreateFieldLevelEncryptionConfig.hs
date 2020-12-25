{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateFieldLevelEncryptionConfig (..),
    mkCreateFieldLevelEncryptionConfig,

    -- ** Request lenses
    cflecFieldLevelEncryptionConfig,

    -- * Destructuring the response
    CreateFieldLevelEncryptionConfigResponse (..),
    mkCreateFieldLevelEncryptionConfigResponse,

    -- ** Response lenses
    cflecrrsETag,
    cflecrrsFieldLevelEncryption,
    cflecrrsLocation,
    cflecrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateFieldLevelEncryptionConfig' smart constructor.
newtype CreateFieldLevelEncryptionConfig = CreateFieldLevelEncryptionConfig'
  { -- | The request to create a new field-level encryption configuration.
    fieldLevelEncryptionConfig :: Types.FieldLevelEncryptionConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFieldLevelEncryptionConfig' value with any optional fields omitted.
mkCreateFieldLevelEncryptionConfig ::
  -- | 'fieldLevelEncryptionConfig'
  Types.FieldLevelEncryptionConfig ->
  CreateFieldLevelEncryptionConfig
mkCreateFieldLevelEncryptionConfig fieldLevelEncryptionConfig =
  CreateFieldLevelEncryptionConfig' {fieldLevelEncryptionConfig}

-- | The request to create a new field-level encryption configuration.
--
-- /Note:/ Consider using 'fieldLevelEncryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflecFieldLevelEncryptionConfig :: Lens.Lens' CreateFieldLevelEncryptionConfig Types.FieldLevelEncryptionConfig
cflecFieldLevelEncryptionConfig = Lens.field @"fieldLevelEncryptionConfig"
{-# DEPRECATED cflecFieldLevelEncryptionConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionConfig' instead." #-}

instance Core.AWSRequest CreateFieldLevelEncryptionConfig where
  type
    Rs CreateFieldLevelEncryptionConfig =
      CreateFieldLevelEncryptionConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2020-05-31/field-level-encryption",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateFieldLevelEncryptionConfigResponse'
            Core.<$> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.parseHeaderMaybe "Location" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateFieldLevelEncryptionConfigResponse' smart constructor.
data CreateFieldLevelEncryptionConfigResponse = CreateFieldLevelEncryptionConfigResponse'
  { -- | The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Core.Maybe Types.ETag,
    -- | Returned when you create a new field-level encryption configuration.
    fieldLevelEncryption :: Core.Maybe Types.FieldLevelEncryption,
    -- | The fully qualified URI of the new configuration resource just created.
    location :: Core.Maybe Types.Location,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateFieldLevelEncryptionConfigResponse' value with any optional fields omitted.
mkCreateFieldLevelEncryptionConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateFieldLevelEncryptionConfigResponse
mkCreateFieldLevelEncryptionConfigResponse responseStatus =
  CreateFieldLevelEncryptionConfigResponse'
    { eTag = Core.Nothing,
      fieldLevelEncryption = Core.Nothing,
      location = Core.Nothing,
      responseStatus
    }

-- | The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflecrrsETag :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse (Core.Maybe Types.ETag)
cflecrrsETag = Lens.field @"eTag"
{-# DEPRECATED cflecrrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Returned when you create a new field-level encryption configuration.
--
-- /Note:/ Consider using 'fieldLevelEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflecrrsFieldLevelEncryption :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse (Core.Maybe Types.FieldLevelEncryption)
cflecrrsFieldLevelEncryption = Lens.field @"fieldLevelEncryption"
{-# DEPRECATED cflecrrsFieldLevelEncryption "Use generic-lens or generic-optics with 'fieldLevelEncryption' instead." #-}

-- | The fully qualified URI of the new configuration resource just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflecrrsLocation :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse (Core.Maybe Types.Location)
cflecrrsLocation = Lens.field @"location"
{-# DEPRECATED cflecrrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflecrrsResponseStatus :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse Core.Int
cflecrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cflecrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
