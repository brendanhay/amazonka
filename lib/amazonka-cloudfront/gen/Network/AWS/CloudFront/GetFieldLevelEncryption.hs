{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetFieldLevelEncryption (..),
    mkGetFieldLevelEncryption,

    -- ** Request lenses
    gfleId,

    -- * Destructuring the response
    GetFieldLevelEncryptionResponse (..),
    mkGetFieldLevelEncryptionResponse,

    -- ** Response lenses
    gflerrsETag,
    gflerrsFieldLevelEncryption,
    gflerrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFieldLevelEncryption' smart constructor.
newtype GetFieldLevelEncryption = GetFieldLevelEncryption'
  { -- | Request the ID for the field-level encryption configuration information.
    id :: Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetFieldLevelEncryption' value with any optional fields omitted.
mkGetFieldLevelEncryption ::
  -- | 'id'
  Types.Id ->
  GetFieldLevelEncryption
mkGetFieldLevelEncryption id = GetFieldLevelEncryption' {id}

-- | Request the ID for the field-level encryption configuration information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfleId :: Lens.Lens' GetFieldLevelEncryption Types.Id
gfleId = Lens.field @"id"
{-# DEPRECATED gfleId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest GetFieldLevelEncryption where
  type Rs GetFieldLevelEncryption = GetFieldLevelEncryptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/2020-05-31/field-level-encryption/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetFieldLevelEncryptionResponse'
            Core.<$> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetFieldLevelEncryptionResponse' smart constructor.
data GetFieldLevelEncryptionResponse = GetFieldLevelEncryptionResponse'
  { -- | The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Core.Maybe Types.ETag,
    -- | Return the field-level encryption configuration information.
    fieldLevelEncryption :: Core.Maybe Types.FieldLevelEncryption,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetFieldLevelEncryptionResponse' value with any optional fields omitted.
mkGetFieldLevelEncryptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetFieldLevelEncryptionResponse
mkGetFieldLevelEncryptionResponse responseStatus =
  GetFieldLevelEncryptionResponse'
    { eTag = Core.Nothing,
      fieldLevelEncryption = Core.Nothing,
      responseStatus
    }

-- | The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflerrsETag :: Lens.Lens' GetFieldLevelEncryptionResponse (Core.Maybe Types.ETag)
gflerrsETag = Lens.field @"eTag"
{-# DEPRECATED gflerrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Return the field-level encryption configuration information.
--
-- /Note:/ Consider using 'fieldLevelEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflerrsFieldLevelEncryption :: Lens.Lens' GetFieldLevelEncryptionResponse (Core.Maybe Types.FieldLevelEncryption)
gflerrsFieldLevelEncryption = Lens.field @"fieldLevelEncryption"
{-# DEPRECATED gflerrsFieldLevelEncryption "Use generic-lens or generic-optics with 'fieldLevelEncryption' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflerrsResponseStatus :: Lens.Lens' GetFieldLevelEncryptionResponse Core.Int
gflerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gflerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
