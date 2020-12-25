{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetFieldLevelEncryptionProfileConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption profile configuration information.
module Network.AWS.CloudFront.GetFieldLevelEncryptionProfileConfig
  ( -- * Creating a request
    GetFieldLevelEncryptionProfileConfig (..),
    mkGetFieldLevelEncryptionProfileConfig,

    -- ** Request lenses
    gflepcId,

    -- * Destructuring the response
    GetFieldLevelEncryptionProfileConfigResponse (..),
    mkGetFieldLevelEncryptionProfileConfigResponse,

    -- ** Response lenses
    gflepcrrsETag,
    gflepcrrsFieldLevelEncryptionProfileConfig,
    gflepcrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFieldLevelEncryptionProfileConfig' smart constructor.
newtype GetFieldLevelEncryptionProfileConfig = GetFieldLevelEncryptionProfileConfig'
  { -- | Get the ID for the field-level encryption profile configuration information.
    id :: Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetFieldLevelEncryptionProfileConfig' value with any optional fields omitted.
mkGetFieldLevelEncryptionProfileConfig ::
  -- | 'id'
  Types.Id ->
  GetFieldLevelEncryptionProfileConfig
mkGetFieldLevelEncryptionProfileConfig id =
  GetFieldLevelEncryptionProfileConfig' {id}

-- | Get the ID for the field-level encryption profile configuration information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflepcId :: Lens.Lens' GetFieldLevelEncryptionProfileConfig Types.Id
gflepcId = Lens.field @"id"
{-# DEPRECATED gflepcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest GetFieldLevelEncryptionProfileConfig where
  type
    Rs GetFieldLevelEncryptionProfileConfig =
      GetFieldLevelEncryptionProfileConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2020-05-31/field-level-encryption-profile/"
                Core.<> (Core.toText id)
                Core.<> ("/config")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetFieldLevelEncryptionProfileConfigResponse'
            Core.<$> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetFieldLevelEncryptionProfileConfigResponse' smart constructor.
data GetFieldLevelEncryptionProfileConfigResponse = GetFieldLevelEncryptionProfileConfigResponse'
  { -- | The current version of the field-level encryption profile configuration result. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Core.Maybe Types.String,
    -- | Return the field-level encryption profile configuration information.
    fieldLevelEncryptionProfileConfig :: Core.Maybe Types.FieldLevelEncryptionProfileConfig,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFieldLevelEncryptionProfileConfigResponse' value with any optional fields omitted.
mkGetFieldLevelEncryptionProfileConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetFieldLevelEncryptionProfileConfigResponse
mkGetFieldLevelEncryptionProfileConfigResponse responseStatus =
  GetFieldLevelEncryptionProfileConfigResponse'
    { eTag =
        Core.Nothing,
      fieldLevelEncryptionProfileConfig = Core.Nothing,
      responseStatus
    }

-- | The current version of the field-level encryption profile configuration result. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflepcrrsETag :: Lens.Lens' GetFieldLevelEncryptionProfileConfigResponse (Core.Maybe Types.String)
gflepcrrsETag = Lens.field @"eTag"
{-# DEPRECATED gflepcrrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Return the field-level encryption profile configuration information.
--
-- /Note:/ Consider using 'fieldLevelEncryptionProfileConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflepcrrsFieldLevelEncryptionProfileConfig :: Lens.Lens' GetFieldLevelEncryptionProfileConfigResponse (Core.Maybe Types.FieldLevelEncryptionProfileConfig)
gflepcrrsFieldLevelEncryptionProfileConfig = Lens.field @"fieldLevelEncryptionProfileConfig"
{-# DEPRECATED gflepcrrsFieldLevelEncryptionProfileConfig "Use generic-lens or generic-optics with 'fieldLevelEncryptionProfileConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gflepcrrsResponseStatus :: Lens.Lens' GetFieldLevelEncryptionProfileConfigResponse Core.Int
gflepcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gflepcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
