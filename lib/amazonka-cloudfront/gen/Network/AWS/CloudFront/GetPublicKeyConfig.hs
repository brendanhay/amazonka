{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetPublicKeyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a public key configuration.
module Network.AWS.CloudFront.GetPublicKeyConfig
  ( -- * Creating a request
    GetPublicKeyConfig (..),
    mkGetPublicKeyConfig,

    -- ** Request lenses
    gpkcId,

    -- * Destructuring the response
    GetPublicKeyConfigResponse (..),
    mkGetPublicKeyConfigResponse,

    -- ** Response lenses
    gpkcrrsETag,
    gpkcrrsPublicKeyConfig,
    gpkcrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPublicKeyConfig' smart constructor.
newtype GetPublicKeyConfig = GetPublicKeyConfig'
  { -- | The identifier of the public key whose configuration you are getting.
    id :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetPublicKeyConfig' value with any optional fields omitted.
mkGetPublicKeyConfig ::
  -- | 'id'
  Types.String ->
  GetPublicKeyConfig
mkGetPublicKeyConfig id = GetPublicKeyConfig' {id}

-- | The identifier of the public key whose configuration you are getting.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkcId :: Lens.Lens' GetPublicKeyConfig Types.String
gpkcId = Lens.field @"id"
{-# DEPRECATED gpkcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest GetPublicKeyConfig where
  type Rs GetPublicKeyConfig = GetPublicKeyConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2020-05-31/public-key/" Core.<> (Core.toText id)
                Core.<> ("/config")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetPublicKeyConfigResponse'
            Core.<$> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetPublicKeyConfigResponse' smart constructor.
data GetPublicKeyConfigResponse = GetPublicKeyConfigResponse'
  { -- | The identifier for this version of the public key configuration.
    eTag :: Core.Maybe Types.String,
    -- | A public key configuration.
    publicKeyConfig :: Core.Maybe Types.PublicKeyConfig,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPublicKeyConfigResponse' value with any optional fields omitted.
mkGetPublicKeyConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetPublicKeyConfigResponse
mkGetPublicKeyConfigResponse responseStatus =
  GetPublicKeyConfigResponse'
    { eTag = Core.Nothing,
      publicKeyConfig = Core.Nothing,
      responseStatus
    }

-- | The identifier for this version of the public key configuration.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkcrrsETag :: Lens.Lens' GetPublicKeyConfigResponse (Core.Maybe Types.String)
gpkcrrsETag = Lens.field @"eTag"
{-# DEPRECATED gpkcrrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | A public key configuration.
--
-- /Note:/ Consider using 'publicKeyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkcrrsPublicKeyConfig :: Lens.Lens' GetPublicKeyConfigResponse (Core.Maybe Types.PublicKeyConfig)
gpkcrrsPublicKeyConfig = Lens.field @"publicKeyConfig"
{-# DEPRECATED gpkcrrsPublicKeyConfig "Use generic-lens or generic-optics with 'publicKeyConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpkcrrsResponseStatus :: Lens.Lens' GetPublicKeyConfigResponse Core.Int
gpkcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gpkcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
