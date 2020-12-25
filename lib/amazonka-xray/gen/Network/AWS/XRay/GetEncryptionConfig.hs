{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current encryption configuration for X-Ray data.
module Network.AWS.XRay.GetEncryptionConfig
  ( -- * Creating a request
    GetEncryptionConfig (..),
    mkGetEncryptionConfig,

    -- * Destructuring the response
    GetEncryptionConfigResponse (..),
    mkGetEncryptionConfigResponse,

    -- ** Response lenses
    gecrrsEncryptionConfig,
    gecrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkGetEncryptionConfig' smart constructor.
data GetEncryptionConfig = GetEncryptionConfig'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetEncryptionConfig' value with any optional fields omitted.
mkGetEncryptionConfig ::
  GetEncryptionConfig
mkGetEncryptionConfig = GetEncryptionConfig'

instance Core.FromJSON GetEncryptionConfig where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetEncryptionConfig where
  type Rs GetEncryptionConfig = GetEncryptionConfigResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/EncryptionConfig",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEncryptionConfigResponse'
            Core.<$> (x Core..:? "EncryptionConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetEncryptionConfigResponse' smart constructor.
data GetEncryptionConfigResponse = GetEncryptionConfigResponse'
  { -- | The encryption configuration document.
    encryptionConfig :: Core.Maybe Types.EncryptionConfig,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetEncryptionConfigResponse' value with any optional fields omitted.
mkGetEncryptionConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetEncryptionConfigResponse
mkGetEncryptionConfigResponse responseStatus =
  GetEncryptionConfigResponse'
    { encryptionConfig = Core.Nothing,
      responseStatus
    }

-- | The encryption configuration document.
--
-- /Note:/ Consider using 'encryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gecrrsEncryptionConfig :: Lens.Lens' GetEncryptionConfigResponse (Core.Maybe Types.EncryptionConfig)
gecrrsEncryptionConfig = Lens.field @"encryptionConfig"
{-# DEPRECATED gecrrsEncryptionConfig "Use generic-lens or generic-optics with 'encryptionConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gecrrsResponseStatus :: Lens.Lens' GetEncryptionConfigResponse Core.Int
gecrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gecrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
