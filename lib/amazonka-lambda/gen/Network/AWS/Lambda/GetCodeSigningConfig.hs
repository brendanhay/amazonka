{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetCodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified code signing configuration.
module Network.AWS.Lambda.GetCodeSigningConfig
  ( -- * Creating a request
    GetCodeSigningConfig (..),
    mkGetCodeSigningConfig,

    -- ** Request lenses
    gcscCodeSigningConfigArn,

    -- * Destructuring the response
    GetCodeSigningConfigResponse (..),
    mkGetCodeSigningConfigResponse,

    -- ** Response lenses
    gcscrrsCodeSigningConfig,
    gcscrrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCodeSigningConfig' smart constructor.
newtype GetCodeSigningConfig = GetCodeSigningConfig'
  { -- | The The Amazon Resource Name (ARN) of the code signing configuration.
    codeSigningConfigArn :: Types.CodeSigningConfigArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCodeSigningConfig' value with any optional fields omitted.
mkGetCodeSigningConfig ::
  -- | 'codeSigningConfigArn'
  Types.CodeSigningConfigArn ->
  GetCodeSigningConfig
mkGetCodeSigningConfig codeSigningConfigArn =
  GetCodeSigningConfig' {codeSigningConfigArn}

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcscCodeSigningConfigArn :: Lens.Lens' GetCodeSigningConfig Types.CodeSigningConfigArn
gcscCodeSigningConfigArn = Lens.field @"codeSigningConfigArn"
{-# DEPRECATED gcscCodeSigningConfigArn "Use generic-lens or generic-optics with 'codeSigningConfigArn' instead." #-}

instance Core.AWSRequest GetCodeSigningConfig where
  type Rs GetCodeSigningConfig = GetCodeSigningConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2020-04-22/code-signing-configs/"
                Core.<> (Core.toText codeSigningConfigArn)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCodeSigningConfigResponse'
            Core.<$> (x Core..: "CodeSigningConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCodeSigningConfigResponse' smart constructor.
data GetCodeSigningConfigResponse = GetCodeSigningConfigResponse'
  { -- | The code signing configuration
    codeSigningConfig :: Types.CodeSigningConfig,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCodeSigningConfigResponse' value with any optional fields omitted.
mkGetCodeSigningConfigResponse ::
  -- | 'codeSigningConfig'
  Types.CodeSigningConfig ->
  -- | 'responseStatus'
  Core.Int ->
  GetCodeSigningConfigResponse
mkGetCodeSigningConfigResponse codeSigningConfig responseStatus =
  GetCodeSigningConfigResponse' {codeSigningConfig, responseStatus}

-- | The code signing configuration
--
-- /Note:/ Consider using 'codeSigningConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcscrrsCodeSigningConfig :: Lens.Lens' GetCodeSigningConfigResponse Types.CodeSigningConfig
gcscrrsCodeSigningConfig = Lens.field @"codeSigningConfig"
{-# DEPRECATED gcscrrsCodeSigningConfig "Use generic-lens or generic-optics with 'codeSigningConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcscrrsResponseStatus :: Lens.Lens' GetCodeSigningConfigResponse Core.Int
gcscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
