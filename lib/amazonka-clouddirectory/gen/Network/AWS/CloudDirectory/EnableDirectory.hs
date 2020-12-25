{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.EnableDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified directory. Only disabled directories can be enabled. Once enabled, the directory can then be read and written to.
module Network.AWS.CloudDirectory.EnableDirectory
  ( -- * Creating a request
    EnableDirectory (..),
    mkEnableDirectory,

    -- ** Request lenses
    edDirectoryArn,

    -- * Destructuring the response
    EnableDirectoryResponse (..),
    mkEnableDirectoryResponse,

    -- ** Response lenses
    edrrsDirectoryArn,
    edrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableDirectory' smart constructor.
newtype EnableDirectory = EnableDirectory'
  { -- | The ARN of the directory to enable.
    directoryArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableDirectory' value with any optional fields omitted.
mkEnableDirectory ::
  -- | 'directoryArn'
  Types.Arn ->
  EnableDirectory
mkEnableDirectory directoryArn = EnableDirectory' {directoryArn}

-- | The ARN of the directory to enable.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edDirectoryArn :: Lens.Lens' EnableDirectory Types.Arn
edDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED edDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

instance Core.FromJSON EnableDirectory where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest EnableDirectory where
  type Rs EnableDirectory = EnableDirectoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/directory/enable",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableDirectoryResponse'
            Core.<$> (x Core..: "DirectoryArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkEnableDirectoryResponse' smart constructor.
data EnableDirectoryResponse = EnableDirectoryResponse'
  { -- | The ARN of the enabled directory.
    directoryArn :: Types.Arn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableDirectoryResponse' value with any optional fields omitted.
mkEnableDirectoryResponse ::
  -- | 'directoryArn'
  Types.Arn ->
  -- | 'responseStatus'
  Core.Int ->
  EnableDirectoryResponse
mkEnableDirectoryResponse directoryArn responseStatus =
  EnableDirectoryResponse' {directoryArn, responseStatus}

-- | The ARN of the enabled directory.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edrrsDirectoryArn :: Lens.Lens' EnableDirectoryResponse Types.Arn
edrrsDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED edrrsDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edrrsResponseStatus :: Lens.Lens' EnableDirectoryResponse Core.Int
edrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED edrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
