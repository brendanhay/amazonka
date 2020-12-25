{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.GetDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata about a directory.
module Network.AWS.CloudDirectory.GetDirectory
  ( -- * Creating a request
    GetDirectory (..),
    mkGetDirectory,

    -- ** Request lenses
    gdDirectoryArn,

    -- * Destructuring the response
    GetDirectoryResponse (..),
    mkGetDirectoryResponse,

    -- ** Response lenses
    gdrrsDirectory,
    gdrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDirectory' smart constructor.
newtype GetDirectory = GetDirectory'
  { -- | The ARN of the directory.
    directoryArn :: Types.DirectoryArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDirectory' value with any optional fields omitted.
mkGetDirectory ::
  -- | 'directoryArn'
  Types.DirectoryArn ->
  GetDirectory
mkGetDirectory directoryArn = GetDirectory' {directoryArn}

-- | The ARN of the directory.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDirectoryArn :: Lens.Lens' GetDirectory Types.DirectoryArn
gdDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED gdDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

instance Core.FromJSON GetDirectory where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetDirectory where
  type Rs GetDirectory = GetDirectoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/directory/get",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDirectoryResponse'
            Core.<$> (x Core..: "Directory") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDirectoryResponse' smart constructor.
data GetDirectoryResponse = GetDirectoryResponse'
  { -- | Metadata about the directory.
    directory :: Types.Directory,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDirectoryResponse' value with any optional fields omitted.
mkGetDirectoryResponse ::
  -- | 'directory'
  Types.Directory ->
  -- | 'responseStatus'
  Core.Int ->
  GetDirectoryResponse
mkGetDirectoryResponse directory responseStatus =
  GetDirectoryResponse' {directory, responseStatus}

-- | Metadata about the directory.
--
-- /Note:/ Consider using 'directory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDirectory :: Lens.Lens' GetDirectoryResponse Types.Directory
gdrrsDirectory = Lens.field @"directory"
{-# DEPRECATED gdrrsDirectory "Use generic-lens or generic-optics with 'directory' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDirectoryResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
