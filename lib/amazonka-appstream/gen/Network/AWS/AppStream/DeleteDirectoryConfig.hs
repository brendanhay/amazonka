{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DeleteDirectoryConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Directory Config object from AppStream 2.0. This object includes the information required to join streaming instances to an Active Directory domain.
module Network.AWS.AppStream.DeleteDirectoryConfig
  ( -- * Creating a request
    DeleteDirectoryConfig (..),
    mkDeleteDirectoryConfig,

    -- ** Request lenses
    ddcDirectoryName,

    -- * Destructuring the response
    DeleteDirectoryConfigResponse (..),
    mkDeleteDirectoryConfigResponse,

    -- ** Response lenses
    ddcrfrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDirectoryConfig' smart constructor.
newtype DeleteDirectoryConfig = DeleteDirectoryConfig'
  { -- | The name of the directory configuration.
    directoryName :: Types.DirectoryName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDirectoryConfig' value with any optional fields omitted.
mkDeleteDirectoryConfig ::
  -- | 'directoryName'
  Types.DirectoryName ->
  DeleteDirectoryConfig
mkDeleteDirectoryConfig directoryName =
  DeleteDirectoryConfig' {directoryName}

-- | The name of the directory configuration.
--
-- /Note:/ Consider using 'directoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDirectoryName :: Lens.Lens' DeleteDirectoryConfig Types.DirectoryName
ddcDirectoryName = Lens.field @"directoryName"
{-# DEPRECATED ddcDirectoryName "Use generic-lens or generic-optics with 'directoryName' instead." #-}

instance Core.FromJSON DeleteDirectoryConfig where
  toJSON DeleteDirectoryConfig {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("DirectoryName" Core..= directoryName)]
      )

instance Core.AWSRequest DeleteDirectoryConfig where
  type Rs DeleteDirectoryConfig = DeleteDirectoryConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "PhotonAdminProxyService.DeleteDirectoryConfig")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDirectoryConfigResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDirectoryConfigResponse' smart constructor.
newtype DeleteDirectoryConfigResponse = DeleteDirectoryConfigResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDirectoryConfigResponse' value with any optional fields omitted.
mkDeleteDirectoryConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDirectoryConfigResponse
mkDeleteDirectoryConfigResponse responseStatus =
  DeleteDirectoryConfigResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrfrsResponseStatus :: Lens.Lens' DeleteDirectoryConfigResponse Core.Int
ddcrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddcrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
