{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.ConnectDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AD Connector to connect to an on-premises directory.
--
-- Before you call @ConnectDirectory@ , ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the @ConnectDirectory@ operation, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference> .
module Network.AWS.DirectoryService.ConnectDirectory
  ( -- * Creating a request
    ConnectDirectory (..),
    mkConnectDirectory,

    -- ** Request lenses
    cdName,
    cdPassword,
    cdSize,
    cdConnectSettings,
    cdDescription,
    cdShortName,
    cdTags,

    -- * Destructuring the response
    ConnectDirectoryResponse (..),
    mkConnectDirectoryResponse,

    -- ** Response lenses
    cdrrsDirectoryId,
    cdrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'ConnectDirectory' operation.
--
-- /See:/ 'mkConnectDirectory' smart constructor.
data ConnectDirectory = ConnectDirectory'
  { -- | The fully qualified name of the on-premises directory, such as @corp.example.com@ .
    name :: Types.DirectoryName,
    -- | The password for the on-premises user account.
    password :: Types.ConnectPassword,
    -- | The size of the directory.
    size :: Types.DirectorySize,
    -- | A 'DirectoryConnectSettings' object that contains additional information for the operation.
    connectSettings :: Types.DirectoryConnectSettings,
    -- | A description for the directory.
    description :: Core.Maybe Types.Description,
    -- | The NetBIOS name of the on-premises directory, such as @CORP@ .
    shortName :: Core.Maybe Types.DirectoryShortName,
    -- | The tags to be assigned to AD Connector.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectDirectory' value with any optional fields omitted.
mkConnectDirectory ::
  -- | 'name'
  Types.DirectoryName ->
  -- | 'password'
  Types.ConnectPassword ->
  -- | 'size'
  Types.DirectorySize ->
  -- | 'connectSettings'
  Types.DirectoryConnectSettings ->
  ConnectDirectory
mkConnectDirectory name password size connectSettings =
  ConnectDirectory'
    { name,
      password,
      size,
      connectSettings,
      description = Core.Nothing,
      shortName = Core.Nothing,
      tags = Core.Nothing
    }

-- | The fully qualified name of the on-premises directory, such as @corp.example.com@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdName :: Lens.Lens' ConnectDirectory Types.DirectoryName
cdName = Lens.field @"name"
{-# DEPRECATED cdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The password for the on-premises user account.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPassword :: Lens.Lens' ConnectDirectory Types.ConnectPassword
cdPassword = Lens.field @"password"
{-# DEPRECATED cdPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The size of the directory.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSize :: Lens.Lens' ConnectDirectory Types.DirectorySize
cdSize = Lens.field @"size"
{-# DEPRECATED cdSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | A 'DirectoryConnectSettings' object that contains additional information for the operation.
--
-- /Note:/ Consider using 'connectSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdConnectSettings :: Lens.Lens' ConnectDirectory Types.DirectoryConnectSettings
cdConnectSettings = Lens.field @"connectSettings"
{-# DEPRECATED cdConnectSettings "Use generic-lens or generic-optics with 'connectSettings' instead." #-}

-- | A description for the directory.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDescription :: Lens.Lens' ConnectDirectory (Core.Maybe Types.Description)
cdDescription = Lens.field @"description"
{-# DEPRECATED cdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The NetBIOS name of the on-premises directory, such as @CORP@ .
--
-- /Note:/ Consider using 'shortName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdShortName :: Lens.Lens' ConnectDirectory (Core.Maybe Types.DirectoryShortName)
cdShortName = Lens.field @"shortName"
{-# DEPRECATED cdShortName "Use generic-lens or generic-optics with 'shortName' instead." #-}

-- | The tags to be assigned to AD Connector.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTags :: Lens.Lens' ConnectDirectory (Core.Maybe [Types.Tag])
cdTags = Lens.field @"tags"
{-# DEPRECATED cdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON ConnectDirectory where
  toJSON ConnectDirectory {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Password" Core..= password),
            Core.Just ("Size" Core..= size),
            Core.Just ("ConnectSettings" Core..= connectSettings),
            ("Description" Core..=) Core.<$> description,
            ("ShortName" Core..=) Core.<$> shortName,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest ConnectDirectory where
  type Rs ConnectDirectory = ConnectDirectoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.ConnectDirectory")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ConnectDirectoryResponse'
            Core.<$> (x Core..:? "DirectoryId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the results of the 'ConnectDirectory' operation.
--
-- /See:/ 'mkConnectDirectoryResponse' smart constructor.
data ConnectDirectoryResponse = ConnectDirectoryResponse'
  { -- | The identifier of the new directory.
    directoryId :: Core.Maybe Types.DirectoryId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectDirectoryResponse' value with any optional fields omitted.
mkConnectDirectoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ConnectDirectoryResponse
mkConnectDirectoryResponse responseStatus =
  ConnectDirectoryResponse'
    { directoryId = Core.Nothing,
      responseStatus
    }

-- | The identifier of the new directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsDirectoryId :: Lens.Lens' ConnectDirectoryResponse (Core.Maybe Types.DirectoryId)
cdrrsDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED cdrrsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' ConnectDirectoryResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
