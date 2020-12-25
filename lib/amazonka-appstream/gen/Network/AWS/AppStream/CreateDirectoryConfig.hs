{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.CreateDirectoryConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Directory Config object in AppStream 2.0. This object includes the configuration information required to join fleets and image builders to Microsoft Active Directory domains.
module Network.AWS.AppStream.CreateDirectoryConfig
  ( -- * Creating a request
    CreateDirectoryConfig (..),
    mkCreateDirectoryConfig,

    -- ** Request lenses
    cdcDirectoryName,
    cdcOrganizationalUnitDistinguishedNames,
    cdcServiceAccountCredentials,

    -- * Destructuring the response
    CreateDirectoryConfigResponse (..),
    mkCreateDirectoryConfigResponse,

    -- ** Response lenses
    cdcrrsDirectoryConfig,
    cdcrrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDirectoryConfig' smart constructor.
data CreateDirectoryConfig = CreateDirectoryConfig'
  { -- | The fully qualified name of the directory (for example, corp.example.com).
    directoryName :: Types.DirectoryName,
    -- | The distinguished names of the organizational units for computer accounts.
    organizationalUnitDistinguishedNames :: [Types.OrganizationalUnitDistinguishedName],
    -- | The credentials for the service account used by the fleet or image builder to connect to the directory.
    serviceAccountCredentials :: Core.Maybe Types.ServiceAccountCredentials
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDirectoryConfig' value with any optional fields omitted.
mkCreateDirectoryConfig ::
  -- | 'directoryName'
  Types.DirectoryName ->
  CreateDirectoryConfig
mkCreateDirectoryConfig directoryName =
  CreateDirectoryConfig'
    { directoryName,
      organizationalUnitDistinguishedNames = Core.mempty,
      serviceAccountCredentials = Core.Nothing
    }

-- | The fully qualified name of the directory (for example, corp.example.com).
--
-- /Note:/ Consider using 'directoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDirectoryName :: Lens.Lens' CreateDirectoryConfig Types.DirectoryName
cdcDirectoryName = Lens.field @"directoryName"
{-# DEPRECATED cdcDirectoryName "Use generic-lens or generic-optics with 'directoryName' instead." #-}

-- | The distinguished names of the organizational units for computer accounts.
--
-- /Note:/ Consider using 'organizationalUnitDistinguishedNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcOrganizationalUnitDistinguishedNames :: Lens.Lens' CreateDirectoryConfig [Types.OrganizationalUnitDistinguishedName]
cdcOrganizationalUnitDistinguishedNames = Lens.field @"organizationalUnitDistinguishedNames"
{-# DEPRECATED cdcOrganizationalUnitDistinguishedNames "Use generic-lens or generic-optics with 'organizationalUnitDistinguishedNames' instead." #-}

-- | The credentials for the service account used by the fleet or image builder to connect to the directory.
--
-- /Note:/ Consider using 'serviceAccountCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcServiceAccountCredentials :: Lens.Lens' CreateDirectoryConfig (Core.Maybe Types.ServiceAccountCredentials)
cdcServiceAccountCredentials = Lens.field @"serviceAccountCredentials"
{-# DEPRECATED cdcServiceAccountCredentials "Use generic-lens or generic-optics with 'serviceAccountCredentials' instead." #-}

instance Core.FromJSON CreateDirectoryConfig where
  toJSON CreateDirectoryConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryName" Core..= directoryName),
            Core.Just
              ( "OrganizationalUnitDistinguishedNames"
                  Core..= organizationalUnitDistinguishedNames
              ),
            ("ServiceAccountCredentials" Core..=)
              Core.<$> serviceAccountCredentials
          ]
      )

instance Core.AWSRequest CreateDirectoryConfig where
  type Rs CreateDirectoryConfig = CreateDirectoryConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "PhotonAdminProxyService.CreateDirectoryConfig")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDirectoryConfigResponse'
            Core.<$> (x Core..:? "DirectoryConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDirectoryConfigResponse' smart constructor.
data CreateDirectoryConfigResponse = CreateDirectoryConfigResponse'
  { -- | Information about the directory configuration.
    directoryConfig :: Core.Maybe Types.DirectoryConfig,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateDirectoryConfigResponse' value with any optional fields omitted.
mkCreateDirectoryConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDirectoryConfigResponse
mkCreateDirectoryConfigResponse responseStatus =
  CreateDirectoryConfigResponse'
    { directoryConfig = Core.Nothing,
      responseStatus
    }

-- | Information about the directory configuration.
--
-- /Note:/ Consider using 'directoryConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrrsDirectoryConfig :: Lens.Lens' CreateDirectoryConfigResponse (Core.Maybe Types.DirectoryConfig)
cdcrrsDirectoryConfig = Lens.field @"directoryConfig"
{-# DEPRECATED cdcrrsDirectoryConfig "Use generic-lens or generic-optics with 'directoryConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrrsResponseStatus :: Lens.Lens' CreateDirectoryConfigResponse Core.Int
cdcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
