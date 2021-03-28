{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateDirectoryConfig (..)
    , mkCreateDirectoryConfig
    -- ** Request lenses
    , cdcDirectoryName
    , cdcOrganizationalUnitDistinguishedNames
    , cdcServiceAccountCredentials

    -- * Destructuring the response
    , CreateDirectoryConfigResponse (..)
    , mkCreateDirectoryConfigResponse
    -- ** Response lenses
    , cdcrrsDirectoryConfig
    , cdcrrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDirectoryConfig' smart constructor.
data CreateDirectoryConfig = CreateDirectoryConfig'
  { directoryName :: Types.DirectoryName
    -- ^ The fully qualified name of the directory (for example, corp.example.com).
  , organizationalUnitDistinguishedNames :: [Types.OrganizationalUnitDistinguishedName]
    -- ^ The distinguished names of the organizational units for computer accounts.
  , serviceAccountCredentials :: Core.Maybe Types.ServiceAccountCredentials
    -- ^ The credentials for the service account used by the fleet or image builder to connect to the directory.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDirectoryConfig' value with any optional fields omitted.
mkCreateDirectoryConfig
    :: Types.DirectoryName -- ^ 'directoryName'
    -> CreateDirectoryConfig
mkCreateDirectoryConfig directoryName
  = CreateDirectoryConfig'{directoryName,
                           organizationalUnitDistinguishedNames = Core.mempty,
                           serviceAccountCredentials = Core.Nothing}

-- | The fully qualified name of the directory (for example, corp.example.com).
--
-- /Note:/ Consider using 'directoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDirectoryName :: Lens.Lens' CreateDirectoryConfig Types.DirectoryName
cdcDirectoryName = Lens.field @"directoryName"
{-# INLINEABLE cdcDirectoryName #-}
{-# DEPRECATED directoryName "Use generic-lens or generic-optics with 'directoryName' instead"  #-}

-- | The distinguished names of the organizational units for computer accounts.
--
-- /Note:/ Consider using 'organizationalUnitDistinguishedNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcOrganizationalUnitDistinguishedNames :: Lens.Lens' CreateDirectoryConfig [Types.OrganizationalUnitDistinguishedName]
cdcOrganizationalUnitDistinguishedNames = Lens.field @"organizationalUnitDistinguishedNames"
{-# INLINEABLE cdcOrganizationalUnitDistinguishedNames #-}
{-# DEPRECATED organizationalUnitDistinguishedNames "Use generic-lens or generic-optics with 'organizationalUnitDistinguishedNames' instead"  #-}

-- | The credentials for the service account used by the fleet or image builder to connect to the directory.
--
-- /Note:/ Consider using 'serviceAccountCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcServiceAccountCredentials :: Lens.Lens' CreateDirectoryConfig (Core.Maybe Types.ServiceAccountCredentials)
cdcServiceAccountCredentials = Lens.field @"serviceAccountCredentials"
{-# INLINEABLE cdcServiceAccountCredentials #-}
{-# DEPRECATED serviceAccountCredentials "Use generic-lens or generic-optics with 'serviceAccountCredentials' instead"  #-}

instance Core.ToQuery CreateDirectoryConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDirectoryConfig where
        toHeaders CreateDirectoryConfig{..}
          = Core.pure
              ("X-Amz-Target", "PhotonAdminProxyService.CreateDirectoryConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDirectoryConfig where
        toJSON CreateDirectoryConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryName" Core..= directoryName),
                  Core.Just
                    ("OrganizationalUnitDistinguishedNames" Core..=
                       organizationalUnitDistinguishedNames),
                  ("ServiceAccountCredentials" Core..=) Core.<$>
                    serviceAccountCredentials])

instance Core.AWSRequest CreateDirectoryConfig where
        type Rs CreateDirectoryConfig = CreateDirectoryConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDirectoryConfigResponse' Core.<$>
                   (x Core..:? "DirectoryConfig") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDirectoryConfigResponse' smart constructor.
data CreateDirectoryConfigResponse = CreateDirectoryConfigResponse'
  { directoryConfig :: Core.Maybe Types.DirectoryConfig
    -- ^ Information about the directory configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateDirectoryConfigResponse' value with any optional fields omitted.
mkCreateDirectoryConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDirectoryConfigResponse
mkCreateDirectoryConfigResponse responseStatus
  = CreateDirectoryConfigResponse'{directoryConfig = Core.Nothing,
                                   responseStatus}

-- | Information about the directory configuration.
--
-- /Note:/ Consider using 'directoryConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrrsDirectoryConfig :: Lens.Lens' CreateDirectoryConfigResponse (Core.Maybe Types.DirectoryConfig)
cdcrrsDirectoryConfig = Lens.field @"directoryConfig"
{-# INLINEABLE cdcrrsDirectoryConfig #-}
{-# DEPRECATED directoryConfig "Use generic-lens or generic-optics with 'directoryConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrrsResponseStatus :: Lens.Lens' CreateDirectoryConfigResponse Core.Int
cdcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
