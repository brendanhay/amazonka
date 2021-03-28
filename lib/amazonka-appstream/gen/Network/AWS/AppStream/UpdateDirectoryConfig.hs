{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.UpdateDirectoryConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified Directory Config object in AppStream 2.0. This object includes the configuration information required to join fleets and image builders to Microsoft Active Directory domains.
module Network.AWS.AppStream.UpdateDirectoryConfig
    (
    -- * Creating a request
      UpdateDirectoryConfig (..)
    , mkUpdateDirectoryConfig
    -- ** Request lenses
    , udcDirectoryName
    , udcOrganizationalUnitDistinguishedNames
    , udcServiceAccountCredentials

    -- * Destructuring the response
    , UpdateDirectoryConfigResponse (..)
    , mkUpdateDirectoryConfigResponse
    -- ** Response lenses
    , udcrrsDirectoryConfig
    , udcrrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDirectoryConfig' smart constructor.
data UpdateDirectoryConfig = UpdateDirectoryConfig'
  { directoryName :: Types.DirectoryName
    -- ^ The name of the Directory Config object.
  , organizationalUnitDistinguishedNames :: Core.Maybe [Types.OrganizationalUnitDistinguishedName]
    -- ^ The distinguished names of the organizational units for computer accounts.
  , serviceAccountCredentials :: Core.Maybe Types.ServiceAccountCredentials
    -- ^ The credentials for the service account used by the fleet or image builder to connect to the directory.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDirectoryConfig' value with any optional fields omitted.
mkUpdateDirectoryConfig
    :: Types.DirectoryName -- ^ 'directoryName'
    -> UpdateDirectoryConfig
mkUpdateDirectoryConfig directoryName
  = UpdateDirectoryConfig'{directoryName,
                           organizationalUnitDistinguishedNames = Core.Nothing,
                           serviceAccountCredentials = Core.Nothing}

-- | The name of the Directory Config object.
--
-- /Note:/ Consider using 'directoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcDirectoryName :: Lens.Lens' UpdateDirectoryConfig Types.DirectoryName
udcDirectoryName = Lens.field @"directoryName"
{-# INLINEABLE udcDirectoryName #-}
{-# DEPRECATED directoryName "Use generic-lens or generic-optics with 'directoryName' instead"  #-}

-- | The distinguished names of the organizational units for computer accounts.
--
-- /Note:/ Consider using 'organizationalUnitDistinguishedNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcOrganizationalUnitDistinguishedNames :: Lens.Lens' UpdateDirectoryConfig (Core.Maybe [Types.OrganizationalUnitDistinguishedName])
udcOrganizationalUnitDistinguishedNames = Lens.field @"organizationalUnitDistinguishedNames"
{-# INLINEABLE udcOrganizationalUnitDistinguishedNames #-}
{-# DEPRECATED organizationalUnitDistinguishedNames "Use generic-lens or generic-optics with 'organizationalUnitDistinguishedNames' instead"  #-}

-- | The credentials for the service account used by the fleet or image builder to connect to the directory.
--
-- /Note:/ Consider using 'serviceAccountCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcServiceAccountCredentials :: Lens.Lens' UpdateDirectoryConfig (Core.Maybe Types.ServiceAccountCredentials)
udcServiceAccountCredentials = Lens.field @"serviceAccountCredentials"
{-# INLINEABLE udcServiceAccountCredentials #-}
{-# DEPRECATED serviceAccountCredentials "Use generic-lens or generic-optics with 'serviceAccountCredentials' instead"  #-}

instance Core.ToQuery UpdateDirectoryConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDirectoryConfig where
        toHeaders UpdateDirectoryConfig{..}
          = Core.pure
              ("X-Amz-Target", "PhotonAdminProxyService.UpdateDirectoryConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateDirectoryConfig where
        toJSON UpdateDirectoryConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryName" Core..= directoryName),
                  ("OrganizationalUnitDistinguishedNames" Core..=) Core.<$>
                    organizationalUnitDistinguishedNames,
                  ("ServiceAccountCredentials" Core..=) Core.<$>
                    serviceAccountCredentials])

instance Core.AWSRequest UpdateDirectoryConfig where
        type Rs UpdateDirectoryConfig = UpdateDirectoryConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateDirectoryConfigResponse' Core.<$>
                   (x Core..:? "DirectoryConfig") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateDirectoryConfigResponse' smart constructor.
data UpdateDirectoryConfigResponse = UpdateDirectoryConfigResponse'
  { directoryConfig :: Core.Maybe Types.DirectoryConfig
    -- ^ Information about the Directory Config object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateDirectoryConfigResponse' value with any optional fields omitted.
mkUpdateDirectoryConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateDirectoryConfigResponse
mkUpdateDirectoryConfigResponse responseStatus
  = UpdateDirectoryConfigResponse'{directoryConfig = Core.Nothing,
                                   responseStatus}

-- | Information about the Directory Config object.
--
-- /Note:/ Consider using 'directoryConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcrrsDirectoryConfig :: Lens.Lens' UpdateDirectoryConfigResponse (Core.Maybe Types.DirectoryConfig)
udcrrsDirectoryConfig = Lens.field @"directoryConfig"
{-# INLINEABLE udcrrsDirectoryConfig #-}
{-# DEPRECATED directoryConfig "Use generic-lens or generic-optics with 'directoryConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcrrsResponseStatus :: Lens.Lens' UpdateDirectoryConfigResponse Core.Int
udcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE udcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
