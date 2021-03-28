{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetFolder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata of the specified folder.
module Network.AWS.WorkDocs.GetFolder
    (
    -- * Creating a request
      GetFolder (..)
    , mkGetFolder
    -- ** Request lenses
    , gfFolderId
    , gfAuthenticationToken
    , gfIncludeCustomMetadata

    -- * Destructuring the response
    , GetFolderResponse (..)
    , mkGetFolderResponse
    -- ** Response lenses
    , gfrrsCustomMetadata
    , gfrrsMetadata
    , gfrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkGetFolder' smart constructor.
data GetFolder = GetFolder'
  { folderId :: Types.ResourceIdType
    -- ^ The ID of the folder.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , includeCustomMetadata :: Core.Maybe Core.Bool
    -- ^ Set to TRUE to include custom metadata in the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFolder' value with any optional fields omitted.
mkGetFolder
    :: Types.ResourceIdType -- ^ 'folderId'
    -> GetFolder
mkGetFolder folderId
  = GetFolder'{folderId, authenticationToken = Core.Nothing,
               includeCustomMetadata = Core.Nothing}

-- | The ID of the folder.
--
-- /Note:/ Consider using 'folderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfFolderId :: Lens.Lens' GetFolder Types.ResourceIdType
gfFolderId = Lens.field @"folderId"
{-# INLINEABLE gfFolderId #-}
{-# DEPRECATED folderId "Use generic-lens or generic-optics with 'folderId' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfAuthenticationToken :: Lens.Lens' GetFolder (Core.Maybe Types.AuthenticationHeaderType)
gfAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE gfAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | Set to TRUE to include custom metadata in the response.
--
-- /Note:/ Consider using 'includeCustomMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfIncludeCustomMetadata :: Lens.Lens' GetFolder (Core.Maybe Core.Bool)
gfIncludeCustomMetadata = Lens.field @"includeCustomMetadata"
{-# INLINEABLE gfIncludeCustomMetadata #-}
{-# DEPRECATED includeCustomMetadata "Use generic-lens or generic-optics with 'includeCustomMetadata' instead"  #-}

instance Core.ToQuery GetFolder where
        toQuery GetFolder{..}
          = Core.maybe Core.mempty (Core.toQueryPair "includeCustomMetadata")
              includeCustomMetadata

instance Core.ToHeaders GetFolder where
        toHeaders GetFolder{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetFolder where
        type Rs GetFolder = GetFolderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/api/v1/folders/" Core.<> Core.toText folderId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetFolderResponse' Core.<$>
                   (x Core..:? "CustomMetadata") Core.<*> x Core..:? "Metadata"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetFolderResponse' smart constructor.
data GetFolderResponse = GetFolderResponse'
  { customMetadata :: Core.Maybe (Core.HashMap Types.CustomMetadataKeyType Types.CustomMetadataValueType)
    -- ^ The custom metadata on the folder.
  , metadata :: Core.Maybe Types.FolderMetadata
    -- ^ The metadata of the folder.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetFolderResponse' value with any optional fields omitted.
mkGetFolderResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetFolderResponse
mkGetFolderResponse responseStatus
  = GetFolderResponse'{customMetadata = Core.Nothing,
                       metadata = Core.Nothing, responseStatus}

-- | The custom metadata on the folder.
--
-- /Note:/ Consider using 'customMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsCustomMetadata :: Lens.Lens' GetFolderResponse (Core.Maybe (Core.HashMap Types.CustomMetadataKeyType Types.CustomMetadataValueType))
gfrrsCustomMetadata = Lens.field @"customMetadata"
{-# INLINEABLE gfrrsCustomMetadata #-}
{-# DEPRECATED customMetadata "Use generic-lens or generic-optics with 'customMetadata' instead"  #-}

-- | The metadata of the folder.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsMetadata :: Lens.Lens' GetFolderResponse (Core.Maybe Types.FolderMetadata)
gfrrsMetadata = Lens.field @"metadata"
{-# INLINEABLE gfrrsMetadata #-}
{-# DEPRECATED metadata "Use generic-lens or generic-optics with 'metadata' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsResponseStatus :: Lens.Lens' GetFolderResponse Core.Int
gfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
