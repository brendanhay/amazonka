{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetFolderPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the path information (the hierarchy from the root folder) for the specified folder.
--
-- By default, Amazon WorkDocs returns a maximum of 100 levels upwards from the requested folder and only includes the IDs of the parent folders in the path. You can limit the maximum number of levels. You can also request the parent folder names.
module Network.AWS.WorkDocs.GetFolderPath
    (
    -- * Creating a request
      GetFolderPath (..)
    , mkGetFolderPath
    -- ** Request lenses
    , gfpFolderId
    , gfpAuthenticationToken
    , gfpFields
    , gfpLimit
    , gfpMarker

    -- * Destructuring the response
    , GetFolderPathResponse (..)
    , mkGetFolderPathResponse
    -- ** Response lenses
    , gfprrsPath
    , gfprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkGetFolderPath' smart constructor.
data GetFolderPath = GetFolderPath'
  { folderId :: Types.FolderId
    -- ^ The ID of the folder.
  , authenticationToken :: Core.Maybe Types.AuthenticationToken
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , fields :: Core.Maybe Types.Fields
    -- ^ A comma-separated list of values. Specify "NAME" to include the names of the parent folders.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of levels in the hierarchy to return.
  , marker :: Core.Maybe Types.Marker
    -- ^ This value is not supported.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFolderPath' value with any optional fields omitted.
mkGetFolderPath
    :: Types.FolderId -- ^ 'folderId'
    -> GetFolderPath
mkGetFolderPath folderId
  = GetFolderPath'{folderId, authenticationToken = Core.Nothing,
                   fields = Core.Nothing, limit = Core.Nothing, marker = Core.Nothing}

-- | The ID of the folder.
--
-- /Note:/ Consider using 'folderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfpFolderId :: Lens.Lens' GetFolderPath Types.FolderId
gfpFolderId = Lens.field @"folderId"
{-# INLINEABLE gfpFolderId #-}
{-# DEPRECATED folderId "Use generic-lens or generic-optics with 'folderId' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfpAuthenticationToken :: Lens.Lens' GetFolderPath (Core.Maybe Types.AuthenticationToken)
gfpAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE gfpAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | A comma-separated list of values. Specify "NAME" to include the names of the parent folders.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfpFields :: Lens.Lens' GetFolderPath (Core.Maybe Types.Fields)
gfpFields = Lens.field @"fields"
{-# INLINEABLE gfpFields #-}
{-# DEPRECATED fields "Use generic-lens or generic-optics with 'fields' instead"  #-}

-- | The maximum number of levels in the hierarchy to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfpLimit :: Lens.Lens' GetFolderPath (Core.Maybe Core.Natural)
gfpLimit = Lens.field @"limit"
{-# INLINEABLE gfpLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | This value is not supported.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfpMarker :: Lens.Lens' GetFolderPath (Core.Maybe Types.Marker)
gfpMarker = Lens.field @"marker"
{-# INLINEABLE gfpMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.ToQuery GetFolderPath where
        toQuery GetFolderPath{..}
          = Core.maybe Core.mempty (Core.toQueryPair "fields") fields Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "limit") limit
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "marker") marker

instance Core.ToHeaders GetFolderPath where
        toHeaders GetFolderPath{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetFolderPath where
        type Rs GetFolderPath = GetFolderPathResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/api/v1/folders/" Core.<> Core.toText folderId Core.<> "/path",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetFolderPathResponse' Core.<$>
                   (x Core..:? "Path") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetFolderPathResponse' smart constructor.
data GetFolderPathResponse = GetFolderPathResponse'
  { path :: Core.Maybe Types.ResourcePath
    -- ^ The path information.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFolderPathResponse' value with any optional fields omitted.
mkGetFolderPathResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetFolderPathResponse
mkGetFolderPathResponse responseStatus
  = GetFolderPathResponse'{path = Core.Nothing, responseStatus}

-- | The path information.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfprrsPath :: Lens.Lens' GetFolderPathResponse (Core.Maybe Types.ResourcePath)
gfprrsPath = Lens.field @"path"
{-# INLINEABLE gfprrsPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfprrsResponseStatus :: Lens.Lens' GetFolderPathResponse Core.Int
gfprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gfprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
