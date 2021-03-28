{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteFolder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified folder and its contents.
module Network.AWS.WorkDocs.DeleteFolder
    (
    -- * Creating a request
      DeleteFolder (..)
    , mkDeleteFolder
    -- ** Request lenses
    , dfFolderId
    , dfAuthenticationToken

    -- * Destructuring the response
    , DeleteFolderResponse (..)
    , mkDeleteFolderResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDeleteFolder' smart constructor.
data DeleteFolder = DeleteFolder'
  { folderId :: Types.ResourceIdType
    -- ^ The ID of the folder.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFolder' value with any optional fields omitted.
mkDeleteFolder
    :: Types.ResourceIdType -- ^ 'folderId'
    -> DeleteFolder
mkDeleteFolder folderId
  = DeleteFolder'{folderId, authenticationToken = Core.Nothing}

-- | The ID of the folder.
--
-- /Note:/ Consider using 'folderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFolderId :: Lens.Lens' DeleteFolder Types.ResourceIdType
dfFolderId = Lens.field @"folderId"
{-# INLINEABLE dfFolderId #-}
{-# DEPRECATED folderId "Use generic-lens or generic-optics with 'folderId' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfAuthenticationToken :: Lens.Lens' DeleteFolder (Core.Maybe Types.AuthenticationHeaderType)
dfAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE dfAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

instance Core.ToQuery DeleteFolder where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteFolder where
        toHeaders DeleteFolder{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteFolder where
        type Rs DeleteFolder = DeleteFolderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/api/v1/folders/" Core.<> Core.toText folderId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteFolderResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteFolderResponse' smart constructor.
data DeleteFolderResponse = DeleteFolderResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFolderResponse' value with any optional fields omitted.
mkDeleteFolderResponse
    :: DeleteFolderResponse
mkDeleteFolderResponse = DeleteFolderResponse'
