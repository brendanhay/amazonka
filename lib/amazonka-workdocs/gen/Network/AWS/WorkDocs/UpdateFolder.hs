{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.UpdateFolder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the specified folder. The user must have access to both the folder and its parent folder, if applicable.
module Network.AWS.WorkDocs.UpdateFolder
    (
    -- * Creating a request
      UpdateFolder (..)
    , mkUpdateFolder
    -- ** Request lenses
    , ufFolderId
    , ufAuthenticationToken
    , ufName
    , ufParentFolderId
    , ufResourceState

    -- * Destructuring the response
    , UpdateFolderResponse (..)
    , mkUpdateFolderResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkUpdateFolder' smart constructor.
data UpdateFolder = UpdateFolder'
  { folderId :: Types.ResourceIdType
    -- ^ The ID of the folder.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , name :: Core.Maybe Types.ResourceNameType
    -- ^ The name of the folder.
  , parentFolderId :: Core.Maybe Types.ResourceIdType
    -- ^ The ID of the parent folder.
  , resourceState :: Core.Maybe Types.ResourceStateType
    -- ^ The resource state of the folder. Only ACTIVE and RECYCLED are accepted values from the API.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFolder' value with any optional fields omitted.
mkUpdateFolder
    :: Types.ResourceIdType -- ^ 'folderId'
    -> UpdateFolder
mkUpdateFolder folderId
  = UpdateFolder'{folderId, authenticationToken = Core.Nothing,
                  name = Core.Nothing, parentFolderId = Core.Nothing,
                  resourceState = Core.Nothing}

-- | The ID of the folder.
--
-- /Note:/ Consider using 'folderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufFolderId :: Lens.Lens' UpdateFolder Types.ResourceIdType
ufFolderId = Lens.field @"folderId"
{-# INLINEABLE ufFolderId #-}
{-# DEPRECATED folderId "Use generic-lens or generic-optics with 'folderId' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufAuthenticationToken :: Lens.Lens' UpdateFolder (Core.Maybe Types.AuthenticationHeaderType)
ufAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE ufAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | The name of the folder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufName :: Lens.Lens' UpdateFolder (Core.Maybe Types.ResourceNameType)
ufName = Lens.field @"name"
{-# INLINEABLE ufName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ID of the parent folder.
--
-- /Note:/ Consider using 'parentFolderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufParentFolderId :: Lens.Lens' UpdateFolder (Core.Maybe Types.ResourceIdType)
ufParentFolderId = Lens.field @"parentFolderId"
{-# INLINEABLE ufParentFolderId #-}
{-# DEPRECATED parentFolderId "Use generic-lens or generic-optics with 'parentFolderId' instead"  #-}

-- | The resource state of the folder. Only ACTIVE and RECYCLED are accepted values from the API.
--
-- /Note:/ Consider using 'resourceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufResourceState :: Lens.Lens' UpdateFolder (Core.Maybe Types.ResourceStateType)
ufResourceState = Lens.field @"resourceState"
{-# INLINEABLE ufResourceState #-}
{-# DEPRECATED resourceState "Use generic-lens or generic-optics with 'resourceState' instead"  #-}

instance Core.ToQuery UpdateFolder where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateFolder where
        toHeaders UpdateFolder{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateFolder where
        toJSON UpdateFolder{..}
          = Core.object
              (Core.catMaybes
                 [("Name" Core..=) Core.<$> name,
                  ("ParentFolderId" Core..=) Core.<$> parentFolderId,
                  ("ResourceState" Core..=) Core.<$> resourceState])

instance Core.AWSRequest UpdateFolder where
        type Rs UpdateFolder = UpdateFolderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath = "/api/v1/folders/" Core.<> Core.toText folderId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull UpdateFolderResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateFolderResponse' smart constructor.
data UpdateFolderResponse = UpdateFolderResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFolderResponse' value with any optional fields omitted.
mkUpdateFolderResponse
    :: UpdateFolderResponse
mkUpdateFolderResponse = UpdateFolderResponse'
