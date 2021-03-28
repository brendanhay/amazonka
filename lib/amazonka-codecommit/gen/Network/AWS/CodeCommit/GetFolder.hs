{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetFolder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the contents of a specified folder in a repository.
module Network.AWS.CodeCommit.GetFolder
    (
    -- * Creating a request
      GetFolder (..)
    , mkGetFolder
    -- ** Request lenses
    , gfRepositoryName
    , gfFolderPath
    , gfCommitSpecifier

    -- * Destructuring the response
    , GetFolderResponse (..)
    , mkGetFolderResponse
    -- ** Response lenses
    , gfrrsCommitId
    , gfrrsFolderPath
    , gfrrsFiles
    , gfrrsSubFolders
    , gfrrsSubModules
    , gfrrsSymbolicLinks
    , gfrrsTreeId
    , gfrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFolder' smart constructor.
data GetFolder = GetFolder'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository.
  , folderPath :: Types.Path
    -- ^ The fully qualified path to the folder whose contents are returned, including the folder name. For example, /examples is a fully-qualified path to a folder named examples that was created off of the root directory (/) of a repository. 
  , commitSpecifier :: Core.Maybe Types.CommitSpecifier
    -- ^ A fully qualified reference used to identify a commit that contains the version of the folder's content to return. A fully qualified reference can be a commit ID, branch name, tag, or reference such as HEAD. If no specifier is provided, the folder content is returned as it exists in the HEAD commit.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFolder' value with any optional fields omitted.
mkGetFolder
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> Types.Path -- ^ 'folderPath'
    -> GetFolder
mkGetFolder repositoryName folderPath
  = GetFolder'{repositoryName, folderPath,
               commitSpecifier = Core.Nothing}

-- | The name of the repository.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfRepositoryName :: Lens.Lens' GetFolder Types.RepositoryName
gfRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE gfRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | The fully qualified path to the folder whose contents are returned, including the folder name. For example, /examples is a fully-qualified path to a folder named examples that was created off of the root directory (/) of a repository. 
--
-- /Note:/ Consider using 'folderPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfFolderPath :: Lens.Lens' GetFolder Types.Path
gfFolderPath = Lens.field @"folderPath"
{-# INLINEABLE gfFolderPath #-}
{-# DEPRECATED folderPath "Use generic-lens or generic-optics with 'folderPath' instead"  #-}

-- | A fully qualified reference used to identify a commit that contains the version of the folder's content to return. A fully qualified reference can be a commit ID, branch name, tag, or reference such as HEAD. If no specifier is provided, the folder content is returned as it exists in the HEAD commit.
--
-- /Note:/ Consider using 'commitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfCommitSpecifier :: Lens.Lens' GetFolder (Core.Maybe Types.CommitSpecifier)
gfCommitSpecifier = Lens.field @"commitSpecifier"
{-# INLINEABLE gfCommitSpecifier #-}
{-# DEPRECATED commitSpecifier "Use generic-lens or generic-optics with 'commitSpecifier' instead"  #-}

instance Core.ToQuery GetFolder where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetFolder where
        toHeaders GetFolder{..}
          = Core.pure ("X-Amz-Target", "CodeCommit_20150413.GetFolder")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetFolder where
        toJSON GetFolder{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  Core.Just ("folderPath" Core..= folderPath),
                  ("commitSpecifier" Core..=) Core.<$> commitSpecifier])

instance Core.AWSRequest GetFolder where
        type Rs GetFolder = GetFolderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetFolderResponse' Core.<$>
                   (x Core..: "commitId") Core.<*> x Core..: "folderPath" Core.<*>
                     x Core..:? "files"
                     Core.<*> x Core..:? "subFolders"
                     Core.<*> x Core..:? "subModules"
                     Core.<*> x Core..:? "symbolicLinks"
                     Core.<*> x Core..:? "treeId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetFolderResponse' smart constructor.
data GetFolderResponse = GetFolderResponse'
  { commitId :: Types.ObjectId
    -- ^ The full commit ID used as a reference for the returned version of the folder content.
  , folderPath :: Types.Path
    -- ^ The fully qualified path of the folder whose contents are returned.
  , files :: Core.Maybe [Types.File]
    -- ^ The list of files in the specified folder, if any.
  , subFolders :: Core.Maybe [Types.Folder]
    -- ^ The list of folders that exist under the specified folder, if any.
  , subModules :: Core.Maybe [Types.SubModule]
    -- ^ The list of submodules in the specified folder, if any.
  , symbolicLinks :: Core.Maybe [Types.SymbolicLink]
    -- ^ The list of symbolic links to other files and folders in the specified folder, if any.
  , treeId :: Core.Maybe Types.ObjectId
    -- ^ The full SHA-1 pointer of the tree information for the commit that contains the folder.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFolderResponse' value with any optional fields omitted.
mkGetFolderResponse
    :: Types.ObjectId -- ^ 'commitId'
    -> Types.Path -- ^ 'folderPath'
    -> Core.Int -- ^ 'responseStatus'
    -> GetFolderResponse
mkGetFolderResponse commitId folderPath responseStatus
  = GetFolderResponse'{commitId, folderPath, files = Core.Nothing,
                       subFolders = Core.Nothing, subModules = Core.Nothing,
                       symbolicLinks = Core.Nothing, treeId = Core.Nothing,
                       responseStatus}

-- | The full commit ID used as a reference for the returned version of the folder content.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsCommitId :: Lens.Lens' GetFolderResponse Types.ObjectId
gfrrsCommitId = Lens.field @"commitId"
{-# INLINEABLE gfrrsCommitId #-}
{-# DEPRECATED commitId "Use generic-lens or generic-optics with 'commitId' instead"  #-}

-- | The fully qualified path of the folder whose contents are returned.
--
-- /Note:/ Consider using 'folderPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsFolderPath :: Lens.Lens' GetFolderResponse Types.Path
gfrrsFolderPath = Lens.field @"folderPath"
{-# INLINEABLE gfrrsFolderPath #-}
{-# DEPRECATED folderPath "Use generic-lens or generic-optics with 'folderPath' instead"  #-}

-- | The list of files in the specified folder, if any.
--
-- /Note:/ Consider using 'files' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsFiles :: Lens.Lens' GetFolderResponse (Core.Maybe [Types.File])
gfrrsFiles = Lens.field @"files"
{-# INLINEABLE gfrrsFiles #-}
{-# DEPRECATED files "Use generic-lens or generic-optics with 'files' instead"  #-}

-- | The list of folders that exist under the specified folder, if any.
--
-- /Note:/ Consider using 'subFolders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsSubFolders :: Lens.Lens' GetFolderResponse (Core.Maybe [Types.Folder])
gfrrsSubFolders = Lens.field @"subFolders"
{-# INLINEABLE gfrrsSubFolders #-}
{-# DEPRECATED subFolders "Use generic-lens or generic-optics with 'subFolders' instead"  #-}

-- | The list of submodules in the specified folder, if any.
--
-- /Note:/ Consider using 'subModules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsSubModules :: Lens.Lens' GetFolderResponse (Core.Maybe [Types.SubModule])
gfrrsSubModules = Lens.field @"subModules"
{-# INLINEABLE gfrrsSubModules #-}
{-# DEPRECATED subModules "Use generic-lens or generic-optics with 'subModules' instead"  #-}

-- | The list of symbolic links to other files and folders in the specified folder, if any.
--
-- /Note:/ Consider using 'symbolicLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsSymbolicLinks :: Lens.Lens' GetFolderResponse (Core.Maybe [Types.SymbolicLink])
gfrrsSymbolicLinks = Lens.field @"symbolicLinks"
{-# INLINEABLE gfrrsSymbolicLinks #-}
{-# DEPRECATED symbolicLinks "Use generic-lens or generic-optics with 'symbolicLinks' instead"  #-}

-- | The full SHA-1 pointer of the tree information for the commit that contains the folder.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsTreeId :: Lens.Lens' GetFolderResponse (Core.Maybe Types.ObjectId)
gfrrsTreeId = Lens.field @"treeId"
{-# INLINEABLE gfrrsTreeId #-}
{-# DEPRECATED treeId "Use generic-lens or generic-optics with 'treeId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsResponseStatus :: Lens.Lens' GetFolderResponse Core.Int
gfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
