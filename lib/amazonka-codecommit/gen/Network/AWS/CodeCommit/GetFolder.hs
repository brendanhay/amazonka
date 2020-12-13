{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetFolder (..),
    mkGetFolder,

    -- ** Request lenses
    gfCommitSpecifier,
    gfRepositoryName,
    gfFolderPath,

    -- * Destructuring the response
    GetFolderResponse (..),
    mkGetFolderResponse,

    -- ** Response lenses
    gffrsCommitId,
    gffrsSubModules,
    gffrsTreeId,
    gffrsSubFolders,
    gffrsSymbolicLinks,
    gffrsFiles,
    gffrsFolderPath,
    gffrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFolder' smart constructor.
data GetFolder = GetFolder'
  { -- | A fully qualified reference used to identify a commit that contains the version of the folder's content to return. A fully qualified reference can be a commit ID, branch name, tag, or reference such as HEAD. If no specifier is provided, the folder content is returned as it exists in the HEAD commit.
    commitSpecifier :: Lude.Maybe Lude.Text,
    -- | The name of the repository.
    repositoryName :: Lude.Text,
    -- | The fully qualified path to the folder whose contents are returned, including the folder name. For example, /examples is a fully-qualified path to a folder named examples that was created off of the root directory (/) of a repository.
    folderPath :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFolder' with the minimum fields required to make a request.
--
-- * 'commitSpecifier' - A fully qualified reference used to identify a commit that contains the version of the folder's content to return. A fully qualified reference can be a commit ID, branch name, tag, or reference such as HEAD. If no specifier is provided, the folder content is returned as it exists in the HEAD commit.
-- * 'repositoryName' - The name of the repository.
-- * 'folderPath' - The fully qualified path to the folder whose contents are returned, including the folder name. For example, /examples is a fully-qualified path to a folder named examples that was created off of the root directory (/) of a repository.
mkGetFolder ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'folderPath'
  Lude.Text ->
  GetFolder
mkGetFolder pRepositoryName_ pFolderPath_ =
  GetFolder'
    { commitSpecifier = Lude.Nothing,
      repositoryName = pRepositoryName_,
      folderPath = pFolderPath_
    }

-- | A fully qualified reference used to identify a commit that contains the version of the folder's content to return. A fully qualified reference can be a commit ID, branch name, tag, or reference such as HEAD. If no specifier is provided, the folder content is returned as it exists in the HEAD commit.
--
-- /Note:/ Consider using 'commitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfCommitSpecifier :: Lens.Lens' GetFolder (Lude.Maybe Lude.Text)
gfCommitSpecifier = Lens.lens (commitSpecifier :: GetFolder -> Lude.Maybe Lude.Text) (\s a -> s {commitSpecifier = a} :: GetFolder)
{-# DEPRECATED gfCommitSpecifier "Use generic-lens or generic-optics with 'commitSpecifier' instead." #-}

-- | The name of the repository.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfRepositoryName :: Lens.Lens' GetFolder Lude.Text
gfRepositoryName = Lens.lens (repositoryName :: GetFolder -> Lude.Text) (\s a -> s {repositoryName = a} :: GetFolder)
{-# DEPRECATED gfRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The fully qualified path to the folder whose contents are returned, including the folder name. For example, /examples is a fully-qualified path to a folder named examples that was created off of the root directory (/) of a repository.
--
-- /Note:/ Consider using 'folderPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfFolderPath :: Lens.Lens' GetFolder Lude.Text
gfFolderPath = Lens.lens (folderPath :: GetFolder -> Lude.Text) (\s a -> s {folderPath = a} :: GetFolder)
{-# DEPRECATED gfFolderPath "Use generic-lens or generic-optics with 'folderPath' instead." #-}

instance Lude.AWSRequest GetFolder where
  type Rs GetFolder = GetFolderResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFolderResponse'
            Lude.<$> (x Lude..:> "commitId")
            Lude.<*> (x Lude..?> "subModules" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "treeId")
            Lude.<*> (x Lude..?> "subFolders" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "symbolicLinks" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "files" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "folderPath")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFolder where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.GetFolder" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetFolder where
  toJSON GetFolder' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("commitSpecifier" Lude..=) Lude.<$> commitSpecifier,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("folderPath" Lude..= folderPath)
          ]
      )

instance Lude.ToPath GetFolder where
  toPath = Lude.const "/"

instance Lude.ToQuery GetFolder where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFolderResponse' smart constructor.
data GetFolderResponse = GetFolderResponse'
  { -- | The full commit ID used as a reference for the returned version of the folder content.
    commitId :: Lude.Text,
    -- | The list of submodules in the specified folder, if any.
    subModules :: Lude.Maybe [SubModule],
    -- | The full SHA-1 pointer of the tree information for the commit that contains the folder.
    treeId :: Lude.Maybe Lude.Text,
    -- | The list of folders that exist under the specified folder, if any.
    subFolders :: Lude.Maybe [Folder],
    -- | The list of symbolic links to other files and folders in the specified folder, if any.
    symbolicLinks :: Lude.Maybe [SymbolicLink],
    -- | The list of files in the specified folder, if any.
    files :: Lude.Maybe [File],
    -- | The fully qualified path of the folder whose contents are returned.
    folderPath :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFolderResponse' with the minimum fields required to make a request.
--
-- * 'commitId' - The full commit ID used as a reference for the returned version of the folder content.
-- * 'subModules' - The list of submodules in the specified folder, if any.
-- * 'treeId' - The full SHA-1 pointer of the tree information for the commit that contains the folder.
-- * 'subFolders' - The list of folders that exist under the specified folder, if any.
-- * 'symbolicLinks' - The list of symbolic links to other files and folders in the specified folder, if any.
-- * 'files' - The list of files in the specified folder, if any.
-- * 'folderPath' - The fully qualified path of the folder whose contents are returned.
-- * 'responseStatus' - The response status code.
mkGetFolderResponse ::
  -- | 'commitId'
  Lude.Text ->
  -- | 'folderPath'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  GetFolderResponse
mkGetFolderResponse pCommitId_ pFolderPath_ pResponseStatus_ =
  GetFolderResponse'
    { commitId = pCommitId_,
      subModules = Lude.Nothing,
      treeId = Lude.Nothing,
      subFolders = Lude.Nothing,
      symbolicLinks = Lude.Nothing,
      files = Lude.Nothing,
      folderPath = pFolderPath_,
      responseStatus = pResponseStatus_
    }

-- | The full commit ID used as a reference for the returned version of the folder content.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gffrsCommitId :: Lens.Lens' GetFolderResponse Lude.Text
gffrsCommitId = Lens.lens (commitId :: GetFolderResponse -> Lude.Text) (\s a -> s {commitId = a} :: GetFolderResponse)
{-# DEPRECATED gffrsCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The list of submodules in the specified folder, if any.
--
-- /Note:/ Consider using 'subModules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gffrsSubModules :: Lens.Lens' GetFolderResponse (Lude.Maybe [SubModule])
gffrsSubModules = Lens.lens (subModules :: GetFolderResponse -> Lude.Maybe [SubModule]) (\s a -> s {subModules = a} :: GetFolderResponse)
{-# DEPRECATED gffrsSubModules "Use generic-lens or generic-optics with 'subModules' instead." #-}

-- | The full SHA-1 pointer of the tree information for the commit that contains the folder.
--
-- /Note:/ Consider using 'treeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gffrsTreeId :: Lens.Lens' GetFolderResponse (Lude.Maybe Lude.Text)
gffrsTreeId = Lens.lens (treeId :: GetFolderResponse -> Lude.Maybe Lude.Text) (\s a -> s {treeId = a} :: GetFolderResponse)
{-# DEPRECATED gffrsTreeId "Use generic-lens or generic-optics with 'treeId' instead." #-}

-- | The list of folders that exist under the specified folder, if any.
--
-- /Note:/ Consider using 'subFolders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gffrsSubFolders :: Lens.Lens' GetFolderResponse (Lude.Maybe [Folder])
gffrsSubFolders = Lens.lens (subFolders :: GetFolderResponse -> Lude.Maybe [Folder]) (\s a -> s {subFolders = a} :: GetFolderResponse)
{-# DEPRECATED gffrsSubFolders "Use generic-lens or generic-optics with 'subFolders' instead." #-}

-- | The list of symbolic links to other files and folders in the specified folder, if any.
--
-- /Note:/ Consider using 'symbolicLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gffrsSymbolicLinks :: Lens.Lens' GetFolderResponse (Lude.Maybe [SymbolicLink])
gffrsSymbolicLinks = Lens.lens (symbolicLinks :: GetFolderResponse -> Lude.Maybe [SymbolicLink]) (\s a -> s {symbolicLinks = a} :: GetFolderResponse)
{-# DEPRECATED gffrsSymbolicLinks "Use generic-lens or generic-optics with 'symbolicLinks' instead." #-}

-- | The list of files in the specified folder, if any.
--
-- /Note:/ Consider using 'files' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gffrsFiles :: Lens.Lens' GetFolderResponse (Lude.Maybe [File])
gffrsFiles = Lens.lens (files :: GetFolderResponse -> Lude.Maybe [File]) (\s a -> s {files = a} :: GetFolderResponse)
{-# DEPRECATED gffrsFiles "Use generic-lens or generic-optics with 'files' instead." #-}

-- | The fully qualified path of the folder whose contents are returned.
--
-- /Note:/ Consider using 'folderPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gffrsFolderPath :: Lens.Lens' GetFolderResponse Lude.Text
gffrsFolderPath = Lens.lens (folderPath :: GetFolderResponse -> Lude.Text) (\s a -> s {folderPath = a} :: GetFolderResponse)
{-# DEPRECATED gffrsFolderPath "Use generic-lens or generic-optics with 'folderPath' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gffrsResponseStatus :: Lens.Lens' GetFolderResponse Lude.Int
gffrsResponseStatus = Lens.lens (responseStatus :: GetFolderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFolderResponse)
{-# DEPRECATED gffrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
