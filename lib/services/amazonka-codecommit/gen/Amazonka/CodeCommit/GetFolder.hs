{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeCommit.GetFolder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the contents of a specified folder in a repository.
module Amazonka.CodeCommit.GetFolder
  ( -- * Creating a Request
    GetFolder (..),
    newGetFolder,

    -- * Request Lenses
    getFolder_commitSpecifier,
    getFolder_repositoryName,
    getFolder_folderPath,

    -- * Destructuring the Response
    GetFolderResponse (..),
    newGetFolderResponse,

    -- * Response Lenses
    getFolderResponse_files,
    getFolderResponse_subFolders,
    getFolderResponse_subModules,
    getFolderResponse_symbolicLinks,
    getFolderResponse_treeId,
    getFolderResponse_httpStatus,
    getFolderResponse_commitId,
    getFolderResponse_folderPath,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFolder' smart constructor.
data GetFolder = GetFolder'
  { -- | A fully qualified reference used to identify a commit that contains the
    -- version of the folder\'s content to return. A fully qualified reference
    -- can be a commit ID, branch name, tag, or reference such as HEAD. If no
    -- specifier is provided, the folder content is returned as it exists in
    -- the HEAD commit.
    commitSpecifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository.
    repositoryName :: Prelude.Text,
    -- | The fully qualified path to the folder whose contents are returned,
    -- including the folder name. For example, \/examples is a fully-qualified
    -- path to a folder named examples that was created off of the root
    -- directory (\/) of a repository.
    folderPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitSpecifier', 'getFolder_commitSpecifier' - A fully qualified reference used to identify a commit that contains the
-- version of the folder\'s content to return. A fully qualified reference
-- can be a commit ID, branch name, tag, or reference such as HEAD. If no
-- specifier is provided, the folder content is returned as it exists in
-- the HEAD commit.
--
-- 'repositoryName', 'getFolder_repositoryName' - The name of the repository.
--
-- 'folderPath', 'getFolder_folderPath' - The fully qualified path to the folder whose contents are returned,
-- including the folder name. For example, \/examples is a fully-qualified
-- path to a folder named examples that was created off of the root
-- directory (\/) of a repository.
newGetFolder ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'folderPath'
  Prelude.Text ->
  GetFolder
newGetFolder pRepositoryName_ pFolderPath_ =
  GetFolder'
    { commitSpecifier = Prelude.Nothing,
      repositoryName = pRepositoryName_,
      folderPath = pFolderPath_
    }

-- | A fully qualified reference used to identify a commit that contains the
-- version of the folder\'s content to return. A fully qualified reference
-- can be a commit ID, branch name, tag, or reference such as HEAD. If no
-- specifier is provided, the folder content is returned as it exists in
-- the HEAD commit.
getFolder_commitSpecifier :: Lens.Lens' GetFolder (Prelude.Maybe Prelude.Text)
getFolder_commitSpecifier = Lens.lens (\GetFolder' {commitSpecifier} -> commitSpecifier) (\s@GetFolder' {} a -> s {commitSpecifier = a} :: GetFolder)

-- | The name of the repository.
getFolder_repositoryName :: Lens.Lens' GetFolder Prelude.Text
getFolder_repositoryName = Lens.lens (\GetFolder' {repositoryName} -> repositoryName) (\s@GetFolder' {} a -> s {repositoryName = a} :: GetFolder)

-- | The fully qualified path to the folder whose contents are returned,
-- including the folder name. For example, \/examples is a fully-qualified
-- path to a folder named examples that was created off of the root
-- directory (\/) of a repository.
getFolder_folderPath :: Lens.Lens' GetFolder Prelude.Text
getFolder_folderPath = Lens.lens (\GetFolder' {folderPath} -> folderPath) (\s@GetFolder' {} a -> s {folderPath = a} :: GetFolder)

instance Core.AWSRequest GetFolder where
  type AWSResponse GetFolder = GetFolderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFolderResponse'
            Prelude.<$> (x Data..?> "files" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "subFolders" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "subModules" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "symbolicLinks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "treeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "commitId")
            Prelude.<*> (x Data..:> "folderPath")
      )

instance Prelude.Hashable GetFolder where
  hashWithSalt _salt GetFolder' {..} =
    _salt
      `Prelude.hashWithSalt` commitSpecifier
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` folderPath

instance Prelude.NFData GetFolder where
  rnf GetFolder' {..} =
    Prelude.rnf commitSpecifier
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf folderPath

instance Data.ToHeaders GetFolder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.GetFolder" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetFolder where
  toJSON GetFolder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("commitSpecifier" Data..=)
              Prelude.<$> commitSpecifier,
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just ("folderPath" Data..= folderPath)
          ]
      )

instance Data.ToPath GetFolder where
  toPath = Prelude.const "/"

instance Data.ToQuery GetFolder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFolderResponse' smart constructor.
data GetFolderResponse = GetFolderResponse'
  { -- | The list of files in the specified folder, if any.
    files :: Prelude.Maybe [File],
    -- | The list of folders that exist under the specified folder, if any.
    subFolders :: Prelude.Maybe [Folder],
    -- | The list of submodules in the specified folder, if any.
    subModules :: Prelude.Maybe [SubModule],
    -- | The list of symbolic links to other files and folders in the specified
    -- folder, if any.
    symbolicLinks :: Prelude.Maybe [SymbolicLink],
    -- | The full SHA-1 pointer of the tree information for the commit that
    -- contains the folder.
    treeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The full commit ID used as a reference for the returned version of the
    -- folder content.
    commitId :: Prelude.Text,
    -- | The fully qualified path of the folder whose contents are returned.
    folderPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'files', 'getFolderResponse_files' - The list of files in the specified folder, if any.
--
-- 'subFolders', 'getFolderResponse_subFolders' - The list of folders that exist under the specified folder, if any.
--
-- 'subModules', 'getFolderResponse_subModules' - The list of submodules in the specified folder, if any.
--
-- 'symbolicLinks', 'getFolderResponse_symbolicLinks' - The list of symbolic links to other files and folders in the specified
-- folder, if any.
--
-- 'treeId', 'getFolderResponse_treeId' - The full SHA-1 pointer of the tree information for the commit that
-- contains the folder.
--
-- 'httpStatus', 'getFolderResponse_httpStatus' - The response's http status code.
--
-- 'commitId', 'getFolderResponse_commitId' - The full commit ID used as a reference for the returned version of the
-- folder content.
--
-- 'folderPath', 'getFolderResponse_folderPath' - The fully qualified path of the folder whose contents are returned.
newGetFolderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'commitId'
  Prelude.Text ->
  -- | 'folderPath'
  Prelude.Text ->
  GetFolderResponse
newGetFolderResponse
  pHttpStatus_
  pCommitId_
  pFolderPath_ =
    GetFolderResponse'
      { files = Prelude.Nothing,
        subFolders = Prelude.Nothing,
        subModules = Prelude.Nothing,
        symbolicLinks = Prelude.Nothing,
        treeId = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        commitId = pCommitId_,
        folderPath = pFolderPath_
      }

-- | The list of files in the specified folder, if any.
getFolderResponse_files :: Lens.Lens' GetFolderResponse (Prelude.Maybe [File])
getFolderResponse_files = Lens.lens (\GetFolderResponse' {files} -> files) (\s@GetFolderResponse' {} a -> s {files = a} :: GetFolderResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of folders that exist under the specified folder, if any.
getFolderResponse_subFolders :: Lens.Lens' GetFolderResponse (Prelude.Maybe [Folder])
getFolderResponse_subFolders = Lens.lens (\GetFolderResponse' {subFolders} -> subFolders) (\s@GetFolderResponse' {} a -> s {subFolders = a} :: GetFolderResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of submodules in the specified folder, if any.
getFolderResponse_subModules :: Lens.Lens' GetFolderResponse (Prelude.Maybe [SubModule])
getFolderResponse_subModules = Lens.lens (\GetFolderResponse' {subModules} -> subModules) (\s@GetFolderResponse' {} a -> s {subModules = a} :: GetFolderResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of symbolic links to other files and folders in the specified
-- folder, if any.
getFolderResponse_symbolicLinks :: Lens.Lens' GetFolderResponse (Prelude.Maybe [SymbolicLink])
getFolderResponse_symbolicLinks = Lens.lens (\GetFolderResponse' {symbolicLinks} -> symbolicLinks) (\s@GetFolderResponse' {} a -> s {symbolicLinks = a} :: GetFolderResponse) Prelude.. Lens.mapping Lens.coerced

-- | The full SHA-1 pointer of the tree information for the commit that
-- contains the folder.
getFolderResponse_treeId :: Lens.Lens' GetFolderResponse (Prelude.Maybe Prelude.Text)
getFolderResponse_treeId = Lens.lens (\GetFolderResponse' {treeId} -> treeId) (\s@GetFolderResponse' {} a -> s {treeId = a} :: GetFolderResponse)

-- | The response's http status code.
getFolderResponse_httpStatus :: Lens.Lens' GetFolderResponse Prelude.Int
getFolderResponse_httpStatus = Lens.lens (\GetFolderResponse' {httpStatus} -> httpStatus) (\s@GetFolderResponse' {} a -> s {httpStatus = a} :: GetFolderResponse)

-- | The full commit ID used as a reference for the returned version of the
-- folder content.
getFolderResponse_commitId :: Lens.Lens' GetFolderResponse Prelude.Text
getFolderResponse_commitId = Lens.lens (\GetFolderResponse' {commitId} -> commitId) (\s@GetFolderResponse' {} a -> s {commitId = a} :: GetFolderResponse)

-- | The fully qualified path of the folder whose contents are returned.
getFolderResponse_folderPath :: Lens.Lens' GetFolderResponse Prelude.Text
getFolderResponse_folderPath = Lens.lens (\GetFolderResponse' {folderPath} -> folderPath) (\s@GetFolderResponse' {} a -> s {folderPath = a} :: GetFolderResponse)

instance Prelude.NFData GetFolderResponse where
  rnf GetFolderResponse' {..} =
    Prelude.rnf files
      `Prelude.seq` Prelude.rnf subFolders
      `Prelude.seq` Prelude.rnf subModules
      `Prelude.seq` Prelude.rnf symbolicLinks
      `Prelude.seq` Prelude.rnf treeId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf commitId
      `Prelude.seq` Prelude.rnf folderPath
