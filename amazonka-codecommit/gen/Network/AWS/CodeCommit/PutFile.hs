{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeCommit.PutFile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates a file in a branch in an AWS CodeCommit repository, and
-- generates a commit for the addition in the specified branch.
module Network.AWS.CodeCommit.PutFile
  ( -- * Creating a Request
    PutFile (..),
    newPutFile,

    -- * Request Lenses
    putFile_parentCommitId,
    putFile_commitMessage,
    putFile_name,
    putFile_email,
    putFile_fileMode,
    putFile_repositoryName,
    putFile_branchName,
    putFile_fileContent,
    putFile_filePath,

    -- * Destructuring the Response
    PutFileResponse (..),
    newPutFileResponse,

    -- * Response Lenses
    putFileResponse_httpStatus,
    putFileResponse_commitId,
    putFileResponse_blobId,
    putFileResponse_treeId,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutFile' smart constructor.
data PutFile = PutFile'
  { -- | The full commit ID of the head commit in the branch where you want to
    -- add or update the file. If this is an empty repository, no commit ID is
    -- required. If this is not an empty repository, a commit ID is required.
    --
    -- The commit ID must match the ID of the head commit at the time of the
    -- operation. Otherwise, an error occurs, and the file is not added or
    -- updated.
    parentCommitId :: Prelude.Maybe Prelude.Text,
    -- | A message about why this file was added or updated. Although it is
    -- optional, a message makes the commit history for your repository more
    -- useful.
    commitMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the person adding or updating the file. Although it is
    -- optional, a name makes the commit history for your repository more
    -- useful.
    name :: Prelude.Maybe Prelude.Text,
    -- | An email address for the person adding or updating the file.
    email :: Prelude.Maybe Prelude.Text,
    -- | The file mode permissions of the blob. Valid file mode permissions are
    -- listed here.
    fileMode :: Prelude.Maybe FileModeTypeEnum,
    -- | The name of the repository where you want to add or update the file.
    repositoryName :: Prelude.Text,
    -- | The name of the branch where you want to add or update the file. If this
    -- is an empty repository, this branch is created.
    branchName :: Prelude.Text,
    -- | The content of the file, in binary object format.
    fileContent :: Prelude.Base64,
    -- | The name of the file you want to add or update, including the relative
    -- path to the file in the repository.
    --
    -- If the path does not currently exist in the repository, the path is
    -- created as part of adding the file.
    filePath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutFile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentCommitId', 'putFile_parentCommitId' - The full commit ID of the head commit in the branch where you want to
-- add or update the file. If this is an empty repository, no commit ID is
-- required. If this is not an empty repository, a commit ID is required.
--
-- The commit ID must match the ID of the head commit at the time of the
-- operation. Otherwise, an error occurs, and the file is not added or
-- updated.
--
-- 'commitMessage', 'putFile_commitMessage' - A message about why this file was added or updated. Although it is
-- optional, a message makes the commit history for your repository more
-- useful.
--
-- 'name', 'putFile_name' - The name of the person adding or updating the file. Although it is
-- optional, a name makes the commit history for your repository more
-- useful.
--
-- 'email', 'putFile_email' - An email address for the person adding or updating the file.
--
-- 'fileMode', 'putFile_fileMode' - The file mode permissions of the blob. Valid file mode permissions are
-- listed here.
--
-- 'repositoryName', 'putFile_repositoryName' - The name of the repository where you want to add or update the file.
--
-- 'branchName', 'putFile_branchName' - The name of the branch where you want to add or update the file. If this
-- is an empty repository, this branch is created.
--
-- 'fileContent', 'putFile_fileContent' - The content of the file, in binary object format.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'filePath', 'putFile_filePath' - The name of the file you want to add or update, including the relative
-- path to the file in the repository.
--
-- If the path does not currently exist in the repository, the path is
-- created as part of adding the file.
newPutFile ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'branchName'
  Prelude.Text ->
  -- | 'fileContent'
  Prelude.ByteString ->
  -- | 'filePath'
  Prelude.Text ->
  PutFile
newPutFile
  pRepositoryName_
  pBranchName_
  pFileContent_
  pFilePath_ =
    PutFile'
      { parentCommitId = Prelude.Nothing,
        commitMessage = Prelude.Nothing,
        name = Prelude.Nothing,
        email = Prelude.Nothing,
        fileMode = Prelude.Nothing,
        repositoryName = pRepositoryName_,
        branchName = pBranchName_,
        fileContent = Prelude._Base64 Lens.# pFileContent_,
        filePath = pFilePath_
      }

-- | The full commit ID of the head commit in the branch where you want to
-- add or update the file. If this is an empty repository, no commit ID is
-- required. If this is not an empty repository, a commit ID is required.
--
-- The commit ID must match the ID of the head commit at the time of the
-- operation. Otherwise, an error occurs, and the file is not added or
-- updated.
putFile_parentCommitId :: Lens.Lens' PutFile (Prelude.Maybe Prelude.Text)
putFile_parentCommitId = Lens.lens (\PutFile' {parentCommitId} -> parentCommitId) (\s@PutFile' {} a -> s {parentCommitId = a} :: PutFile)

-- | A message about why this file was added or updated. Although it is
-- optional, a message makes the commit history for your repository more
-- useful.
putFile_commitMessage :: Lens.Lens' PutFile (Prelude.Maybe Prelude.Text)
putFile_commitMessage = Lens.lens (\PutFile' {commitMessage} -> commitMessage) (\s@PutFile' {} a -> s {commitMessage = a} :: PutFile)

-- | The name of the person adding or updating the file. Although it is
-- optional, a name makes the commit history for your repository more
-- useful.
putFile_name :: Lens.Lens' PutFile (Prelude.Maybe Prelude.Text)
putFile_name = Lens.lens (\PutFile' {name} -> name) (\s@PutFile' {} a -> s {name = a} :: PutFile)

-- | An email address for the person adding or updating the file.
putFile_email :: Lens.Lens' PutFile (Prelude.Maybe Prelude.Text)
putFile_email = Lens.lens (\PutFile' {email} -> email) (\s@PutFile' {} a -> s {email = a} :: PutFile)

-- | The file mode permissions of the blob. Valid file mode permissions are
-- listed here.
putFile_fileMode :: Lens.Lens' PutFile (Prelude.Maybe FileModeTypeEnum)
putFile_fileMode = Lens.lens (\PutFile' {fileMode} -> fileMode) (\s@PutFile' {} a -> s {fileMode = a} :: PutFile)

-- | The name of the repository where you want to add or update the file.
putFile_repositoryName :: Lens.Lens' PutFile Prelude.Text
putFile_repositoryName = Lens.lens (\PutFile' {repositoryName} -> repositoryName) (\s@PutFile' {} a -> s {repositoryName = a} :: PutFile)

-- | The name of the branch where you want to add or update the file. If this
-- is an empty repository, this branch is created.
putFile_branchName :: Lens.Lens' PutFile Prelude.Text
putFile_branchName = Lens.lens (\PutFile' {branchName} -> branchName) (\s@PutFile' {} a -> s {branchName = a} :: PutFile)

-- | The content of the file, in binary object format.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
putFile_fileContent :: Lens.Lens' PutFile Prelude.ByteString
putFile_fileContent = Lens.lens (\PutFile' {fileContent} -> fileContent) (\s@PutFile' {} a -> s {fileContent = a} :: PutFile) Prelude.. Prelude._Base64

-- | The name of the file you want to add or update, including the relative
-- path to the file in the repository.
--
-- If the path does not currently exist in the repository, the path is
-- created as part of adding the file.
putFile_filePath :: Lens.Lens' PutFile Prelude.Text
putFile_filePath = Lens.lens (\PutFile' {filePath} -> filePath) (\s@PutFile' {} a -> s {filePath = a} :: PutFile)

instance Prelude.AWSRequest PutFile where
  type Rs PutFile = PutFileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutFileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "commitId")
            Prelude.<*> (x Prelude..:> "blobId")
            Prelude.<*> (x Prelude..:> "treeId")
      )

instance Prelude.Hashable PutFile

instance Prelude.NFData PutFile

instance Prelude.ToHeaders PutFile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.PutFile" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutFile where
  toJSON PutFile' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("parentCommitId" Prelude..=)
              Prelude.<$> parentCommitId,
            ("commitMessage" Prelude..=)
              Prelude.<$> commitMessage,
            ("name" Prelude..=) Prelude.<$> name,
            ("email" Prelude..=) Prelude.<$> email,
            ("fileMode" Prelude..=) Prelude.<$> fileMode,
            Prelude.Just
              ("repositoryName" Prelude..= repositoryName),
            Prelude.Just ("branchName" Prelude..= branchName),
            Prelude.Just ("fileContent" Prelude..= fileContent),
            Prelude.Just ("filePath" Prelude..= filePath)
          ]
      )

instance Prelude.ToPath PutFile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutFile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutFileResponse' smart constructor.
data PutFileResponse = PutFileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The full SHA ID of the commit that contains this file change.
    commitId :: Prelude.Text,
    -- | The ID of the blob, which is its SHA-1 pointer.
    blobId :: Prelude.Text,
    -- | The full SHA-1 pointer of the tree information for the commit that
    -- contains this file change.
    treeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutFileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putFileResponse_httpStatus' - The response's http status code.
--
-- 'commitId', 'putFileResponse_commitId' - The full SHA ID of the commit that contains this file change.
--
-- 'blobId', 'putFileResponse_blobId' - The ID of the blob, which is its SHA-1 pointer.
--
-- 'treeId', 'putFileResponse_treeId' - The full SHA-1 pointer of the tree information for the commit that
-- contains this file change.
newPutFileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'commitId'
  Prelude.Text ->
  -- | 'blobId'
  Prelude.Text ->
  -- | 'treeId'
  Prelude.Text ->
  PutFileResponse
newPutFileResponse
  pHttpStatus_
  pCommitId_
  pBlobId_
  pTreeId_ =
    PutFileResponse'
      { httpStatus = pHttpStatus_,
        commitId = pCommitId_,
        blobId = pBlobId_,
        treeId = pTreeId_
      }

-- | The response's http status code.
putFileResponse_httpStatus :: Lens.Lens' PutFileResponse Prelude.Int
putFileResponse_httpStatus = Lens.lens (\PutFileResponse' {httpStatus} -> httpStatus) (\s@PutFileResponse' {} a -> s {httpStatus = a} :: PutFileResponse)

-- | The full SHA ID of the commit that contains this file change.
putFileResponse_commitId :: Lens.Lens' PutFileResponse Prelude.Text
putFileResponse_commitId = Lens.lens (\PutFileResponse' {commitId} -> commitId) (\s@PutFileResponse' {} a -> s {commitId = a} :: PutFileResponse)

-- | The ID of the blob, which is its SHA-1 pointer.
putFileResponse_blobId :: Lens.Lens' PutFileResponse Prelude.Text
putFileResponse_blobId = Lens.lens (\PutFileResponse' {blobId} -> blobId) (\s@PutFileResponse' {} a -> s {blobId = a} :: PutFileResponse)

-- | The full SHA-1 pointer of the tree information for the commit that
-- contains this file change.
putFileResponse_treeId :: Lens.Lens' PutFileResponse Prelude.Text
putFileResponse_treeId = Lens.lens (\PutFileResponse' {treeId} -> treeId) (\s@PutFileResponse' {} a -> s {treeId = a} :: PutFileResponse)

instance Prelude.NFData PutFileResponse
