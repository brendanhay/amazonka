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
-- Module      : Network.AWS.CodeCommit.CreateRepository
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, empty repository.
module Network.AWS.CodeCommit.CreateRepository
  ( -- * Creating a Request
    CreateRepository (..),
    newCreateRepository,

    -- * Request Lenses
    createRepository_repositoryDescription,
    createRepository_tags,
    createRepository_repositoryName,

    -- * Destructuring the Response
    CreateRepositoryResponse (..),
    newCreateRepositoryResponse,

    -- * Response Lenses
    createRepositoryResponse_repositoryMetadata,
    createRepositoryResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a create repository operation.
--
-- /See:/ 'newCreateRepository' smart constructor.
data CreateRepository = CreateRepository'
  { -- | A comment or description about the new repository.
    --
    -- The description field for a repository accepts all HTML characters and
    -- all valid Unicode characters. Applications that do not HTML-encode the
    -- description and display it in a webpage can expose users to potentially
    -- malicious code. Make sure that you HTML-encode the description field in
    -- any application that uses this API to display the repository description
    -- on a webpage.
    repositoryDescription :: Prelude.Maybe Prelude.Text,
    -- | One or more tag key-value pairs to use when tagging this repository.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the new repository to be created.
    --
    -- The repository name must be unique across the calling AWS account.
    -- Repository names are limited to 100 alphanumeric, dash, and underscore
    -- characters, and cannot include certain characters. For more information
    -- about the limits on repository names, see
    -- <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html Limits>
    -- in the /AWS CodeCommit User Guide/. The suffix .git is prohibited.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryDescription', 'createRepository_repositoryDescription' - A comment or description about the new repository.
--
-- The description field for a repository accepts all HTML characters and
-- all valid Unicode characters. Applications that do not HTML-encode the
-- description and display it in a webpage can expose users to potentially
-- malicious code. Make sure that you HTML-encode the description field in
-- any application that uses this API to display the repository description
-- on a webpage.
--
-- 'tags', 'createRepository_tags' - One or more tag key-value pairs to use when tagging this repository.
--
-- 'repositoryName', 'createRepository_repositoryName' - The name of the new repository to be created.
--
-- The repository name must be unique across the calling AWS account.
-- Repository names are limited to 100 alphanumeric, dash, and underscore
-- characters, and cannot include certain characters. For more information
-- about the limits on repository names, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html Limits>
-- in the /AWS CodeCommit User Guide/. The suffix .git is prohibited.
newCreateRepository ::
  -- | 'repositoryName'
  Prelude.Text ->
  CreateRepository
newCreateRepository pRepositoryName_ =
  CreateRepository'
    { repositoryDescription =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | A comment or description about the new repository.
--
-- The description field for a repository accepts all HTML characters and
-- all valid Unicode characters. Applications that do not HTML-encode the
-- description and display it in a webpage can expose users to potentially
-- malicious code. Make sure that you HTML-encode the description field in
-- any application that uses this API to display the repository description
-- on a webpage.
createRepository_repositoryDescription :: Lens.Lens' CreateRepository (Prelude.Maybe Prelude.Text)
createRepository_repositoryDescription = Lens.lens (\CreateRepository' {repositoryDescription} -> repositoryDescription) (\s@CreateRepository' {} a -> s {repositoryDescription = a} :: CreateRepository)

-- | One or more tag key-value pairs to use when tagging this repository.
createRepository_tags :: Lens.Lens' CreateRepository (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRepository_tags = Lens.lens (\CreateRepository' {tags} -> tags) (\s@CreateRepository' {} a -> s {tags = a} :: CreateRepository) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the new repository to be created.
--
-- The repository name must be unique across the calling AWS account.
-- Repository names are limited to 100 alphanumeric, dash, and underscore
-- characters, and cannot include certain characters. For more information
-- about the limits on repository names, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html Limits>
-- in the /AWS CodeCommit User Guide/. The suffix .git is prohibited.
createRepository_repositoryName :: Lens.Lens' CreateRepository Prelude.Text
createRepository_repositoryName = Lens.lens (\CreateRepository' {repositoryName} -> repositoryName) (\s@CreateRepository' {} a -> s {repositoryName = a} :: CreateRepository)

instance Core.AWSRequest CreateRepository where
  type
    AWSResponse CreateRepository =
      CreateRepositoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRepositoryResponse'
            Prelude.<$> (x Core..?> "repositoryMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRepository

instance Prelude.NFData CreateRepository

instance Core.ToHeaders CreateRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.CreateRepository" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateRepository where
  toJSON CreateRepository' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("repositoryDescription" Core..=)
              Prelude.<$> repositoryDescription,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath CreateRepository where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateRepository where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a create repository operation.
--
-- /See:/ 'newCreateRepositoryResponse' smart constructor.
data CreateRepositoryResponse = CreateRepositoryResponse'
  { -- | Information about the newly created repository.
    repositoryMetadata :: Prelude.Maybe RepositoryMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryMetadata', 'createRepositoryResponse_repositoryMetadata' - Information about the newly created repository.
--
-- 'httpStatus', 'createRepositoryResponse_httpStatus' - The response's http status code.
newCreateRepositoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRepositoryResponse
newCreateRepositoryResponse pHttpStatus_ =
  CreateRepositoryResponse'
    { repositoryMetadata =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the newly created repository.
createRepositoryResponse_repositoryMetadata :: Lens.Lens' CreateRepositoryResponse (Prelude.Maybe RepositoryMetadata)
createRepositoryResponse_repositoryMetadata = Lens.lens (\CreateRepositoryResponse' {repositoryMetadata} -> repositoryMetadata) (\s@CreateRepositoryResponse' {} a -> s {repositoryMetadata = a} :: CreateRepositoryResponse)

-- | The response's http status code.
createRepositoryResponse_httpStatus :: Lens.Lens' CreateRepositoryResponse Prelude.Int
createRepositoryResponse_httpStatus = Lens.lens (\CreateRepositoryResponse' {httpStatus} -> httpStatus) (\s@CreateRepositoryResponse' {} a -> s {httpStatus = a} :: CreateRepositoryResponse)

instance Prelude.NFData CreateRepositoryResponse
