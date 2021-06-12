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
-- Module      : Network.AWS.CodeCommit.GetRepository
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a repository.
--
-- The description field for a repository accepts all HTML characters and
-- all valid Unicode characters. Applications that do not HTML-encode the
-- description and display it in a webpage can expose users to potentially
-- malicious code. Make sure that you HTML-encode the description field in
-- any application that uses this API to display the repository description
-- on a webpage.
module Network.AWS.CodeCommit.GetRepository
  ( -- * Creating a Request
    GetRepository (..),
    newGetRepository,

    -- * Request Lenses
    getRepository_repositoryName,

    -- * Destructuring the Response
    GetRepositoryResponse (..),
    newGetRepositoryResponse,

    -- * Response Lenses
    getRepositoryResponse_repositoryMetadata,
    getRepositoryResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a get repository operation.
--
-- /See:/ 'newGetRepository' smart constructor.
data GetRepository = GetRepository'
  { -- | The name of the repository to get information about.
    repositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryName', 'getRepository_repositoryName' - The name of the repository to get information about.
newGetRepository ::
  -- | 'repositoryName'
  Core.Text ->
  GetRepository
newGetRepository pRepositoryName_ =
  GetRepository' {repositoryName = pRepositoryName_}

-- | The name of the repository to get information about.
getRepository_repositoryName :: Lens.Lens' GetRepository Core.Text
getRepository_repositoryName = Lens.lens (\GetRepository' {repositoryName} -> repositoryName) (\s@GetRepository' {} a -> s {repositoryName = a} :: GetRepository)

instance Core.AWSRequest GetRepository where
  type
    AWSResponse GetRepository =
      GetRepositoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRepositoryResponse'
            Core.<$> (x Core..?> "repositoryMetadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRepository

instance Core.NFData GetRepository

instance Core.ToHeaders GetRepository where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.GetRepository" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRepository where
  toJSON GetRepository' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath GetRepository where
  toPath = Core.const "/"

instance Core.ToQuery GetRepository where
  toQuery = Core.const Core.mempty

-- | Represents the output of a get repository operation.
--
-- /See:/ 'newGetRepositoryResponse' smart constructor.
data GetRepositoryResponse = GetRepositoryResponse'
  { -- | Information about the repository.
    repositoryMetadata :: Core.Maybe RepositoryMetadata,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryMetadata', 'getRepositoryResponse_repositoryMetadata' - Information about the repository.
--
-- 'httpStatus', 'getRepositoryResponse_httpStatus' - The response's http status code.
newGetRepositoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetRepositoryResponse
newGetRepositoryResponse pHttpStatus_ =
  GetRepositoryResponse'
    { repositoryMetadata =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the repository.
getRepositoryResponse_repositoryMetadata :: Lens.Lens' GetRepositoryResponse (Core.Maybe RepositoryMetadata)
getRepositoryResponse_repositoryMetadata = Lens.lens (\GetRepositoryResponse' {repositoryMetadata} -> repositoryMetadata) (\s@GetRepositoryResponse' {} a -> s {repositoryMetadata = a} :: GetRepositoryResponse)

-- | The response's http status code.
getRepositoryResponse_httpStatus :: Lens.Lens' GetRepositoryResponse Core.Int
getRepositoryResponse_httpStatus = Lens.lens (\GetRepositoryResponse' {httpStatus} -> httpStatus) (\s@GetRepositoryResponse' {} a -> s {httpStatus = a} :: GetRepositoryResponse)

instance Core.NFData GetRepositoryResponse
