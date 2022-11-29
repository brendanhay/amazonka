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
-- Module      : Amazonka.CodeCommit.GetRepository
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CodeCommit.GetRepository
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

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a get repository operation.
--
-- /See:/ 'newGetRepository' smart constructor.
data GetRepository = GetRepository'
  { -- | The name of the repository to get information about.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetRepository
newGetRepository pRepositoryName_ =
  GetRepository' {repositoryName = pRepositoryName_}

-- | The name of the repository to get information about.
getRepository_repositoryName :: Lens.Lens' GetRepository Prelude.Text
getRepository_repositoryName = Lens.lens (\GetRepository' {repositoryName} -> repositoryName) (\s@GetRepository' {} a -> s {repositoryName = a} :: GetRepository)

instance Core.AWSRequest GetRepository where
  type
    AWSResponse GetRepository =
      GetRepositoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRepositoryResponse'
            Prelude.<$> (x Core..?> "repositoryMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRepository where
  hashWithSalt _salt GetRepository' {..} =
    _salt `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData GetRepository where
  rnf GetRepository' {..} = Prelude.rnf repositoryName

instance Core.ToHeaders GetRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.GetRepository" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRepository where
  toJSON GetRepository' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath GetRepository where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRepository where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a get repository operation.
--
-- /See:/ 'newGetRepositoryResponse' smart constructor.
data GetRepositoryResponse = GetRepositoryResponse'
  { -- | Information about the repository.
    repositoryMetadata :: Prelude.Maybe RepositoryMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetRepositoryResponse
newGetRepositoryResponse pHttpStatus_ =
  GetRepositoryResponse'
    { repositoryMetadata =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the repository.
getRepositoryResponse_repositoryMetadata :: Lens.Lens' GetRepositoryResponse (Prelude.Maybe RepositoryMetadata)
getRepositoryResponse_repositoryMetadata = Lens.lens (\GetRepositoryResponse' {repositoryMetadata} -> repositoryMetadata) (\s@GetRepositoryResponse' {} a -> s {repositoryMetadata = a} :: GetRepositoryResponse)

-- | The response's http status code.
getRepositoryResponse_httpStatus :: Lens.Lens' GetRepositoryResponse Prelude.Int
getRepositoryResponse_httpStatus = Lens.lens (\GetRepositoryResponse' {httpStatus} -> httpStatus) (\s@GetRepositoryResponse' {} a -> s {httpStatus = a} :: GetRepositoryResponse)

instance Prelude.NFData GetRepositoryResponse where
  rnf GetRepositoryResponse' {..} =
    Prelude.rnf repositoryMetadata
      `Prelude.seq` Prelude.rnf httpStatus
