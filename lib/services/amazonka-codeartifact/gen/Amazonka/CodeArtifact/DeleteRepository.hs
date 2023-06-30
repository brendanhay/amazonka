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
-- Module      : Amazonka.CodeArtifact.DeleteRepository
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a repository.
module Amazonka.CodeArtifact.DeleteRepository
  ( -- * Creating a Request
    DeleteRepository (..),
    newDeleteRepository,

    -- * Request Lenses
    deleteRepository_domainOwner,
    deleteRepository_domain,
    deleteRepository_repository,

    -- * Destructuring the Response
    DeleteRepositoryResponse (..),
    newDeleteRepositoryResponse,

    -- * Response Lenses
    deleteRepositoryResponse_repository,
    deleteRepositoryResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRepository' smart constructor.
data DeleteRepository = DeleteRepository'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the repository to delete.
    domain :: Prelude.Text,
    -- | The name of the repository to delete.
    repository :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'deleteRepository_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'domain', 'deleteRepository_domain' - The name of the domain that contains the repository to delete.
--
-- 'repository', 'deleteRepository_repository' - The name of the repository to delete.
newDeleteRepository ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  DeleteRepository
newDeleteRepository pDomain_ pRepository_ =
  DeleteRepository'
    { domainOwner = Prelude.Nothing,
      domain = pDomain_,
      repository = pRepository_
    }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
deleteRepository_domainOwner :: Lens.Lens' DeleteRepository (Prelude.Maybe Prelude.Text)
deleteRepository_domainOwner = Lens.lens (\DeleteRepository' {domainOwner} -> domainOwner) (\s@DeleteRepository' {} a -> s {domainOwner = a} :: DeleteRepository)

-- | The name of the domain that contains the repository to delete.
deleteRepository_domain :: Lens.Lens' DeleteRepository Prelude.Text
deleteRepository_domain = Lens.lens (\DeleteRepository' {domain} -> domain) (\s@DeleteRepository' {} a -> s {domain = a} :: DeleteRepository)

-- | The name of the repository to delete.
deleteRepository_repository :: Lens.Lens' DeleteRepository Prelude.Text
deleteRepository_repository = Lens.lens (\DeleteRepository' {repository} -> repository) (\s@DeleteRepository' {} a -> s {repository = a} :: DeleteRepository)

instance Core.AWSRequest DeleteRepository where
  type
    AWSResponse DeleteRepository =
      DeleteRepositoryResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRepositoryResponse'
            Prelude.<$> (x Data..?> "repository")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRepository where
  hashWithSalt _salt DeleteRepository' {..} =
    _salt
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` repository

instance Prelude.NFData DeleteRepository where
  rnf DeleteRepository' {..} =
    Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository

instance Data.ToHeaders DeleteRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteRepository where
  toPath = Prelude.const "/v1/repository"

instance Data.ToQuery DeleteRepository where
  toQuery DeleteRepository' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "domain" Data.=: domain,
        "repository" Data.=: repository
      ]

-- | /See:/ 'newDeleteRepositoryResponse' smart constructor.
data DeleteRepositoryResponse = DeleteRepositoryResponse'
  { -- | Information about the deleted repository after processing the request.
    repository :: Prelude.Maybe RepositoryDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repository', 'deleteRepositoryResponse_repository' - Information about the deleted repository after processing the request.
--
-- 'httpStatus', 'deleteRepositoryResponse_httpStatus' - The response's http status code.
newDeleteRepositoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRepositoryResponse
newDeleteRepositoryResponse pHttpStatus_ =
  DeleteRepositoryResponse'
    { repository =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the deleted repository after processing the request.
deleteRepositoryResponse_repository :: Lens.Lens' DeleteRepositoryResponse (Prelude.Maybe RepositoryDescription)
deleteRepositoryResponse_repository = Lens.lens (\DeleteRepositoryResponse' {repository} -> repository) (\s@DeleteRepositoryResponse' {} a -> s {repository = a} :: DeleteRepositoryResponse)

-- | The response's http status code.
deleteRepositoryResponse_httpStatus :: Lens.Lens' DeleteRepositoryResponse Prelude.Int
deleteRepositoryResponse_httpStatus = Lens.lens (\DeleteRepositoryResponse' {httpStatus} -> httpStatus) (\s@DeleteRepositoryResponse' {} a -> s {httpStatus = a} :: DeleteRepositoryResponse)

instance Prelude.NFData DeleteRepositoryResponse where
  rnf DeleteRepositoryResponse' {..} =
    Prelude.rnf repository
      `Prelude.seq` Prelude.rnf httpStatus
