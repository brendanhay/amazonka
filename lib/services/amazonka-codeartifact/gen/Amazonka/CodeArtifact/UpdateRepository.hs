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
-- Module      : Amazonka.CodeArtifact.UpdateRepository
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the properties of a repository.
module Amazonka.CodeArtifact.UpdateRepository
  ( -- * Creating a Request
    UpdateRepository (..),
    newUpdateRepository,

    -- * Request Lenses
    updateRepository_description,
    updateRepository_domainOwner,
    updateRepository_upstreams,
    updateRepository_domain,
    updateRepository_repository,

    -- * Destructuring the Response
    UpdateRepositoryResponse (..),
    newUpdateRepositoryResponse,

    -- * Response Lenses
    updateRepositoryResponse_repository,
    updateRepositoryResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRepository' smart constructor.
data UpdateRepository = UpdateRepository'
  { -- | An updated repository description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | A list of upstream repositories to associate with the repository. The
    -- order of the upstream repositories in the list determines their priority
    -- order when CodeArtifact looks for a requested package version. For more
    -- information, see
    -- <https://docs.aws.amazon.com/codeartifact/latest/ug/repos-upstream.html Working with upstream repositories>.
    upstreams :: Prelude.Maybe [UpstreamRepository],
    -- | The name of the domain associated with the repository to update.
    domain :: Prelude.Text,
    -- | The name of the repository to update.
    repository :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateRepository_description' - An updated repository description.
--
-- 'domainOwner', 'updateRepository_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'upstreams', 'updateRepository_upstreams' - A list of upstream repositories to associate with the repository. The
-- order of the upstream repositories in the list determines their priority
-- order when CodeArtifact looks for a requested package version. For more
-- information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/repos-upstream.html Working with upstream repositories>.
--
-- 'domain', 'updateRepository_domain' - The name of the domain associated with the repository to update.
--
-- 'repository', 'updateRepository_repository' - The name of the repository to update.
newUpdateRepository ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  UpdateRepository
newUpdateRepository pDomain_ pRepository_ =
  UpdateRepository'
    { description = Prelude.Nothing,
      domainOwner = Prelude.Nothing,
      upstreams = Prelude.Nothing,
      domain = pDomain_,
      repository = pRepository_
    }

-- | An updated repository description.
updateRepository_description :: Lens.Lens' UpdateRepository (Prelude.Maybe Prelude.Text)
updateRepository_description = Lens.lens (\UpdateRepository' {description} -> description) (\s@UpdateRepository' {} a -> s {description = a} :: UpdateRepository)

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
updateRepository_domainOwner :: Lens.Lens' UpdateRepository (Prelude.Maybe Prelude.Text)
updateRepository_domainOwner = Lens.lens (\UpdateRepository' {domainOwner} -> domainOwner) (\s@UpdateRepository' {} a -> s {domainOwner = a} :: UpdateRepository)

-- | A list of upstream repositories to associate with the repository. The
-- order of the upstream repositories in the list determines their priority
-- order when CodeArtifact looks for a requested package version. For more
-- information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/repos-upstream.html Working with upstream repositories>.
updateRepository_upstreams :: Lens.Lens' UpdateRepository (Prelude.Maybe [UpstreamRepository])
updateRepository_upstreams = Lens.lens (\UpdateRepository' {upstreams} -> upstreams) (\s@UpdateRepository' {} a -> s {upstreams = a} :: UpdateRepository) Prelude.. Lens.mapping Lens.coerced

-- | The name of the domain associated with the repository to update.
updateRepository_domain :: Lens.Lens' UpdateRepository Prelude.Text
updateRepository_domain = Lens.lens (\UpdateRepository' {domain} -> domain) (\s@UpdateRepository' {} a -> s {domain = a} :: UpdateRepository)

-- | The name of the repository to update.
updateRepository_repository :: Lens.Lens' UpdateRepository Prelude.Text
updateRepository_repository = Lens.lens (\UpdateRepository' {repository} -> repository) (\s@UpdateRepository' {} a -> s {repository = a} :: UpdateRepository)

instance Core.AWSRequest UpdateRepository where
  type
    AWSResponse UpdateRepository =
      UpdateRepositoryResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRepositoryResponse'
            Prelude.<$> (x Data..?> "repository")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRepository where
  hashWithSalt _salt UpdateRepository' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` upstreams
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` repository

instance Prelude.NFData UpdateRepository where
  rnf UpdateRepository' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf upstreams
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository

instance Data.ToHeaders UpdateRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRepository where
  toJSON UpdateRepository' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("upstreams" Data..=) Prelude.<$> upstreams
          ]
      )

instance Data.ToPath UpdateRepository where
  toPath = Prelude.const "/v1/repository"

instance Data.ToQuery UpdateRepository where
  toQuery UpdateRepository' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "domain" Data.=: domain,
        "repository" Data.=: repository
      ]

-- | /See:/ 'newUpdateRepositoryResponse' smart constructor.
data UpdateRepositoryResponse = UpdateRepositoryResponse'
  { -- | The updated repository.
    repository :: Prelude.Maybe RepositoryDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repository', 'updateRepositoryResponse_repository' - The updated repository.
--
-- 'httpStatus', 'updateRepositoryResponse_httpStatus' - The response's http status code.
newUpdateRepositoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRepositoryResponse
newUpdateRepositoryResponse pHttpStatus_ =
  UpdateRepositoryResponse'
    { repository =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated repository.
updateRepositoryResponse_repository :: Lens.Lens' UpdateRepositoryResponse (Prelude.Maybe RepositoryDescription)
updateRepositoryResponse_repository = Lens.lens (\UpdateRepositoryResponse' {repository} -> repository) (\s@UpdateRepositoryResponse' {} a -> s {repository = a} :: UpdateRepositoryResponse)

-- | The response's http status code.
updateRepositoryResponse_httpStatus :: Lens.Lens' UpdateRepositoryResponse Prelude.Int
updateRepositoryResponse_httpStatus = Lens.lens (\UpdateRepositoryResponse' {httpStatus} -> httpStatus) (\s@UpdateRepositoryResponse' {} a -> s {httpStatus = a} :: UpdateRepositoryResponse)

instance Prelude.NFData UpdateRepositoryResponse where
  rnf UpdateRepositoryResponse' {..} =
    Prelude.rnf repository
      `Prelude.seq` Prelude.rnf httpStatus
