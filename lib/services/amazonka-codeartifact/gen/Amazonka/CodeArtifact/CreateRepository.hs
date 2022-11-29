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
-- Module      : Amazonka.CodeArtifact.CreateRepository
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a repository.
module Amazonka.CodeArtifact.CreateRepository
  ( -- * Creating a Request
    CreateRepository (..),
    newCreateRepository,

    -- * Request Lenses
    createRepository_tags,
    createRepository_upstreams,
    createRepository_description,
    createRepository_domainOwner,
    createRepository_domain,
    createRepository_repository,

    -- * Destructuring the Response
    CreateRepositoryResponse (..),
    newCreateRepositoryResponse,

    -- * Response Lenses
    createRepositoryResponse_repository,
    createRepositoryResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRepository' smart constructor.
data CreateRepository = CreateRepository'
  { -- | One or more tag key-value pairs for the repository.
    tags :: Prelude.Maybe [Tag],
    -- | A list of upstream repositories to associate with the repository. The
    -- order of the upstream repositories in the list determines their priority
    -- order when CodeArtifact looks for a requested package version. For more
    -- information, see
    -- <https://docs.aws.amazon.com/codeartifact/latest/ug/repos-upstream.html Working with upstream repositories>.
    upstreams :: Prelude.Maybe [UpstreamRepository],
    -- | A description of the created repository.
    description :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the created repository.
    domain :: Prelude.Text,
    -- | The name of the repository to create.
    repository :: Prelude.Text
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
-- 'tags', 'createRepository_tags' - One or more tag key-value pairs for the repository.
--
-- 'upstreams', 'createRepository_upstreams' - A list of upstream repositories to associate with the repository. The
-- order of the upstream repositories in the list determines their priority
-- order when CodeArtifact looks for a requested package version. For more
-- information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/repos-upstream.html Working with upstream repositories>.
--
-- 'description', 'createRepository_description' - A description of the created repository.
--
-- 'domainOwner', 'createRepository_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'domain', 'createRepository_domain' - The name of the domain that contains the created repository.
--
-- 'repository', 'createRepository_repository' - The name of the repository to create.
newCreateRepository ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  CreateRepository
newCreateRepository pDomain_ pRepository_ =
  CreateRepository'
    { tags = Prelude.Nothing,
      upstreams = Prelude.Nothing,
      description = Prelude.Nothing,
      domainOwner = Prelude.Nothing,
      domain = pDomain_,
      repository = pRepository_
    }

-- | One or more tag key-value pairs for the repository.
createRepository_tags :: Lens.Lens' CreateRepository (Prelude.Maybe [Tag])
createRepository_tags = Lens.lens (\CreateRepository' {tags} -> tags) (\s@CreateRepository' {} a -> s {tags = a} :: CreateRepository) Prelude.. Lens.mapping Lens.coerced

-- | A list of upstream repositories to associate with the repository. The
-- order of the upstream repositories in the list determines their priority
-- order when CodeArtifact looks for a requested package version. For more
-- information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/repos-upstream.html Working with upstream repositories>.
createRepository_upstreams :: Lens.Lens' CreateRepository (Prelude.Maybe [UpstreamRepository])
createRepository_upstreams = Lens.lens (\CreateRepository' {upstreams} -> upstreams) (\s@CreateRepository' {} a -> s {upstreams = a} :: CreateRepository) Prelude.. Lens.mapping Lens.coerced

-- | A description of the created repository.
createRepository_description :: Lens.Lens' CreateRepository (Prelude.Maybe Prelude.Text)
createRepository_description = Lens.lens (\CreateRepository' {description} -> description) (\s@CreateRepository' {} a -> s {description = a} :: CreateRepository)

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
createRepository_domainOwner :: Lens.Lens' CreateRepository (Prelude.Maybe Prelude.Text)
createRepository_domainOwner = Lens.lens (\CreateRepository' {domainOwner} -> domainOwner) (\s@CreateRepository' {} a -> s {domainOwner = a} :: CreateRepository)

-- | The name of the domain that contains the created repository.
createRepository_domain :: Lens.Lens' CreateRepository Prelude.Text
createRepository_domain = Lens.lens (\CreateRepository' {domain} -> domain) (\s@CreateRepository' {} a -> s {domain = a} :: CreateRepository)

-- | The name of the repository to create.
createRepository_repository :: Lens.Lens' CreateRepository Prelude.Text
createRepository_repository = Lens.lens (\CreateRepository' {repository} -> repository) (\s@CreateRepository' {} a -> s {repository = a} :: CreateRepository)

instance Core.AWSRequest CreateRepository where
  type
    AWSResponse CreateRepository =
      CreateRepositoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRepositoryResponse'
            Prelude.<$> (x Core..?> "repository")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRepository where
  hashWithSalt _salt CreateRepository' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` upstreams
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` repository

instance Prelude.NFData CreateRepository where
  rnf CreateRepository' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf upstreams
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository

instance Core.ToHeaders CreateRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateRepository where
  toJSON CreateRepository' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("upstreams" Core..=) Prelude.<$> upstreams,
            ("description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath CreateRepository where
  toPath = Prelude.const "/v1/repository"

instance Core.ToQuery CreateRepository where
  toQuery CreateRepository' {..} =
    Prelude.mconcat
      [ "domain-owner" Core.=: domainOwner,
        "domain" Core.=: domain,
        "repository" Core.=: repository
      ]

-- | /See:/ 'newCreateRepositoryResponse' smart constructor.
data CreateRepositoryResponse = CreateRepositoryResponse'
  { -- | Information about the created repository after processing the request.
    repository :: Prelude.Maybe RepositoryDescription,
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
-- 'repository', 'createRepositoryResponse_repository' - Information about the created repository after processing the request.
--
-- 'httpStatus', 'createRepositoryResponse_httpStatus' - The response's http status code.
newCreateRepositoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRepositoryResponse
newCreateRepositoryResponse pHttpStatus_ =
  CreateRepositoryResponse'
    { repository =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the created repository after processing the request.
createRepositoryResponse_repository :: Lens.Lens' CreateRepositoryResponse (Prelude.Maybe RepositoryDescription)
createRepositoryResponse_repository = Lens.lens (\CreateRepositoryResponse' {repository} -> repository) (\s@CreateRepositoryResponse' {} a -> s {repository = a} :: CreateRepositoryResponse)

-- | The response's http status code.
createRepositoryResponse_httpStatus :: Lens.Lens' CreateRepositoryResponse Prelude.Int
createRepositoryResponse_httpStatus = Lens.lens (\CreateRepositoryResponse' {httpStatus} -> httpStatus) (\s@CreateRepositoryResponse' {} a -> s {httpStatus = a} :: CreateRepositoryResponse)

instance Prelude.NFData CreateRepositoryResponse where
  rnf CreateRepositoryResponse' {..} =
    Prelude.rnf repository
      `Prelude.seq` Prelude.rnf httpStatus
