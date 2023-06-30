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
-- Module      : Amazonka.CodeArtifact.GetRepositoryEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the endpoint of a repository for a specific package format. A
-- repository has one endpoint for each package format:
--
-- -   @maven@
--
-- -   @npm@
--
-- -   @nuget@
--
-- -   @pypi@
module Amazonka.CodeArtifact.GetRepositoryEndpoint
  ( -- * Creating a Request
    GetRepositoryEndpoint (..),
    newGetRepositoryEndpoint,

    -- * Request Lenses
    getRepositoryEndpoint_domainOwner,
    getRepositoryEndpoint_domain,
    getRepositoryEndpoint_repository,
    getRepositoryEndpoint_format,

    -- * Destructuring the Response
    GetRepositoryEndpointResponse (..),
    newGetRepositoryEndpointResponse,

    -- * Response Lenses
    getRepositoryEndpointResponse_repositoryEndpoint,
    getRepositoryEndpointResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRepositoryEndpoint' smart constructor.
data GetRepositoryEndpoint = GetRepositoryEndpoint'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain that contains the repository. It does not include dashes or
    -- spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the repository.
    domain :: Prelude.Text,
    -- | The name of the repository.
    repository :: Prelude.Text,
    -- | Returns which endpoint of a repository to return. A repository has one
    -- endpoint for each package format.
    format :: PackageFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRepositoryEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'getRepositoryEndpoint_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain that contains the repository. It does not include dashes or
-- spaces.
--
-- 'domain', 'getRepositoryEndpoint_domain' - The name of the domain that contains the repository.
--
-- 'repository', 'getRepositoryEndpoint_repository' - The name of the repository.
--
-- 'format', 'getRepositoryEndpoint_format' - Returns which endpoint of a repository to return. A repository has one
-- endpoint for each package format.
newGetRepositoryEndpoint ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  -- | 'format'
  PackageFormat ->
  GetRepositoryEndpoint
newGetRepositoryEndpoint
  pDomain_
  pRepository_
  pFormat_ =
    GetRepositoryEndpoint'
      { domainOwner =
          Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        format = pFormat_
      }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain that contains the repository. It does not include dashes or
-- spaces.
getRepositoryEndpoint_domainOwner :: Lens.Lens' GetRepositoryEndpoint (Prelude.Maybe Prelude.Text)
getRepositoryEndpoint_domainOwner = Lens.lens (\GetRepositoryEndpoint' {domainOwner} -> domainOwner) (\s@GetRepositoryEndpoint' {} a -> s {domainOwner = a} :: GetRepositoryEndpoint)

-- | The name of the domain that contains the repository.
getRepositoryEndpoint_domain :: Lens.Lens' GetRepositoryEndpoint Prelude.Text
getRepositoryEndpoint_domain = Lens.lens (\GetRepositoryEndpoint' {domain} -> domain) (\s@GetRepositoryEndpoint' {} a -> s {domain = a} :: GetRepositoryEndpoint)

-- | The name of the repository.
getRepositoryEndpoint_repository :: Lens.Lens' GetRepositoryEndpoint Prelude.Text
getRepositoryEndpoint_repository = Lens.lens (\GetRepositoryEndpoint' {repository} -> repository) (\s@GetRepositoryEndpoint' {} a -> s {repository = a} :: GetRepositoryEndpoint)

-- | Returns which endpoint of a repository to return. A repository has one
-- endpoint for each package format.
getRepositoryEndpoint_format :: Lens.Lens' GetRepositoryEndpoint PackageFormat
getRepositoryEndpoint_format = Lens.lens (\GetRepositoryEndpoint' {format} -> format) (\s@GetRepositoryEndpoint' {} a -> s {format = a} :: GetRepositoryEndpoint)

instance Core.AWSRequest GetRepositoryEndpoint where
  type
    AWSResponse GetRepositoryEndpoint =
      GetRepositoryEndpointResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRepositoryEndpointResponse'
            Prelude.<$> (x Data..?> "repositoryEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRepositoryEndpoint where
  hashWithSalt _salt GetRepositoryEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` format

instance Prelude.NFData GetRepositoryEndpoint where
  rnf GetRepositoryEndpoint' {..} =
    Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf format

instance Data.ToHeaders GetRepositoryEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRepositoryEndpoint where
  toPath = Prelude.const "/v1/repository/endpoint"

instance Data.ToQuery GetRepositoryEndpoint where
  toQuery GetRepositoryEndpoint' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "domain" Data.=: domain,
        "repository" Data.=: repository,
        "format" Data.=: format
      ]

-- | /See:/ 'newGetRepositoryEndpointResponse' smart constructor.
data GetRepositoryEndpointResponse = GetRepositoryEndpointResponse'
  { -- | A string that specifies the URL of the returned endpoint.
    repositoryEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRepositoryEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryEndpoint', 'getRepositoryEndpointResponse_repositoryEndpoint' - A string that specifies the URL of the returned endpoint.
--
-- 'httpStatus', 'getRepositoryEndpointResponse_httpStatus' - The response's http status code.
newGetRepositoryEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRepositoryEndpointResponse
newGetRepositoryEndpointResponse pHttpStatus_ =
  GetRepositoryEndpointResponse'
    { repositoryEndpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that specifies the URL of the returned endpoint.
getRepositoryEndpointResponse_repositoryEndpoint :: Lens.Lens' GetRepositoryEndpointResponse (Prelude.Maybe Prelude.Text)
getRepositoryEndpointResponse_repositoryEndpoint = Lens.lens (\GetRepositoryEndpointResponse' {repositoryEndpoint} -> repositoryEndpoint) (\s@GetRepositoryEndpointResponse' {} a -> s {repositoryEndpoint = a} :: GetRepositoryEndpointResponse)

-- | The response's http status code.
getRepositoryEndpointResponse_httpStatus :: Lens.Lens' GetRepositoryEndpointResponse Prelude.Int
getRepositoryEndpointResponse_httpStatus = Lens.lens (\GetRepositoryEndpointResponse' {httpStatus} -> httpStatus) (\s@GetRepositoryEndpointResponse' {} a -> s {httpStatus = a} :: GetRepositoryEndpointResponse)

instance Prelude.NFData GetRepositoryEndpointResponse where
  rnf GetRepositoryEndpointResponse' {..} =
    Prelude.rnf repositoryEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
