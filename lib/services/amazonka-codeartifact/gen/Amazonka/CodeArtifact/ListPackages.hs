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
-- Module      : Amazonka.CodeArtifact.ListPackages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageSummary.html PackageSummary>
-- objects for packages in a repository that match the request parameters.
--
-- This operation returns paginated results.
module Amazonka.CodeArtifact.ListPackages
  ( -- * Creating a Request
    ListPackages (..),
    newListPackages,

    -- * Request Lenses
    listPackages_format,
    listPackages_namespace,
    listPackages_domainOwner,
    listPackages_nextToken,
    listPackages_packagePrefix,
    listPackages_maxResults,
    listPackages_domain,
    listPackages_repository,

    -- * Destructuring the Response
    ListPackagesResponse (..),
    newListPackagesResponse,

    -- * Response Lenses
    listPackagesResponse_packages,
    listPackagesResponse_nextToken,
    listPackagesResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPackages' smart constructor.
data ListPackages = ListPackages'
  { -- | The format of the packages. The valid package types are:
    --
    -- -   @npm@: A Node Package Manager (npm) package.
    --
    -- -   @pypi@: A Python Package Index (PyPI) package.
    --
    -- -   @maven@: A Maven package that contains compiled code in a
    --     distributable format, such as a JAR file.
    format :: Prelude.Maybe PackageFormat,
    -- | The namespace of the package. The package component that specifies its
    -- namespace depends on its type. For example:
    --
    -- -   The namespace of a Maven package is its @groupId@.
    --
    -- -   The namespace of an npm package is its @scope@.
    --
    -- -   A Python package does not contain a corresponding component, so
    --     Python packages do not have a namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit account number of the AWS account that owns the domain. It
    -- does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A prefix used to filter returned packages. Only packages with names that
    -- start with @packagePrefix@ are returned.
    packagePrefix :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the domain that contains the repository that contains the
    -- requested list of packages.
    domain :: Prelude.Text,
    -- | The name of the repository from which packages are to be listed.
    repository :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'listPackages_format' - The format of the packages. The valid package types are:
--
-- -   @npm@: A Node Package Manager (npm) package.
--
-- -   @pypi@: A Python Package Index (PyPI) package.
--
-- -   @maven@: A Maven package that contains compiled code in a
--     distributable format, such as a JAR file.
--
-- 'namespace', 'listPackages_namespace' - The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   A Python package does not contain a corresponding component, so
--     Python packages do not have a namespace.
--
-- 'domainOwner', 'listPackages_domainOwner' - The 12-digit account number of the AWS account that owns the domain. It
-- does not include dashes or spaces.
--
-- 'nextToken', 'listPackages_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'packagePrefix', 'listPackages_packagePrefix' - A prefix used to filter returned packages. Only packages with names that
-- start with @packagePrefix@ are returned.
--
-- 'maxResults', 'listPackages_maxResults' - The maximum number of results to return per page.
--
-- 'domain', 'listPackages_domain' - The name of the domain that contains the repository that contains the
-- requested list of packages.
--
-- 'repository', 'listPackages_repository' - The name of the repository from which packages are to be listed.
newListPackages ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  ListPackages
newListPackages pDomain_ pRepository_ =
  ListPackages'
    { format = Prelude.Nothing,
      namespace = Prelude.Nothing,
      domainOwner = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      packagePrefix = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      domain = pDomain_,
      repository = pRepository_
    }

-- | The format of the packages. The valid package types are:
--
-- -   @npm@: A Node Package Manager (npm) package.
--
-- -   @pypi@: A Python Package Index (PyPI) package.
--
-- -   @maven@: A Maven package that contains compiled code in a
--     distributable format, such as a JAR file.
listPackages_format :: Lens.Lens' ListPackages (Prelude.Maybe PackageFormat)
listPackages_format = Lens.lens (\ListPackages' {format} -> format) (\s@ListPackages' {} a -> s {format = a} :: ListPackages)

-- | The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   A Python package does not contain a corresponding component, so
--     Python packages do not have a namespace.
listPackages_namespace :: Lens.Lens' ListPackages (Prelude.Maybe Prelude.Text)
listPackages_namespace = Lens.lens (\ListPackages' {namespace} -> namespace) (\s@ListPackages' {} a -> s {namespace = a} :: ListPackages)

-- | The 12-digit account number of the AWS account that owns the domain. It
-- does not include dashes or spaces.
listPackages_domainOwner :: Lens.Lens' ListPackages (Prelude.Maybe Prelude.Text)
listPackages_domainOwner = Lens.lens (\ListPackages' {domainOwner} -> domainOwner) (\s@ListPackages' {} a -> s {domainOwner = a} :: ListPackages)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listPackages_nextToken :: Lens.Lens' ListPackages (Prelude.Maybe Prelude.Text)
listPackages_nextToken = Lens.lens (\ListPackages' {nextToken} -> nextToken) (\s@ListPackages' {} a -> s {nextToken = a} :: ListPackages)

-- | A prefix used to filter returned packages. Only packages with names that
-- start with @packagePrefix@ are returned.
listPackages_packagePrefix :: Lens.Lens' ListPackages (Prelude.Maybe Prelude.Text)
listPackages_packagePrefix = Lens.lens (\ListPackages' {packagePrefix} -> packagePrefix) (\s@ListPackages' {} a -> s {packagePrefix = a} :: ListPackages)

-- | The maximum number of results to return per page.
listPackages_maxResults :: Lens.Lens' ListPackages (Prelude.Maybe Prelude.Natural)
listPackages_maxResults = Lens.lens (\ListPackages' {maxResults} -> maxResults) (\s@ListPackages' {} a -> s {maxResults = a} :: ListPackages)

-- | The name of the domain that contains the repository that contains the
-- requested list of packages.
listPackages_domain :: Lens.Lens' ListPackages Prelude.Text
listPackages_domain = Lens.lens (\ListPackages' {domain} -> domain) (\s@ListPackages' {} a -> s {domain = a} :: ListPackages)

-- | The name of the repository from which packages are to be listed.
listPackages_repository :: Lens.Lens' ListPackages Prelude.Text
listPackages_repository = Lens.lens (\ListPackages' {repository} -> repository) (\s@ListPackages' {} a -> s {repository = a} :: ListPackages)

instance Core.AWSPager ListPackages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPackagesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPackagesResponse_packages Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPackages_nextToken
          Lens..~ rs
          Lens.^? listPackagesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListPackages where
  type AWSResponse ListPackages = ListPackagesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPackagesResponse'
            Prelude.<$> (x Core..?> "packages" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPackages where
  hashWithSalt salt' ListPackages' {..} =
    salt' `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` packagePrefix
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` format

instance Prelude.NFData ListPackages where
  rnf ListPackages' {..} =
    Prelude.rnf format
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf packagePrefix
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf namespace

instance Core.ToHeaders ListPackages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPackages where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath ListPackages where
  toPath = Prelude.const "/v1/packages"

instance Core.ToQuery ListPackages where
  toQuery ListPackages' {..} =
    Prelude.mconcat
      [ "format" Core.=: format,
        "namespace" Core.=: namespace,
        "domain-owner" Core.=: domainOwner,
        "next-token" Core.=: nextToken,
        "package-prefix" Core.=: packagePrefix,
        "max-results" Core.=: maxResults,
        "domain" Core.=: domain,
        "repository" Core.=: repository
      ]

-- | /See:/ 'newListPackagesResponse' smart constructor.
data ListPackagesResponse = ListPackagesResponse'
  { -- | The list of returned
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageSummary.html PackageSummary>
    -- objects.
    packages :: Prelude.Maybe [PackageSummary],
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packages', 'listPackagesResponse_packages' - The list of returned
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageSummary.html PackageSummary>
-- objects.
--
-- 'nextToken', 'listPackagesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listPackagesResponse_httpStatus' - The response's http status code.
newListPackagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPackagesResponse
newListPackagesResponse pHttpStatus_ =
  ListPackagesResponse'
    { packages = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of returned
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageSummary.html PackageSummary>
-- objects.
listPackagesResponse_packages :: Lens.Lens' ListPackagesResponse (Prelude.Maybe [PackageSummary])
listPackagesResponse_packages = Lens.lens (\ListPackagesResponse' {packages} -> packages) (\s@ListPackagesResponse' {} a -> s {packages = a} :: ListPackagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are additional results, this is the token for the next set of
-- results.
listPackagesResponse_nextToken :: Lens.Lens' ListPackagesResponse (Prelude.Maybe Prelude.Text)
listPackagesResponse_nextToken = Lens.lens (\ListPackagesResponse' {nextToken} -> nextToken) (\s@ListPackagesResponse' {} a -> s {nextToken = a} :: ListPackagesResponse)

-- | The response's http status code.
listPackagesResponse_httpStatus :: Lens.Lens' ListPackagesResponse Prelude.Int
listPackagesResponse_httpStatus = Lens.lens (\ListPackagesResponse' {httpStatus} -> httpStatus) (\s@ListPackagesResponse' {} a -> s {httpStatus = a} :: ListPackagesResponse)

instance Prelude.NFData ListPackagesResponse where
  rnf ListPackagesResponse' {..} =
    Prelude.rnf packages
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
