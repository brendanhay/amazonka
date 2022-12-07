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
-- Module      : Amazonka.CodeArtifact.ListPackageVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageVersionSummary.html PackageVersionSummary>
-- objects for package versions in a repository that match the request
-- parameters.
--
-- This operation returns paginated results.
module Amazonka.CodeArtifact.ListPackageVersions
  ( -- * Creating a Request
    ListPackageVersions (..),
    newListPackageVersions,

    -- * Request Lenses
    listPackageVersions_nextToken,
    listPackageVersions_originType,
    listPackageVersions_status,
    listPackageVersions_sortBy,
    listPackageVersions_maxResults,
    listPackageVersions_domainOwner,
    listPackageVersions_namespace,
    listPackageVersions_domain,
    listPackageVersions_repository,
    listPackageVersions_format,
    listPackageVersions_package,

    -- * Destructuring the Response
    ListPackageVersionsResponse (..),
    newListPackageVersionsResponse,

    -- * Response Lenses
    listPackageVersionsResponse_nextToken,
    listPackageVersionsResponse_defaultDisplayVersion,
    listPackageVersionsResponse_format,
    listPackageVersionsResponse_package,
    listPackageVersionsResponse_versions,
    listPackageVersionsResponse_namespace,
    listPackageVersionsResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPackageVersions' smart constructor.
data ListPackageVersions = ListPackageVersions'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The @originType@ used to filter package versions. Only package versions
    -- with the provided @originType@ will be returned.
    originType :: Prelude.Maybe PackageVersionOriginType,
    -- | A string that filters the requested package versions by status.
    status :: Prelude.Maybe PackageVersionStatus,
    -- | How to sort the requested list of package versions.
    sortBy :: Prelude.Maybe PackageVersionSortType,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the package that contains the requested package
    -- versions. The package component that specifies its namespace depends on
    -- its type. For example:
    --
    -- -   The namespace of a Maven package is its @groupId@.
    --
    -- -   The namespace of an npm package is its @scope@.
    --
    -- -   Python and NuGet packages do not contain a corresponding component,
    --     packages of those formats do not have a namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the repository that contains the
    -- requested package versions.
    domain :: Prelude.Text,
    -- | The name of the repository that contains the requested package versions.
    repository :: Prelude.Text,
    -- | The format of the returned package versions.
    format :: PackageFormat,
    -- | The name of the package for which you want to request package versions.
    package :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackageVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPackageVersions_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'originType', 'listPackageVersions_originType' - The @originType@ used to filter package versions. Only package versions
-- with the provided @originType@ will be returned.
--
-- 'status', 'listPackageVersions_status' - A string that filters the requested package versions by status.
--
-- 'sortBy', 'listPackageVersions_sortBy' - How to sort the requested list of package versions.
--
-- 'maxResults', 'listPackageVersions_maxResults' - The maximum number of results to return per page.
--
-- 'domainOwner', 'listPackageVersions_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'namespace', 'listPackageVersions_namespace' - The namespace of the package that contains the requested package
-- versions. The package component that specifies its namespace depends on
-- its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   Python and NuGet packages do not contain a corresponding component,
--     packages of those formats do not have a namespace.
--
-- 'domain', 'listPackageVersions_domain' - The name of the domain that contains the repository that contains the
-- requested package versions.
--
-- 'repository', 'listPackageVersions_repository' - The name of the repository that contains the requested package versions.
--
-- 'format', 'listPackageVersions_format' - The format of the returned package versions.
--
-- 'package', 'listPackageVersions_package' - The name of the package for which you want to request package versions.
newListPackageVersions ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  -- | 'format'
  PackageFormat ->
  -- | 'package'
  Prelude.Text ->
  ListPackageVersions
newListPackageVersions
  pDomain_
  pRepository_
  pFormat_
  pPackage_ =
    ListPackageVersions'
      { nextToken = Prelude.Nothing,
        originType = Prelude.Nothing,
        status = Prelude.Nothing,
        sortBy = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        domainOwner = Prelude.Nothing,
        namespace = Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        format = pFormat_,
        package = pPackage_
      }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listPackageVersions_nextToken :: Lens.Lens' ListPackageVersions (Prelude.Maybe Prelude.Text)
listPackageVersions_nextToken = Lens.lens (\ListPackageVersions' {nextToken} -> nextToken) (\s@ListPackageVersions' {} a -> s {nextToken = a} :: ListPackageVersions)

-- | The @originType@ used to filter package versions. Only package versions
-- with the provided @originType@ will be returned.
listPackageVersions_originType :: Lens.Lens' ListPackageVersions (Prelude.Maybe PackageVersionOriginType)
listPackageVersions_originType = Lens.lens (\ListPackageVersions' {originType} -> originType) (\s@ListPackageVersions' {} a -> s {originType = a} :: ListPackageVersions)

-- | A string that filters the requested package versions by status.
listPackageVersions_status :: Lens.Lens' ListPackageVersions (Prelude.Maybe PackageVersionStatus)
listPackageVersions_status = Lens.lens (\ListPackageVersions' {status} -> status) (\s@ListPackageVersions' {} a -> s {status = a} :: ListPackageVersions)

-- | How to sort the requested list of package versions.
listPackageVersions_sortBy :: Lens.Lens' ListPackageVersions (Prelude.Maybe PackageVersionSortType)
listPackageVersions_sortBy = Lens.lens (\ListPackageVersions' {sortBy} -> sortBy) (\s@ListPackageVersions' {} a -> s {sortBy = a} :: ListPackageVersions)

-- | The maximum number of results to return per page.
listPackageVersions_maxResults :: Lens.Lens' ListPackageVersions (Prelude.Maybe Prelude.Natural)
listPackageVersions_maxResults = Lens.lens (\ListPackageVersions' {maxResults} -> maxResults) (\s@ListPackageVersions' {} a -> s {maxResults = a} :: ListPackageVersions)

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
listPackageVersions_domainOwner :: Lens.Lens' ListPackageVersions (Prelude.Maybe Prelude.Text)
listPackageVersions_domainOwner = Lens.lens (\ListPackageVersions' {domainOwner} -> domainOwner) (\s@ListPackageVersions' {} a -> s {domainOwner = a} :: ListPackageVersions)

-- | The namespace of the package that contains the requested package
-- versions. The package component that specifies its namespace depends on
-- its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   Python and NuGet packages do not contain a corresponding component,
--     packages of those formats do not have a namespace.
listPackageVersions_namespace :: Lens.Lens' ListPackageVersions (Prelude.Maybe Prelude.Text)
listPackageVersions_namespace = Lens.lens (\ListPackageVersions' {namespace} -> namespace) (\s@ListPackageVersions' {} a -> s {namespace = a} :: ListPackageVersions)

-- | The name of the domain that contains the repository that contains the
-- requested package versions.
listPackageVersions_domain :: Lens.Lens' ListPackageVersions Prelude.Text
listPackageVersions_domain = Lens.lens (\ListPackageVersions' {domain} -> domain) (\s@ListPackageVersions' {} a -> s {domain = a} :: ListPackageVersions)

-- | The name of the repository that contains the requested package versions.
listPackageVersions_repository :: Lens.Lens' ListPackageVersions Prelude.Text
listPackageVersions_repository = Lens.lens (\ListPackageVersions' {repository} -> repository) (\s@ListPackageVersions' {} a -> s {repository = a} :: ListPackageVersions)

-- | The format of the returned package versions.
listPackageVersions_format :: Lens.Lens' ListPackageVersions PackageFormat
listPackageVersions_format = Lens.lens (\ListPackageVersions' {format} -> format) (\s@ListPackageVersions' {} a -> s {format = a} :: ListPackageVersions)

-- | The name of the package for which you want to request package versions.
listPackageVersions_package :: Lens.Lens' ListPackageVersions Prelude.Text
listPackageVersions_package = Lens.lens (\ListPackageVersions' {package} -> package) (\s@ListPackageVersions' {} a -> s {package = a} :: ListPackageVersions)

instance Core.AWSPager ListPackageVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPackageVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPackageVersionsResponse_versions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPackageVersions_nextToken
          Lens..~ rs
          Lens.^? listPackageVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListPackageVersions where
  type
    AWSResponse ListPackageVersions =
      ListPackageVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPackageVersionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "defaultDisplayVersion")
            Prelude.<*> (x Data..?> "format")
            Prelude.<*> (x Data..?> "package")
            Prelude.<*> (x Data..?> "versions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "namespace")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPackageVersions where
  hashWithSalt _salt ListPackageVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` originType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` package

instance Prelude.NFData ListPackageVersions where
  rnf ListPackageVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf originType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf package

instance Data.ToHeaders ListPackageVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPackageVersions where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ListPackageVersions where
  toPath = Prelude.const "/v1/package/versions"

instance Data.ToQuery ListPackageVersions where
  toQuery ListPackageVersions' {..} =
    Prelude.mconcat
      [ "next-token" Data.=: nextToken,
        "originType" Data.=: originType,
        "status" Data.=: status,
        "sortBy" Data.=: sortBy,
        "max-results" Data.=: maxResults,
        "domain-owner" Data.=: domainOwner,
        "namespace" Data.=: namespace,
        "domain" Data.=: domain,
        "repository" Data.=: repository,
        "format" Data.=: format,
        "package" Data.=: package
      ]

-- | /See:/ 'newListPackageVersionsResponse' smart constructor.
data ListPackageVersionsResponse = ListPackageVersionsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The default package version to display. This depends on the package
    -- format:
    --
    -- -   For Maven and PyPI packages, it\'s the most recently published
    --     package version.
    --
    -- -   For npm packages, it\'s the version referenced by the @latest@ tag.
    --     If the @latest@ tag is not set, it\'s the most recently published
    --     package version.
    defaultDisplayVersion :: Prelude.Maybe Prelude.Text,
    -- | A format of the package.
    format :: Prelude.Maybe PackageFormat,
    -- | The name of the package.
    package :: Prelude.Maybe Prelude.Text,
    -- | The returned list of
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageVersionSummary.html PackageVersionSummary>
    -- objects.
    versions :: Prelude.Maybe [PackageVersionSummary],
    -- | The namespace of the package that contains the requested package
    -- versions. The package component that specifies its namespace depends on
    -- its type. For example:
    --
    -- -   The namespace of a Maven package is its @groupId@.
    --
    -- -   The namespace of an npm package is its @scope@.
    --
    -- -   Python and NuGet packages do not contain a corresponding component,
    --     packages of those formats do not have a namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackageVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPackageVersionsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'defaultDisplayVersion', 'listPackageVersionsResponse_defaultDisplayVersion' - The default package version to display. This depends on the package
-- format:
--
-- -   For Maven and PyPI packages, it\'s the most recently published
--     package version.
--
-- -   For npm packages, it\'s the version referenced by the @latest@ tag.
--     If the @latest@ tag is not set, it\'s the most recently published
--     package version.
--
-- 'format', 'listPackageVersionsResponse_format' - A format of the package.
--
-- 'package', 'listPackageVersionsResponse_package' - The name of the package.
--
-- 'versions', 'listPackageVersionsResponse_versions' - The returned list of
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageVersionSummary.html PackageVersionSummary>
-- objects.
--
-- 'namespace', 'listPackageVersionsResponse_namespace' - The namespace of the package that contains the requested package
-- versions. The package component that specifies its namespace depends on
-- its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   Python and NuGet packages do not contain a corresponding component,
--     packages of those formats do not have a namespace.
--
-- 'httpStatus', 'listPackageVersionsResponse_httpStatus' - The response's http status code.
newListPackageVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPackageVersionsResponse
newListPackageVersionsResponse pHttpStatus_ =
  ListPackageVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      defaultDisplayVersion = Prelude.Nothing,
      format = Prelude.Nothing,
      package = Prelude.Nothing,
      versions = Prelude.Nothing,
      namespace = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listPackageVersionsResponse_nextToken :: Lens.Lens' ListPackageVersionsResponse (Prelude.Maybe Prelude.Text)
listPackageVersionsResponse_nextToken = Lens.lens (\ListPackageVersionsResponse' {nextToken} -> nextToken) (\s@ListPackageVersionsResponse' {} a -> s {nextToken = a} :: ListPackageVersionsResponse)

-- | The default package version to display. This depends on the package
-- format:
--
-- -   For Maven and PyPI packages, it\'s the most recently published
--     package version.
--
-- -   For npm packages, it\'s the version referenced by the @latest@ tag.
--     If the @latest@ tag is not set, it\'s the most recently published
--     package version.
listPackageVersionsResponse_defaultDisplayVersion :: Lens.Lens' ListPackageVersionsResponse (Prelude.Maybe Prelude.Text)
listPackageVersionsResponse_defaultDisplayVersion = Lens.lens (\ListPackageVersionsResponse' {defaultDisplayVersion} -> defaultDisplayVersion) (\s@ListPackageVersionsResponse' {} a -> s {defaultDisplayVersion = a} :: ListPackageVersionsResponse)

-- | A format of the package.
listPackageVersionsResponse_format :: Lens.Lens' ListPackageVersionsResponse (Prelude.Maybe PackageFormat)
listPackageVersionsResponse_format = Lens.lens (\ListPackageVersionsResponse' {format} -> format) (\s@ListPackageVersionsResponse' {} a -> s {format = a} :: ListPackageVersionsResponse)

-- | The name of the package.
listPackageVersionsResponse_package :: Lens.Lens' ListPackageVersionsResponse (Prelude.Maybe Prelude.Text)
listPackageVersionsResponse_package = Lens.lens (\ListPackageVersionsResponse' {package} -> package) (\s@ListPackageVersionsResponse' {} a -> s {package = a} :: ListPackageVersionsResponse)

-- | The returned list of
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageVersionSummary.html PackageVersionSummary>
-- objects.
listPackageVersionsResponse_versions :: Lens.Lens' ListPackageVersionsResponse (Prelude.Maybe [PackageVersionSummary])
listPackageVersionsResponse_versions = Lens.lens (\ListPackageVersionsResponse' {versions} -> versions) (\s@ListPackageVersionsResponse' {} a -> s {versions = a} :: ListPackageVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The namespace of the package that contains the requested package
-- versions. The package component that specifies its namespace depends on
-- its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   Python and NuGet packages do not contain a corresponding component,
--     packages of those formats do not have a namespace.
listPackageVersionsResponse_namespace :: Lens.Lens' ListPackageVersionsResponse (Prelude.Maybe Prelude.Text)
listPackageVersionsResponse_namespace = Lens.lens (\ListPackageVersionsResponse' {namespace} -> namespace) (\s@ListPackageVersionsResponse' {} a -> s {namespace = a} :: ListPackageVersionsResponse)

-- | The response's http status code.
listPackageVersionsResponse_httpStatus :: Lens.Lens' ListPackageVersionsResponse Prelude.Int
listPackageVersionsResponse_httpStatus = Lens.lens (\ListPackageVersionsResponse' {httpStatus} -> httpStatus) (\s@ListPackageVersionsResponse' {} a -> s {httpStatus = a} :: ListPackageVersionsResponse)

instance Prelude.NFData ListPackageVersionsResponse where
  rnf ListPackageVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf defaultDisplayVersion
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf package
      `Prelude.seq` Prelude.rnf versions
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf httpStatus
