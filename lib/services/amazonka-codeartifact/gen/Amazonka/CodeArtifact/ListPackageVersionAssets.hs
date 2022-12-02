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
-- Module      : Amazonka.CodeArtifact.ListPackageVersionAssets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_AssetSummary.html AssetSummary>
-- objects for assets in a package version.
--
-- This operation returns paginated results.
module Amazonka.CodeArtifact.ListPackageVersionAssets
  ( -- * Creating a Request
    ListPackageVersionAssets (..),
    newListPackageVersionAssets,

    -- * Request Lenses
    listPackageVersionAssets_nextToken,
    listPackageVersionAssets_maxResults,
    listPackageVersionAssets_domainOwner,
    listPackageVersionAssets_namespace,
    listPackageVersionAssets_domain,
    listPackageVersionAssets_repository,
    listPackageVersionAssets_format,
    listPackageVersionAssets_package,
    listPackageVersionAssets_packageVersion,

    -- * Destructuring the Response
    ListPackageVersionAssetsResponse (..),
    newListPackageVersionAssetsResponse,

    -- * Response Lenses
    listPackageVersionAssetsResponse_nextToken,
    listPackageVersionAssetsResponse_versionRevision,
    listPackageVersionAssetsResponse_format,
    listPackageVersionAssetsResponse_package,
    listPackageVersionAssetsResponse_assets,
    listPackageVersionAssetsResponse_namespace,
    listPackageVersionAssetsResponse_version,
    listPackageVersionAssetsResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPackageVersionAssets' smart constructor.
data ListPackageVersionAssets = ListPackageVersionAssets'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the package version that contains the requested package
    -- version assets. The package version component that specifies its
    -- namespace depends on its type. For example:
    --
    -- -   The namespace of a Maven package version is its @groupId@.
    --
    -- -   The namespace of an npm package version is its @scope@.
    --
    -- -   Python and NuGet package versions do not contain a corresponding
    --     component, package versions of those formats do not have a
    --     namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the repository associated with the
    -- package version assets.
    domain :: Prelude.Text,
    -- | The name of the repository that contains the package that contains the
    -- requested package version assets.
    repository :: Prelude.Text,
    -- | The format of the package that contains the requested package version
    -- assets.
    format :: PackageFormat,
    -- | The name of the package that contains the requested package version
    -- assets.
    package :: Prelude.Text,
    -- | A string that contains the package version (for example, @3.5.2@).
    packageVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackageVersionAssets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPackageVersionAssets_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listPackageVersionAssets_maxResults' - The maximum number of results to return per page.
--
-- 'domainOwner', 'listPackageVersionAssets_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'namespace', 'listPackageVersionAssets_namespace' - The namespace of the package version that contains the requested package
-- version assets. The package version component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package version is its @groupId@.
--
-- -   The namespace of an npm package version is its @scope@.
--
-- -   Python and NuGet package versions do not contain a corresponding
--     component, package versions of those formats do not have a
--     namespace.
--
-- 'domain', 'listPackageVersionAssets_domain' - The name of the domain that contains the repository associated with the
-- package version assets.
--
-- 'repository', 'listPackageVersionAssets_repository' - The name of the repository that contains the package that contains the
-- requested package version assets.
--
-- 'format', 'listPackageVersionAssets_format' - The format of the package that contains the requested package version
-- assets.
--
-- 'package', 'listPackageVersionAssets_package' - The name of the package that contains the requested package version
-- assets.
--
-- 'packageVersion', 'listPackageVersionAssets_packageVersion' - A string that contains the package version (for example, @3.5.2@).
newListPackageVersionAssets ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  -- | 'format'
  PackageFormat ->
  -- | 'package'
  Prelude.Text ->
  -- | 'packageVersion'
  Prelude.Text ->
  ListPackageVersionAssets
newListPackageVersionAssets
  pDomain_
  pRepository_
  pFormat_
  pPackage_
  pPackageVersion_ =
    ListPackageVersionAssets'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        domainOwner = Prelude.Nothing,
        namespace = Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        format = pFormat_,
        package = pPackage_,
        packageVersion = pPackageVersion_
      }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listPackageVersionAssets_nextToken :: Lens.Lens' ListPackageVersionAssets (Prelude.Maybe Prelude.Text)
listPackageVersionAssets_nextToken = Lens.lens (\ListPackageVersionAssets' {nextToken} -> nextToken) (\s@ListPackageVersionAssets' {} a -> s {nextToken = a} :: ListPackageVersionAssets)

-- | The maximum number of results to return per page.
listPackageVersionAssets_maxResults :: Lens.Lens' ListPackageVersionAssets (Prelude.Maybe Prelude.Natural)
listPackageVersionAssets_maxResults = Lens.lens (\ListPackageVersionAssets' {maxResults} -> maxResults) (\s@ListPackageVersionAssets' {} a -> s {maxResults = a} :: ListPackageVersionAssets)

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
listPackageVersionAssets_domainOwner :: Lens.Lens' ListPackageVersionAssets (Prelude.Maybe Prelude.Text)
listPackageVersionAssets_domainOwner = Lens.lens (\ListPackageVersionAssets' {domainOwner} -> domainOwner) (\s@ListPackageVersionAssets' {} a -> s {domainOwner = a} :: ListPackageVersionAssets)

-- | The namespace of the package version that contains the requested package
-- version assets. The package version component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package version is its @groupId@.
--
-- -   The namespace of an npm package version is its @scope@.
--
-- -   Python and NuGet package versions do not contain a corresponding
--     component, package versions of those formats do not have a
--     namespace.
listPackageVersionAssets_namespace :: Lens.Lens' ListPackageVersionAssets (Prelude.Maybe Prelude.Text)
listPackageVersionAssets_namespace = Lens.lens (\ListPackageVersionAssets' {namespace} -> namespace) (\s@ListPackageVersionAssets' {} a -> s {namespace = a} :: ListPackageVersionAssets)

-- | The name of the domain that contains the repository associated with the
-- package version assets.
listPackageVersionAssets_domain :: Lens.Lens' ListPackageVersionAssets Prelude.Text
listPackageVersionAssets_domain = Lens.lens (\ListPackageVersionAssets' {domain} -> domain) (\s@ListPackageVersionAssets' {} a -> s {domain = a} :: ListPackageVersionAssets)

-- | The name of the repository that contains the package that contains the
-- requested package version assets.
listPackageVersionAssets_repository :: Lens.Lens' ListPackageVersionAssets Prelude.Text
listPackageVersionAssets_repository = Lens.lens (\ListPackageVersionAssets' {repository} -> repository) (\s@ListPackageVersionAssets' {} a -> s {repository = a} :: ListPackageVersionAssets)

-- | The format of the package that contains the requested package version
-- assets.
listPackageVersionAssets_format :: Lens.Lens' ListPackageVersionAssets PackageFormat
listPackageVersionAssets_format = Lens.lens (\ListPackageVersionAssets' {format} -> format) (\s@ListPackageVersionAssets' {} a -> s {format = a} :: ListPackageVersionAssets)

-- | The name of the package that contains the requested package version
-- assets.
listPackageVersionAssets_package :: Lens.Lens' ListPackageVersionAssets Prelude.Text
listPackageVersionAssets_package = Lens.lens (\ListPackageVersionAssets' {package} -> package) (\s@ListPackageVersionAssets' {} a -> s {package = a} :: ListPackageVersionAssets)

-- | A string that contains the package version (for example, @3.5.2@).
listPackageVersionAssets_packageVersion :: Lens.Lens' ListPackageVersionAssets Prelude.Text
listPackageVersionAssets_packageVersion = Lens.lens (\ListPackageVersionAssets' {packageVersion} -> packageVersion) (\s@ListPackageVersionAssets' {} a -> s {packageVersion = a} :: ListPackageVersionAssets)

instance Core.AWSPager ListPackageVersionAssets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPackageVersionAssetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPackageVersionAssetsResponse_assets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPackageVersionAssets_nextToken
          Lens..~ rs
          Lens.^? listPackageVersionAssetsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListPackageVersionAssets where
  type
    AWSResponse ListPackageVersionAssets =
      ListPackageVersionAssetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPackageVersionAssetsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "versionRevision")
            Prelude.<*> (x Data..?> "format")
            Prelude.<*> (x Data..?> "package")
            Prelude.<*> (x Data..?> "assets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "namespace")
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPackageVersionAssets where
  hashWithSalt _salt ListPackageVersionAssets' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` package
      `Prelude.hashWithSalt` packageVersion

instance Prelude.NFData ListPackageVersionAssets where
  rnf ListPackageVersionAssets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf package
      `Prelude.seq` Prelude.rnf packageVersion

instance Data.ToHeaders ListPackageVersionAssets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPackageVersionAssets where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ListPackageVersionAssets where
  toPath = Prelude.const "/v1/package/version/assets"

instance Data.ToQuery ListPackageVersionAssets where
  toQuery ListPackageVersionAssets' {..} =
    Prelude.mconcat
      [ "next-token" Data.=: nextToken,
        "max-results" Data.=: maxResults,
        "domain-owner" Data.=: domainOwner,
        "namespace" Data.=: namespace,
        "domain" Data.=: domain,
        "repository" Data.=: repository,
        "format" Data.=: format,
        "package" Data.=: package,
        "version" Data.=: packageVersion
      ]

-- | /See:/ 'newListPackageVersionAssetsResponse' smart constructor.
data ListPackageVersionAssetsResponse = ListPackageVersionAssetsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The current revision associated with the package version.
    versionRevision :: Prelude.Maybe Prelude.Text,
    -- | The format of the package that contains the requested package version
    -- assets.
    format :: Prelude.Maybe PackageFormat,
    -- | The name of the package that contains the requested package version
    -- assets.
    package :: Prelude.Maybe Prelude.Text,
    -- | The returned list of
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_AssetSummary.html AssetSummary>
    -- objects.
    assets :: Prelude.Maybe [AssetSummary],
    -- | The namespace of the package version that contains the requested package
    -- version assets. The package version component that specifies its
    -- namespace depends on its type. For example:
    --
    -- -   The namespace of a Maven package version is its @groupId@.
    --
    -- -   The namespace of an npm package version is its @scope@.
    --
    -- -   Python and NuGet package versions do not contain a corresponding
    --     component, package versions of those formats do not have a
    --     namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The version of the package associated with the requested assets.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackageVersionAssetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPackageVersionAssetsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'versionRevision', 'listPackageVersionAssetsResponse_versionRevision' - The current revision associated with the package version.
--
-- 'format', 'listPackageVersionAssetsResponse_format' - The format of the package that contains the requested package version
-- assets.
--
-- 'package', 'listPackageVersionAssetsResponse_package' - The name of the package that contains the requested package version
-- assets.
--
-- 'assets', 'listPackageVersionAssetsResponse_assets' - The returned list of
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_AssetSummary.html AssetSummary>
-- objects.
--
-- 'namespace', 'listPackageVersionAssetsResponse_namespace' - The namespace of the package version that contains the requested package
-- version assets. The package version component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package version is its @groupId@.
--
-- -   The namespace of an npm package version is its @scope@.
--
-- -   Python and NuGet package versions do not contain a corresponding
--     component, package versions of those formats do not have a
--     namespace.
--
-- 'version', 'listPackageVersionAssetsResponse_version' - The version of the package associated with the requested assets.
--
-- 'httpStatus', 'listPackageVersionAssetsResponse_httpStatus' - The response's http status code.
newListPackageVersionAssetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPackageVersionAssetsResponse
newListPackageVersionAssetsResponse pHttpStatus_ =
  ListPackageVersionAssetsResponse'
    { nextToken =
        Prelude.Nothing,
      versionRevision = Prelude.Nothing,
      format = Prelude.Nothing,
      package = Prelude.Nothing,
      assets = Prelude.Nothing,
      namespace = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listPackageVersionAssetsResponse_nextToken :: Lens.Lens' ListPackageVersionAssetsResponse (Prelude.Maybe Prelude.Text)
listPackageVersionAssetsResponse_nextToken = Lens.lens (\ListPackageVersionAssetsResponse' {nextToken} -> nextToken) (\s@ListPackageVersionAssetsResponse' {} a -> s {nextToken = a} :: ListPackageVersionAssetsResponse)

-- | The current revision associated with the package version.
listPackageVersionAssetsResponse_versionRevision :: Lens.Lens' ListPackageVersionAssetsResponse (Prelude.Maybe Prelude.Text)
listPackageVersionAssetsResponse_versionRevision = Lens.lens (\ListPackageVersionAssetsResponse' {versionRevision} -> versionRevision) (\s@ListPackageVersionAssetsResponse' {} a -> s {versionRevision = a} :: ListPackageVersionAssetsResponse)

-- | The format of the package that contains the requested package version
-- assets.
listPackageVersionAssetsResponse_format :: Lens.Lens' ListPackageVersionAssetsResponse (Prelude.Maybe PackageFormat)
listPackageVersionAssetsResponse_format = Lens.lens (\ListPackageVersionAssetsResponse' {format} -> format) (\s@ListPackageVersionAssetsResponse' {} a -> s {format = a} :: ListPackageVersionAssetsResponse)

-- | The name of the package that contains the requested package version
-- assets.
listPackageVersionAssetsResponse_package :: Lens.Lens' ListPackageVersionAssetsResponse (Prelude.Maybe Prelude.Text)
listPackageVersionAssetsResponse_package = Lens.lens (\ListPackageVersionAssetsResponse' {package} -> package) (\s@ListPackageVersionAssetsResponse' {} a -> s {package = a} :: ListPackageVersionAssetsResponse)

-- | The returned list of
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_AssetSummary.html AssetSummary>
-- objects.
listPackageVersionAssetsResponse_assets :: Lens.Lens' ListPackageVersionAssetsResponse (Prelude.Maybe [AssetSummary])
listPackageVersionAssetsResponse_assets = Lens.lens (\ListPackageVersionAssetsResponse' {assets} -> assets) (\s@ListPackageVersionAssetsResponse' {} a -> s {assets = a} :: ListPackageVersionAssetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The namespace of the package version that contains the requested package
-- version assets. The package version component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package version is its @groupId@.
--
-- -   The namespace of an npm package version is its @scope@.
--
-- -   Python and NuGet package versions do not contain a corresponding
--     component, package versions of those formats do not have a
--     namespace.
listPackageVersionAssetsResponse_namespace :: Lens.Lens' ListPackageVersionAssetsResponse (Prelude.Maybe Prelude.Text)
listPackageVersionAssetsResponse_namespace = Lens.lens (\ListPackageVersionAssetsResponse' {namespace} -> namespace) (\s@ListPackageVersionAssetsResponse' {} a -> s {namespace = a} :: ListPackageVersionAssetsResponse)

-- | The version of the package associated with the requested assets.
listPackageVersionAssetsResponse_version :: Lens.Lens' ListPackageVersionAssetsResponse (Prelude.Maybe Prelude.Text)
listPackageVersionAssetsResponse_version = Lens.lens (\ListPackageVersionAssetsResponse' {version} -> version) (\s@ListPackageVersionAssetsResponse' {} a -> s {version = a} :: ListPackageVersionAssetsResponse)

-- | The response's http status code.
listPackageVersionAssetsResponse_httpStatus :: Lens.Lens' ListPackageVersionAssetsResponse Prelude.Int
listPackageVersionAssetsResponse_httpStatus = Lens.lens (\ListPackageVersionAssetsResponse' {httpStatus} -> httpStatus) (\s@ListPackageVersionAssetsResponse' {} a -> s {httpStatus = a} :: ListPackageVersionAssetsResponse)

instance
  Prelude.NFData
    ListPackageVersionAssetsResponse
  where
  rnf ListPackageVersionAssetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf versionRevision
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf package
      `Prelude.seq` Prelude.rnf assets
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
