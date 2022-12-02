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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    listPackages_nextToken,
    listPackages_publish,
    listPackages_upstream,
    listPackages_format,
    listPackages_maxResults,
    listPackages_domainOwner,
    listPackages_namespace,
    listPackages_packagePrefix,
    listPackages_domain,
    listPackages_repository,

    -- * Destructuring the Response
    ListPackagesResponse (..),
    newListPackagesResponse,

    -- * Response Lenses
    listPackagesResponse_nextToken,
    listPackagesResponse_packages,
    listPackagesResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPackages' smart constructor.
data ListPackages = ListPackages'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The value of the @Publish@ package origin control restriction used to
    -- filter requested packages. Only packages with the provided restriction
    -- are returned. For more information, see
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginRestrictions.html PackageOriginRestrictions>.
    publish :: Prelude.Maybe AllowPublish,
    -- | The value of the @Upstream@ package origin control restriction used to
    -- filter requested packages. Only packages with the provided restriction
    -- are returned. For more information, see
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginRestrictions.html PackageOriginRestrictions>.
    upstream :: Prelude.Maybe AllowUpstream,
    -- | The format used to filter requested packages. Only packages from the
    -- provided format will be returned.
    format :: Prelude.Maybe PackageFormat,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The namespace used to filter requested packages. Only packages with the
    -- provided namespace will be returned. The package component that
    -- specifies its namespace depends on its type. For example:
    --
    -- -   The namespace of a Maven package is its @groupId@.
    --
    -- -   The namespace of an npm package is its @scope@.
    --
    -- -   Python and NuGet packages do not contain a corresponding component,
    --     packages of those formats do not have a namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | A prefix used to filter requested packages. Only packages with names
    -- that start with @packagePrefix@ are returned.
    packagePrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the repository that contains the
    -- requested packages.
    domain :: Prelude.Text,
    -- | The name of the repository that contains the requested packages.
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
-- 'nextToken', 'listPackages_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'publish', 'listPackages_publish' - The value of the @Publish@ package origin control restriction used to
-- filter requested packages. Only packages with the provided restriction
-- are returned. For more information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginRestrictions.html PackageOriginRestrictions>.
--
-- 'upstream', 'listPackages_upstream' - The value of the @Upstream@ package origin control restriction used to
-- filter requested packages. Only packages with the provided restriction
-- are returned. For more information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginRestrictions.html PackageOriginRestrictions>.
--
-- 'format', 'listPackages_format' - The format used to filter requested packages. Only packages from the
-- provided format will be returned.
--
-- 'maxResults', 'listPackages_maxResults' - The maximum number of results to return per page.
--
-- 'domainOwner', 'listPackages_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'namespace', 'listPackages_namespace' - The namespace used to filter requested packages. Only packages with the
-- provided namespace will be returned. The package component that
-- specifies its namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   Python and NuGet packages do not contain a corresponding component,
--     packages of those formats do not have a namespace.
--
-- 'packagePrefix', 'listPackages_packagePrefix' - A prefix used to filter requested packages. Only packages with names
-- that start with @packagePrefix@ are returned.
--
-- 'domain', 'listPackages_domain' - The name of the domain that contains the repository that contains the
-- requested packages.
--
-- 'repository', 'listPackages_repository' - The name of the repository that contains the requested packages.
newListPackages ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  ListPackages
newListPackages pDomain_ pRepository_ =
  ListPackages'
    { nextToken = Prelude.Nothing,
      publish = Prelude.Nothing,
      upstream = Prelude.Nothing,
      format = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      domainOwner = Prelude.Nothing,
      namespace = Prelude.Nothing,
      packagePrefix = Prelude.Nothing,
      domain = pDomain_,
      repository = pRepository_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listPackages_nextToken :: Lens.Lens' ListPackages (Prelude.Maybe Prelude.Text)
listPackages_nextToken = Lens.lens (\ListPackages' {nextToken} -> nextToken) (\s@ListPackages' {} a -> s {nextToken = a} :: ListPackages)

-- | The value of the @Publish@ package origin control restriction used to
-- filter requested packages. Only packages with the provided restriction
-- are returned. For more information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginRestrictions.html PackageOriginRestrictions>.
listPackages_publish :: Lens.Lens' ListPackages (Prelude.Maybe AllowPublish)
listPackages_publish = Lens.lens (\ListPackages' {publish} -> publish) (\s@ListPackages' {} a -> s {publish = a} :: ListPackages)

-- | The value of the @Upstream@ package origin control restriction used to
-- filter requested packages. Only packages with the provided restriction
-- are returned. For more information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginRestrictions.html PackageOriginRestrictions>.
listPackages_upstream :: Lens.Lens' ListPackages (Prelude.Maybe AllowUpstream)
listPackages_upstream = Lens.lens (\ListPackages' {upstream} -> upstream) (\s@ListPackages' {} a -> s {upstream = a} :: ListPackages)

-- | The format used to filter requested packages. Only packages from the
-- provided format will be returned.
listPackages_format :: Lens.Lens' ListPackages (Prelude.Maybe PackageFormat)
listPackages_format = Lens.lens (\ListPackages' {format} -> format) (\s@ListPackages' {} a -> s {format = a} :: ListPackages)

-- | The maximum number of results to return per page.
listPackages_maxResults :: Lens.Lens' ListPackages (Prelude.Maybe Prelude.Natural)
listPackages_maxResults = Lens.lens (\ListPackages' {maxResults} -> maxResults) (\s@ListPackages' {} a -> s {maxResults = a} :: ListPackages)

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
listPackages_domainOwner :: Lens.Lens' ListPackages (Prelude.Maybe Prelude.Text)
listPackages_domainOwner = Lens.lens (\ListPackages' {domainOwner} -> domainOwner) (\s@ListPackages' {} a -> s {domainOwner = a} :: ListPackages)

-- | The namespace used to filter requested packages. Only packages with the
-- provided namespace will be returned. The package component that
-- specifies its namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   Python and NuGet packages do not contain a corresponding component,
--     packages of those formats do not have a namespace.
listPackages_namespace :: Lens.Lens' ListPackages (Prelude.Maybe Prelude.Text)
listPackages_namespace = Lens.lens (\ListPackages' {namespace} -> namespace) (\s@ListPackages' {} a -> s {namespace = a} :: ListPackages)

-- | A prefix used to filter requested packages. Only packages with names
-- that start with @packagePrefix@ are returned.
listPackages_packagePrefix :: Lens.Lens' ListPackages (Prelude.Maybe Prelude.Text)
listPackages_packagePrefix = Lens.lens (\ListPackages' {packagePrefix} -> packagePrefix) (\s@ListPackages' {} a -> s {packagePrefix = a} :: ListPackages)

-- | The name of the domain that contains the repository that contains the
-- requested packages.
listPackages_domain :: Lens.Lens' ListPackages Prelude.Text
listPackages_domain = Lens.lens (\ListPackages' {domain} -> domain) (\s@ListPackages' {} a -> s {domain = a} :: ListPackages)

-- | The name of the repository that contains the requested packages.
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPackagesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "packages" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPackages where
  hashWithSalt _salt ListPackages' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` publish
      `Prelude.hashWithSalt` upstream
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` packagePrefix
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` repository

instance Prelude.NFData ListPackages where
  rnf ListPackages' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf publish
      `Prelude.seq` Prelude.rnf upstream
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf packagePrefix
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository

instance Data.ToHeaders ListPackages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPackages where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ListPackages where
  toPath = Prelude.const "/v1/packages"

instance Data.ToQuery ListPackages where
  toQuery ListPackages' {..} =
    Prelude.mconcat
      [ "next-token" Data.=: nextToken,
        "publish" Data.=: publish,
        "upstream" Data.=: upstream,
        "format" Data.=: format,
        "max-results" Data.=: maxResults,
        "domain-owner" Data.=: domainOwner,
        "namespace" Data.=: namespace,
        "package-prefix" Data.=: packagePrefix,
        "domain" Data.=: domain,
        "repository" Data.=: repository
      ]

-- | /See:/ 'newListPackagesResponse' smart constructor.
data ListPackagesResponse = ListPackagesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of returned
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageSummary.html PackageSummary>
    -- objects.
    packages :: Prelude.Maybe [PackageSummary],
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
-- 'nextToken', 'listPackagesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'packages', 'listPackagesResponse_packages' - The list of returned
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageSummary.html PackageSummary>
-- objects.
--
-- 'httpStatus', 'listPackagesResponse_httpStatus' - The response's http status code.
newListPackagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPackagesResponse
newListPackagesResponse pHttpStatus_ =
  ListPackagesResponse'
    { nextToken = Prelude.Nothing,
      packages = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listPackagesResponse_nextToken :: Lens.Lens' ListPackagesResponse (Prelude.Maybe Prelude.Text)
listPackagesResponse_nextToken = Lens.lens (\ListPackagesResponse' {nextToken} -> nextToken) (\s@ListPackagesResponse' {} a -> s {nextToken = a} :: ListPackagesResponse)

-- | The list of returned
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageSummary.html PackageSummary>
-- objects.
listPackagesResponse_packages :: Lens.Lens' ListPackagesResponse (Prelude.Maybe [PackageSummary])
listPackagesResponse_packages = Lens.lens (\ListPackagesResponse' {packages} -> packages) (\s@ListPackagesResponse' {} a -> s {packages = a} :: ListPackagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPackagesResponse_httpStatus :: Lens.Lens' ListPackagesResponse Prelude.Int
listPackagesResponse_httpStatus = Lens.lens (\ListPackagesResponse' {httpStatus} -> httpStatus) (\s@ListPackagesResponse' {} a -> s {httpStatus = a} :: ListPackagesResponse)

instance Prelude.NFData ListPackagesResponse where
  rnf ListPackagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf packages
      `Prelude.seq` Prelude.rnf httpStatus
