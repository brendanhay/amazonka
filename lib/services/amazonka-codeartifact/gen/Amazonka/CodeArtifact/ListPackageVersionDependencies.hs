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
-- Module      : Amazonka.CodeArtifact.ListPackageVersionDependencies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the direct dependencies for a package version. The dependencies
-- are returned as
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageDependency.html PackageDependency>
-- objects. CodeArtifact extracts the dependencies for a package version
-- from the metadata file for the package format (for example, the
-- @package.json@ file for npm packages and the @pom.xml@ file for Maven).
-- Any package version dependencies that are not listed in the
-- configuration file are not returned.
module Amazonka.CodeArtifact.ListPackageVersionDependencies
  ( -- * Creating a Request
    ListPackageVersionDependencies (..),
    newListPackageVersionDependencies,

    -- * Request Lenses
    listPackageVersionDependencies_namespace,
    listPackageVersionDependencies_domainOwner,
    listPackageVersionDependencies_nextToken,
    listPackageVersionDependencies_domain,
    listPackageVersionDependencies_repository,
    listPackageVersionDependencies_format,
    listPackageVersionDependencies_package,
    listPackageVersionDependencies_packageVersion,

    -- * Destructuring the Response
    ListPackageVersionDependenciesResponse (..),
    newListPackageVersionDependenciesResponse,

    -- * Response Lenses
    listPackageVersionDependenciesResponse_format,
    listPackageVersionDependenciesResponse_namespace,
    listPackageVersionDependenciesResponse_versionRevision,
    listPackageVersionDependenciesResponse_dependencies,
    listPackageVersionDependenciesResponse_nextToken,
    listPackageVersionDependenciesResponse_version,
    listPackageVersionDependenciesResponse_package,
    listPackageVersionDependenciesResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPackageVersionDependencies' smart constructor.
data ListPackageVersionDependencies = ListPackageVersionDependencies'
  { -- | The namespace of the package. The package component that specifies its
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
    -- | The name of the domain that contains the repository that contains the
    -- requested package version dependencies.
    domain :: Prelude.Text,
    -- | The name of the repository that contains the requested package version.
    repository :: Prelude.Text,
    -- | The format of the package with the requested dependencies. The valid
    -- package types are:
    --
    -- -   @npm@: A Node Package Manager (npm) package.
    --
    -- -   @pypi@: A Python Package Index (PyPI) package.
    --
    -- -   @maven@: A Maven package that contains compiled code in a
    --     distributable format, such as a JAR file.
    format :: PackageFormat,
    -- | The name of the package versions\' package.
    package :: Prelude.Text,
    -- | A string that contains the package version (for example, @3.5.2@).
    packageVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackageVersionDependencies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespace', 'listPackageVersionDependencies_namespace' - The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   A Python package does not contain a corresponding component, so
--     Python packages do not have a namespace.
--
-- 'domainOwner', 'listPackageVersionDependencies_domainOwner' - The 12-digit account number of the AWS account that owns the domain. It
-- does not include dashes or spaces.
--
-- 'nextToken', 'listPackageVersionDependencies_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'domain', 'listPackageVersionDependencies_domain' - The name of the domain that contains the repository that contains the
-- requested package version dependencies.
--
-- 'repository', 'listPackageVersionDependencies_repository' - The name of the repository that contains the requested package version.
--
-- 'format', 'listPackageVersionDependencies_format' - The format of the package with the requested dependencies. The valid
-- package types are:
--
-- -   @npm@: A Node Package Manager (npm) package.
--
-- -   @pypi@: A Python Package Index (PyPI) package.
--
-- -   @maven@: A Maven package that contains compiled code in a
--     distributable format, such as a JAR file.
--
-- 'package', 'listPackageVersionDependencies_package' - The name of the package versions\' package.
--
-- 'packageVersion', 'listPackageVersionDependencies_packageVersion' - A string that contains the package version (for example, @3.5.2@).
newListPackageVersionDependencies ::
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
  ListPackageVersionDependencies
newListPackageVersionDependencies
  pDomain_
  pRepository_
  pFormat_
  pPackage_
  pPackageVersion_ =
    ListPackageVersionDependencies'
      { namespace =
          Prelude.Nothing,
        domainOwner = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        format = pFormat_,
        package = pPackage_,
        packageVersion = pPackageVersion_
      }

-- | The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   A Python package does not contain a corresponding component, so
--     Python packages do not have a namespace.
listPackageVersionDependencies_namespace :: Lens.Lens' ListPackageVersionDependencies (Prelude.Maybe Prelude.Text)
listPackageVersionDependencies_namespace = Lens.lens (\ListPackageVersionDependencies' {namespace} -> namespace) (\s@ListPackageVersionDependencies' {} a -> s {namespace = a} :: ListPackageVersionDependencies)

-- | The 12-digit account number of the AWS account that owns the domain. It
-- does not include dashes or spaces.
listPackageVersionDependencies_domainOwner :: Lens.Lens' ListPackageVersionDependencies (Prelude.Maybe Prelude.Text)
listPackageVersionDependencies_domainOwner = Lens.lens (\ListPackageVersionDependencies' {domainOwner} -> domainOwner) (\s@ListPackageVersionDependencies' {} a -> s {domainOwner = a} :: ListPackageVersionDependencies)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listPackageVersionDependencies_nextToken :: Lens.Lens' ListPackageVersionDependencies (Prelude.Maybe Prelude.Text)
listPackageVersionDependencies_nextToken = Lens.lens (\ListPackageVersionDependencies' {nextToken} -> nextToken) (\s@ListPackageVersionDependencies' {} a -> s {nextToken = a} :: ListPackageVersionDependencies)

-- | The name of the domain that contains the repository that contains the
-- requested package version dependencies.
listPackageVersionDependencies_domain :: Lens.Lens' ListPackageVersionDependencies Prelude.Text
listPackageVersionDependencies_domain = Lens.lens (\ListPackageVersionDependencies' {domain} -> domain) (\s@ListPackageVersionDependencies' {} a -> s {domain = a} :: ListPackageVersionDependencies)

-- | The name of the repository that contains the requested package version.
listPackageVersionDependencies_repository :: Lens.Lens' ListPackageVersionDependencies Prelude.Text
listPackageVersionDependencies_repository = Lens.lens (\ListPackageVersionDependencies' {repository} -> repository) (\s@ListPackageVersionDependencies' {} a -> s {repository = a} :: ListPackageVersionDependencies)

-- | The format of the package with the requested dependencies. The valid
-- package types are:
--
-- -   @npm@: A Node Package Manager (npm) package.
--
-- -   @pypi@: A Python Package Index (PyPI) package.
--
-- -   @maven@: A Maven package that contains compiled code in a
--     distributable format, such as a JAR file.
listPackageVersionDependencies_format :: Lens.Lens' ListPackageVersionDependencies PackageFormat
listPackageVersionDependencies_format = Lens.lens (\ListPackageVersionDependencies' {format} -> format) (\s@ListPackageVersionDependencies' {} a -> s {format = a} :: ListPackageVersionDependencies)

-- | The name of the package versions\' package.
listPackageVersionDependencies_package :: Lens.Lens' ListPackageVersionDependencies Prelude.Text
listPackageVersionDependencies_package = Lens.lens (\ListPackageVersionDependencies' {package} -> package) (\s@ListPackageVersionDependencies' {} a -> s {package = a} :: ListPackageVersionDependencies)

-- | A string that contains the package version (for example, @3.5.2@).
listPackageVersionDependencies_packageVersion :: Lens.Lens' ListPackageVersionDependencies Prelude.Text
listPackageVersionDependencies_packageVersion = Lens.lens (\ListPackageVersionDependencies' {packageVersion} -> packageVersion) (\s@ListPackageVersionDependencies' {} a -> s {packageVersion = a} :: ListPackageVersionDependencies)

instance
  Core.AWSRequest
    ListPackageVersionDependencies
  where
  type
    AWSResponse ListPackageVersionDependencies =
      ListPackageVersionDependenciesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPackageVersionDependenciesResponse'
            Prelude.<$> (x Core..?> "format")
            Prelude.<*> (x Core..?> "namespace")
            Prelude.<*> (x Core..?> "versionRevision")
            Prelude.<*> (x Core..?> "dependencies" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "version")
            Prelude.<*> (x Core..?> "package")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListPackageVersionDependencies
  where
  hashWithSalt
    _salt
    ListPackageVersionDependencies' {..} =
      _salt `Prelude.hashWithSalt` namespace
        `Prelude.hashWithSalt` domainOwner
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` domain
        `Prelude.hashWithSalt` repository
        `Prelude.hashWithSalt` format
        `Prelude.hashWithSalt` package
        `Prelude.hashWithSalt` packageVersion

instance
  Prelude.NFData
    ListPackageVersionDependencies
  where
  rnf ListPackageVersionDependencies' {..} =
    Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf package
      `Prelude.seq` Prelude.rnf packageVersion

instance
  Core.ToHeaders
    ListPackageVersionDependencies
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPackageVersionDependencies where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath ListPackageVersionDependencies where
  toPath =
    Prelude.const "/v1/package/version/dependencies"

instance Core.ToQuery ListPackageVersionDependencies where
  toQuery ListPackageVersionDependencies' {..} =
    Prelude.mconcat
      [ "namespace" Core.=: namespace,
        "domain-owner" Core.=: domainOwner,
        "next-token" Core.=: nextToken,
        "domain" Core.=: domain,
        "repository" Core.=: repository,
        "format" Core.=: format,
        "package" Core.=: package,
        "version" Core.=: packageVersion
      ]

-- | /See:/ 'newListPackageVersionDependenciesResponse' smart constructor.
data ListPackageVersionDependenciesResponse = ListPackageVersionDependenciesResponse'
  { -- | A format that specifies the type of the package that contains the
    -- returned dependencies. The valid values are:
    --
    -- -   @npm@
    --
    -- -   @pypi@
    --
    -- -   @maven@
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
    -- | The current revision associated with the package version.
    versionRevision :: Prelude.Maybe Prelude.Text,
    -- | The returned list of
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageDependency.html PackageDependency>
    -- objects.
    dependencies :: Prelude.Maybe [PackageDependency],
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The version of the package that is specified in the request.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the package that contains the returned package versions
    -- dependencies.
    package :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPackageVersionDependenciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'listPackageVersionDependenciesResponse_format' - A format that specifies the type of the package that contains the
-- returned dependencies. The valid values are:
--
-- -   @npm@
--
-- -   @pypi@
--
-- -   @maven@
--
-- 'namespace', 'listPackageVersionDependenciesResponse_namespace' - The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   A Python package does not contain a corresponding component, so
--     Python packages do not have a namespace.
--
-- 'versionRevision', 'listPackageVersionDependenciesResponse_versionRevision' - The current revision associated with the package version.
--
-- 'dependencies', 'listPackageVersionDependenciesResponse_dependencies' - The returned list of
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageDependency.html PackageDependency>
-- objects.
--
-- 'nextToken', 'listPackageVersionDependenciesResponse_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'version', 'listPackageVersionDependenciesResponse_version' - The version of the package that is specified in the request.
--
-- 'package', 'listPackageVersionDependenciesResponse_package' - The name of the package that contains the returned package versions
-- dependencies.
--
-- 'httpStatus', 'listPackageVersionDependenciesResponse_httpStatus' - The response's http status code.
newListPackageVersionDependenciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPackageVersionDependenciesResponse
newListPackageVersionDependenciesResponse
  pHttpStatus_ =
    ListPackageVersionDependenciesResponse'
      { format =
          Prelude.Nothing,
        namespace = Prelude.Nothing,
        versionRevision = Prelude.Nothing,
        dependencies = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        version = Prelude.Nothing,
        package = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A format that specifies the type of the package that contains the
-- returned dependencies. The valid values are:
--
-- -   @npm@
--
-- -   @pypi@
--
-- -   @maven@
listPackageVersionDependenciesResponse_format :: Lens.Lens' ListPackageVersionDependenciesResponse (Prelude.Maybe PackageFormat)
listPackageVersionDependenciesResponse_format = Lens.lens (\ListPackageVersionDependenciesResponse' {format} -> format) (\s@ListPackageVersionDependenciesResponse' {} a -> s {format = a} :: ListPackageVersionDependenciesResponse)

-- | The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   A Python package does not contain a corresponding component, so
--     Python packages do not have a namespace.
listPackageVersionDependenciesResponse_namespace :: Lens.Lens' ListPackageVersionDependenciesResponse (Prelude.Maybe Prelude.Text)
listPackageVersionDependenciesResponse_namespace = Lens.lens (\ListPackageVersionDependenciesResponse' {namespace} -> namespace) (\s@ListPackageVersionDependenciesResponse' {} a -> s {namespace = a} :: ListPackageVersionDependenciesResponse)

-- | The current revision associated with the package version.
listPackageVersionDependenciesResponse_versionRevision :: Lens.Lens' ListPackageVersionDependenciesResponse (Prelude.Maybe Prelude.Text)
listPackageVersionDependenciesResponse_versionRevision = Lens.lens (\ListPackageVersionDependenciesResponse' {versionRevision} -> versionRevision) (\s@ListPackageVersionDependenciesResponse' {} a -> s {versionRevision = a} :: ListPackageVersionDependenciesResponse)

-- | The returned list of
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageDependency.html PackageDependency>
-- objects.
listPackageVersionDependenciesResponse_dependencies :: Lens.Lens' ListPackageVersionDependenciesResponse (Prelude.Maybe [PackageDependency])
listPackageVersionDependenciesResponse_dependencies = Lens.lens (\ListPackageVersionDependenciesResponse' {dependencies} -> dependencies) (\s@ListPackageVersionDependenciesResponse' {} a -> s {dependencies = a} :: ListPackageVersionDependenciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listPackageVersionDependenciesResponse_nextToken :: Lens.Lens' ListPackageVersionDependenciesResponse (Prelude.Maybe Prelude.Text)
listPackageVersionDependenciesResponse_nextToken = Lens.lens (\ListPackageVersionDependenciesResponse' {nextToken} -> nextToken) (\s@ListPackageVersionDependenciesResponse' {} a -> s {nextToken = a} :: ListPackageVersionDependenciesResponse)

-- | The version of the package that is specified in the request.
listPackageVersionDependenciesResponse_version :: Lens.Lens' ListPackageVersionDependenciesResponse (Prelude.Maybe Prelude.Text)
listPackageVersionDependenciesResponse_version = Lens.lens (\ListPackageVersionDependenciesResponse' {version} -> version) (\s@ListPackageVersionDependenciesResponse' {} a -> s {version = a} :: ListPackageVersionDependenciesResponse)

-- | The name of the package that contains the returned package versions
-- dependencies.
listPackageVersionDependenciesResponse_package :: Lens.Lens' ListPackageVersionDependenciesResponse (Prelude.Maybe Prelude.Text)
listPackageVersionDependenciesResponse_package = Lens.lens (\ListPackageVersionDependenciesResponse' {package} -> package) (\s@ListPackageVersionDependenciesResponse' {} a -> s {package = a} :: ListPackageVersionDependenciesResponse)

-- | The response's http status code.
listPackageVersionDependenciesResponse_httpStatus :: Lens.Lens' ListPackageVersionDependenciesResponse Prelude.Int
listPackageVersionDependenciesResponse_httpStatus = Lens.lens (\ListPackageVersionDependenciesResponse' {httpStatus} -> httpStatus) (\s@ListPackageVersionDependenciesResponse' {} a -> s {httpStatus = a} :: ListPackageVersionDependenciesResponse)

instance
  Prelude.NFData
    ListPackageVersionDependenciesResponse
  where
  rnf ListPackageVersionDependenciesResponse' {..} =
    Prelude.rnf format
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf versionRevision
      `Prelude.seq` Prelude.rnf dependencies
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf package
      `Prelude.seq` Prelude.rnf httpStatus
