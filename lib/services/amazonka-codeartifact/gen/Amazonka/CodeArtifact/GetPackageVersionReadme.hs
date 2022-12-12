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
-- Module      : Amazonka.CodeArtifact.GetPackageVersionReadme
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the readme file or descriptive text for a package version. For
-- packages that do not contain a readme file, CodeArtifact extracts a
-- description from a metadata file. For example, from the @\<description>@
-- element in the @pom.xml@ file of a Maven package.
--
-- The returned text might contain formatting. For example, it might
-- contain formatting for Markdown or reStructuredText.
module Amazonka.CodeArtifact.GetPackageVersionReadme
  ( -- * Creating a Request
    GetPackageVersionReadme (..),
    newGetPackageVersionReadme,

    -- * Request Lenses
    getPackageVersionReadme_domainOwner,
    getPackageVersionReadme_namespace,
    getPackageVersionReadme_domain,
    getPackageVersionReadme_repository,
    getPackageVersionReadme_format,
    getPackageVersionReadme_package,
    getPackageVersionReadme_packageVersion,

    -- * Destructuring the Response
    GetPackageVersionReadmeResponse (..),
    newGetPackageVersionReadmeResponse,

    -- * Response Lenses
    getPackageVersionReadmeResponse_format,
    getPackageVersionReadmeResponse_namespace,
    getPackageVersionReadmeResponse_package,
    getPackageVersionReadmeResponse_readme,
    getPackageVersionReadmeResponse_version,
    getPackageVersionReadmeResponse_versionRevision,
    getPackageVersionReadmeResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPackageVersionReadme' smart constructor.
data GetPackageVersionReadme = GetPackageVersionReadme'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the package version with the requested readme file. The
    -- package version component that specifies its namespace depends on its
    -- type. For example:
    --
    -- -   The namespace of a Maven package version is its @groupId@.
    --
    -- -   The namespace of an npm package version is its @scope@.
    --
    -- -   Python and NuGet package versions do not contain a corresponding
    --     component, package versions of those formats do not have a
    --     namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the repository that contains the
    -- package version with the requested readme file.
    domain :: Prelude.Text,
    -- | The repository that contains the package with the requested readme file.
    repository :: Prelude.Text,
    -- | A format that specifies the type of the package version with the
    -- requested readme file.
    format :: PackageFormat,
    -- | The name of the package version that contains the requested readme file.
    package :: Prelude.Text,
    -- | A string that contains the package version (for example, @3.5.2@).
    packageVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPackageVersionReadme' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'getPackageVersionReadme_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'namespace', 'getPackageVersionReadme_namespace' - The namespace of the package version with the requested readme file. The
-- package version component that specifies its namespace depends on its
-- type. For example:
--
-- -   The namespace of a Maven package version is its @groupId@.
--
-- -   The namespace of an npm package version is its @scope@.
--
-- -   Python and NuGet package versions do not contain a corresponding
--     component, package versions of those formats do not have a
--     namespace.
--
-- 'domain', 'getPackageVersionReadme_domain' - The name of the domain that contains the repository that contains the
-- package version with the requested readme file.
--
-- 'repository', 'getPackageVersionReadme_repository' - The repository that contains the package with the requested readme file.
--
-- 'format', 'getPackageVersionReadme_format' - A format that specifies the type of the package version with the
-- requested readme file.
--
-- 'package', 'getPackageVersionReadme_package' - The name of the package version that contains the requested readme file.
--
-- 'packageVersion', 'getPackageVersionReadme_packageVersion' - A string that contains the package version (for example, @3.5.2@).
newGetPackageVersionReadme ::
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
  GetPackageVersionReadme
newGetPackageVersionReadme
  pDomain_
  pRepository_
  pFormat_
  pPackage_
  pPackageVersion_ =
    GetPackageVersionReadme'
      { domainOwner =
          Prelude.Nothing,
        namespace = Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        format = pFormat_,
        package = pPackage_,
        packageVersion = pPackageVersion_
      }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
getPackageVersionReadme_domainOwner :: Lens.Lens' GetPackageVersionReadme (Prelude.Maybe Prelude.Text)
getPackageVersionReadme_domainOwner = Lens.lens (\GetPackageVersionReadme' {domainOwner} -> domainOwner) (\s@GetPackageVersionReadme' {} a -> s {domainOwner = a} :: GetPackageVersionReadme)

-- | The namespace of the package version with the requested readme file. The
-- package version component that specifies its namespace depends on its
-- type. For example:
--
-- -   The namespace of a Maven package version is its @groupId@.
--
-- -   The namespace of an npm package version is its @scope@.
--
-- -   Python and NuGet package versions do not contain a corresponding
--     component, package versions of those formats do not have a
--     namespace.
getPackageVersionReadme_namespace :: Lens.Lens' GetPackageVersionReadme (Prelude.Maybe Prelude.Text)
getPackageVersionReadme_namespace = Lens.lens (\GetPackageVersionReadme' {namespace} -> namespace) (\s@GetPackageVersionReadme' {} a -> s {namespace = a} :: GetPackageVersionReadme)

-- | The name of the domain that contains the repository that contains the
-- package version with the requested readme file.
getPackageVersionReadme_domain :: Lens.Lens' GetPackageVersionReadme Prelude.Text
getPackageVersionReadme_domain = Lens.lens (\GetPackageVersionReadme' {domain} -> domain) (\s@GetPackageVersionReadme' {} a -> s {domain = a} :: GetPackageVersionReadme)

-- | The repository that contains the package with the requested readme file.
getPackageVersionReadme_repository :: Lens.Lens' GetPackageVersionReadme Prelude.Text
getPackageVersionReadme_repository = Lens.lens (\GetPackageVersionReadme' {repository} -> repository) (\s@GetPackageVersionReadme' {} a -> s {repository = a} :: GetPackageVersionReadme)

-- | A format that specifies the type of the package version with the
-- requested readme file.
getPackageVersionReadme_format :: Lens.Lens' GetPackageVersionReadme PackageFormat
getPackageVersionReadme_format = Lens.lens (\GetPackageVersionReadme' {format} -> format) (\s@GetPackageVersionReadme' {} a -> s {format = a} :: GetPackageVersionReadme)

-- | The name of the package version that contains the requested readme file.
getPackageVersionReadme_package :: Lens.Lens' GetPackageVersionReadme Prelude.Text
getPackageVersionReadme_package = Lens.lens (\GetPackageVersionReadme' {package} -> package) (\s@GetPackageVersionReadme' {} a -> s {package = a} :: GetPackageVersionReadme)

-- | A string that contains the package version (for example, @3.5.2@).
getPackageVersionReadme_packageVersion :: Lens.Lens' GetPackageVersionReadme Prelude.Text
getPackageVersionReadme_packageVersion = Lens.lens (\GetPackageVersionReadme' {packageVersion} -> packageVersion) (\s@GetPackageVersionReadme' {} a -> s {packageVersion = a} :: GetPackageVersionReadme)

instance Core.AWSRequest GetPackageVersionReadme where
  type
    AWSResponse GetPackageVersionReadme =
      GetPackageVersionReadmeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPackageVersionReadmeResponse'
            Prelude.<$> (x Data..?> "format")
            Prelude.<*> (x Data..?> "namespace")
            Prelude.<*> (x Data..?> "package")
            Prelude.<*> (x Data..?> "readme")
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (x Data..?> "versionRevision")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPackageVersionReadme where
  hashWithSalt _salt GetPackageVersionReadme' {..} =
    _salt `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` package
      `Prelude.hashWithSalt` packageVersion

instance Prelude.NFData GetPackageVersionReadme where
  rnf GetPackageVersionReadme' {..} =
    Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf package
      `Prelude.seq` Prelude.rnf packageVersion

instance Data.ToHeaders GetPackageVersionReadme where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetPackageVersionReadme where
  toPath = Prelude.const "/v1/package/version/readme"

instance Data.ToQuery GetPackageVersionReadme where
  toQuery GetPackageVersionReadme' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "namespace" Data.=: namespace,
        "domain" Data.=: domain,
        "repository" Data.=: repository,
        "format" Data.=: format,
        "package" Data.=: package,
        "version" Data.=: packageVersion
      ]

-- | /See:/ 'newGetPackageVersionReadmeResponse' smart constructor.
data GetPackageVersionReadmeResponse = GetPackageVersionReadmeResponse'
  { -- | The format of the package with the requested readme file.
    format :: Prelude.Maybe PackageFormat,
    -- | The namespace of the package version with the requested readme file. The
    -- package version component that specifies its namespace depends on its
    -- type. For example:
    --
    -- -   The namespace of a Maven package version is its @groupId@.
    --
    -- -   The namespace of an npm package version is its @scope@.
    --
    -- -   Python and NuGet package versions do not contain a corresponding
    --     component, package versions of those formats do not have a
    --     namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The name of the package that contains the returned readme file.
    package :: Prelude.Maybe Prelude.Text,
    -- | The text of the returned readme file.
    readme :: Prelude.Maybe Prelude.Text,
    -- | The version of the package with the requested readme file.
    version :: Prelude.Maybe Prelude.Text,
    -- | The current revision associated with the package version.
    versionRevision :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPackageVersionReadmeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'getPackageVersionReadmeResponse_format' - The format of the package with the requested readme file.
--
-- 'namespace', 'getPackageVersionReadmeResponse_namespace' - The namespace of the package version with the requested readme file. The
-- package version component that specifies its namespace depends on its
-- type. For example:
--
-- -   The namespace of a Maven package version is its @groupId@.
--
-- -   The namespace of an npm package version is its @scope@.
--
-- -   Python and NuGet package versions do not contain a corresponding
--     component, package versions of those formats do not have a
--     namespace.
--
-- 'package', 'getPackageVersionReadmeResponse_package' - The name of the package that contains the returned readme file.
--
-- 'readme', 'getPackageVersionReadmeResponse_readme' - The text of the returned readme file.
--
-- 'version', 'getPackageVersionReadmeResponse_version' - The version of the package with the requested readme file.
--
-- 'versionRevision', 'getPackageVersionReadmeResponse_versionRevision' - The current revision associated with the package version.
--
-- 'httpStatus', 'getPackageVersionReadmeResponse_httpStatus' - The response's http status code.
newGetPackageVersionReadmeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPackageVersionReadmeResponse
newGetPackageVersionReadmeResponse pHttpStatus_ =
  GetPackageVersionReadmeResponse'
    { format =
        Prelude.Nothing,
      namespace = Prelude.Nothing,
      package = Prelude.Nothing,
      readme = Prelude.Nothing,
      version = Prelude.Nothing,
      versionRevision = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The format of the package with the requested readme file.
getPackageVersionReadmeResponse_format :: Lens.Lens' GetPackageVersionReadmeResponse (Prelude.Maybe PackageFormat)
getPackageVersionReadmeResponse_format = Lens.lens (\GetPackageVersionReadmeResponse' {format} -> format) (\s@GetPackageVersionReadmeResponse' {} a -> s {format = a} :: GetPackageVersionReadmeResponse)

-- | The namespace of the package version with the requested readme file. The
-- package version component that specifies its namespace depends on its
-- type. For example:
--
-- -   The namespace of a Maven package version is its @groupId@.
--
-- -   The namespace of an npm package version is its @scope@.
--
-- -   Python and NuGet package versions do not contain a corresponding
--     component, package versions of those formats do not have a
--     namespace.
getPackageVersionReadmeResponse_namespace :: Lens.Lens' GetPackageVersionReadmeResponse (Prelude.Maybe Prelude.Text)
getPackageVersionReadmeResponse_namespace = Lens.lens (\GetPackageVersionReadmeResponse' {namespace} -> namespace) (\s@GetPackageVersionReadmeResponse' {} a -> s {namespace = a} :: GetPackageVersionReadmeResponse)

-- | The name of the package that contains the returned readme file.
getPackageVersionReadmeResponse_package :: Lens.Lens' GetPackageVersionReadmeResponse (Prelude.Maybe Prelude.Text)
getPackageVersionReadmeResponse_package = Lens.lens (\GetPackageVersionReadmeResponse' {package} -> package) (\s@GetPackageVersionReadmeResponse' {} a -> s {package = a} :: GetPackageVersionReadmeResponse)

-- | The text of the returned readme file.
getPackageVersionReadmeResponse_readme :: Lens.Lens' GetPackageVersionReadmeResponse (Prelude.Maybe Prelude.Text)
getPackageVersionReadmeResponse_readme = Lens.lens (\GetPackageVersionReadmeResponse' {readme} -> readme) (\s@GetPackageVersionReadmeResponse' {} a -> s {readme = a} :: GetPackageVersionReadmeResponse)

-- | The version of the package with the requested readme file.
getPackageVersionReadmeResponse_version :: Lens.Lens' GetPackageVersionReadmeResponse (Prelude.Maybe Prelude.Text)
getPackageVersionReadmeResponse_version = Lens.lens (\GetPackageVersionReadmeResponse' {version} -> version) (\s@GetPackageVersionReadmeResponse' {} a -> s {version = a} :: GetPackageVersionReadmeResponse)

-- | The current revision associated with the package version.
getPackageVersionReadmeResponse_versionRevision :: Lens.Lens' GetPackageVersionReadmeResponse (Prelude.Maybe Prelude.Text)
getPackageVersionReadmeResponse_versionRevision = Lens.lens (\GetPackageVersionReadmeResponse' {versionRevision} -> versionRevision) (\s@GetPackageVersionReadmeResponse' {} a -> s {versionRevision = a} :: GetPackageVersionReadmeResponse)

-- | The response's http status code.
getPackageVersionReadmeResponse_httpStatus :: Lens.Lens' GetPackageVersionReadmeResponse Prelude.Int
getPackageVersionReadmeResponse_httpStatus = Lens.lens (\GetPackageVersionReadmeResponse' {httpStatus} -> httpStatus) (\s@GetPackageVersionReadmeResponse' {} a -> s {httpStatus = a} :: GetPackageVersionReadmeResponse)

instance
  Prelude.NFData
    GetPackageVersionReadmeResponse
  where
  rnf GetPackageVersionReadmeResponse' {..} =
    Prelude.rnf format
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf package
      `Prelude.seq` Prelude.rnf readme
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf versionRevision
      `Prelude.seq` Prelude.rnf httpStatus
