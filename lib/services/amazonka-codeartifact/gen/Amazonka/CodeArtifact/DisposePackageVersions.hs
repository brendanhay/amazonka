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
-- Module      : Amazonka.CodeArtifact.DisposePackageVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assets in package versions and sets the package versions\'
-- status to @Disposed@. A disposed package version cannot be restored in
-- your repository because its assets are deleted.
--
-- To view all disposed package versions in a repository, use
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_ListPackageVersions.html ListPackageVersions>
-- and set the
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_ListPackageVersions.html#API_ListPackageVersions_RequestSyntax status>
-- parameter to @Disposed@.
--
-- To view information about a disposed package version, use
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_DescribePackageVersion.html DescribePackageVersion>.
module Amazonka.CodeArtifact.DisposePackageVersions
  ( -- * Creating a Request
    DisposePackageVersions (..),
    newDisposePackageVersions,

    -- * Request Lenses
    disposePackageVersions_domainOwner,
    disposePackageVersions_expectedStatus,
    disposePackageVersions_namespace,
    disposePackageVersions_versionRevisions,
    disposePackageVersions_domain,
    disposePackageVersions_repository,
    disposePackageVersions_format,
    disposePackageVersions_package,
    disposePackageVersions_versions,

    -- * Destructuring the Response
    DisposePackageVersionsResponse (..),
    newDisposePackageVersionsResponse,

    -- * Response Lenses
    disposePackageVersionsResponse_failedVersions,
    disposePackageVersionsResponse_successfulVersions,
    disposePackageVersionsResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisposePackageVersions' smart constructor.
data DisposePackageVersions = DisposePackageVersions'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The expected status of the package version to dispose.
    expectedStatus :: Prelude.Maybe PackageVersionStatus,
    -- | The namespace of the package versions to be disposed. The package
    -- version component that specifies its namespace depends on its type. For
    -- example:
    --
    -- -   The namespace of a Maven package version is its @groupId@.
    --
    -- -   The namespace of an npm package version is its @scope@.
    --
    -- -   Python and NuGet package versions do not contain a corresponding
    --     component, package versions of those formats do not have a
    --     namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The revisions of the package versions you want to dispose.
    versionRevisions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the domain that contains the repository you want to dispose.
    domain :: Prelude.Text,
    -- | The name of the repository that contains the package versions you want
    -- to dispose.
    repository :: Prelude.Text,
    -- | A format that specifies the type of package versions you want to
    -- dispose.
    format :: PackageFormat,
    -- | The name of the package with the versions you want to dispose.
    package :: Prelude.Text,
    -- | The versions of the package you want to dispose.
    versions :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisposePackageVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'disposePackageVersions_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'expectedStatus', 'disposePackageVersions_expectedStatus' - The expected status of the package version to dispose.
--
-- 'namespace', 'disposePackageVersions_namespace' - The namespace of the package versions to be disposed. The package
-- version component that specifies its namespace depends on its type. For
-- example:
--
-- -   The namespace of a Maven package version is its @groupId@.
--
-- -   The namespace of an npm package version is its @scope@.
--
-- -   Python and NuGet package versions do not contain a corresponding
--     component, package versions of those formats do not have a
--     namespace.
--
-- 'versionRevisions', 'disposePackageVersions_versionRevisions' - The revisions of the package versions you want to dispose.
--
-- 'domain', 'disposePackageVersions_domain' - The name of the domain that contains the repository you want to dispose.
--
-- 'repository', 'disposePackageVersions_repository' - The name of the repository that contains the package versions you want
-- to dispose.
--
-- 'format', 'disposePackageVersions_format' - A format that specifies the type of package versions you want to
-- dispose.
--
-- 'package', 'disposePackageVersions_package' - The name of the package with the versions you want to dispose.
--
-- 'versions', 'disposePackageVersions_versions' - The versions of the package you want to dispose.
newDisposePackageVersions ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  -- | 'format'
  PackageFormat ->
  -- | 'package'
  Prelude.Text ->
  DisposePackageVersions
newDisposePackageVersions
  pDomain_
  pRepository_
  pFormat_
  pPackage_ =
    DisposePackageVersions'
      { domainOwner =
          Prelude.Nothing,
        expectedStatus = Prelude.Nothing,
        namespace = Prelude.Nothing,
        versionRevisions = Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        format = pFormat_,
        package = pPackage_,
        versions = Prelude.mempty
      }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
disposePackageVersions_domainOwner :: Lens.Lens' DisposePackageVersions (Prelude.Maybe Prelude.Text)
disposePackageVersions_domainOwner = Lens.lens (\DisposePackageVersions' {domainOwner} -> domainOwner) (\s@DisposePackageVersions' {} a -> s {domainOwner = a} :: DisposePackageVersions)

-- | The expected status of the package version to dispose.
disposePackageVersions_expectedStatus :: Lens.Lens' DisposePackageVersions (Prelude.Maybe PackageVersionStatus)
disposePackageVersions_expectedStatus = Lens.lens (\DisposePackageVersions' {expectedStatus} -> expectedStatus) (\s@DisposePackageVersions' {} a -> s {expectedStatus = a} :: DisposePackageVersions)

-- | The namespace of the package versions to be disposed. The package
-- version component that specifies its namespace depends on its type. For
-- example:
--
-- -   The namespace of a Maven package version is its @groupId@.
--
-- -   The namespace of an npm package version is its @scope@.
--
-- -   Python and NuGet package versions do not contain a corresponding
--     component, package versions of those formats do not have a
--     namespace.
disposePackageVersions_namespace :: Lens.Lens' DisposePackageVersions (Prelude.Maybe Prelude.Text)
disposePackageVersions_namespace = Lens.lens (\DisposePackageVersions' {namespace} -> namespace) (\s@DisposePackageVersions' {} a -> s {namespace = a} :: DisposePackageVersions)

-- | The revisions of the package versions you want to dispose.
disposePackageVersions_versionRevisions :: Lens.Lens' DisposePackageVersions (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
disposePackageVersions_versionRevisions = Lens.lens (\DisposePackageVersions' {versionRevisions} -> versionRevisions) (\s@DisposePackageVersions' {} a -> s {versionRevisions = a} :: DisposePackageVersions) Prelude.. Lens.mapping Lens.coerced

-- | The name of the domain that contains the repository you want to dispose.
disposePackageVersions_domain :: Lens.Lens' DisposePackageVersions Prelude.Text
disposePackageVersions_domain = Lens.lens (\DisposePackageVersions' {domain} -> domain) (\s@DisposePackageVersions' {} a -> s {domain = a} :: DisposePackageVersions)

-- | The name of the repository that contains the package versions you want
-- to dispose.
disposePackageVersions_repository :: Lens.Lens' DisposePackageVersions Prelude.Text
disposePackageVersions_repository = Lens.lens (\DisposePackageVersions' {repository} -> repository) (\s@DisposePackageVersions' {} a -> s {repository = a} :: DisposePackageVersions)

-- | A format that specifies the type of package versions you want to
-- dispose.
disposePackageVersions_format :: Lens.Lens' DisposePackageVersions PackageFormat
disposePackageVersions_format = Lens.lens (\DisposePackageVersions' {format} -> format) (\s@DisposePackageVersions' {} a -> s {format = a} :: DisposePackageVersions)

-- | The name of the package with the versions you want to dispose.
disposePackageVersions_package :: Lens.Lens' DisposePackageVersions Prelude.Text
disposePackageVersions_package = Lens.lens (\DisposePackageVersions' {package} -> package) (\s@DisposePackageVersions' {} a -> s {package = a} :: DisposePackageVersions)

-- | The versions of the package you want to dispose.
disposePackageVersions_versions :: Lens.Lens' DisposePackageVersions [Prelude.Text]
disposePackageVersions_versions = Lens.lens (\DisposePackageVersions' {versions} -> versions) (\s@DisposePackageVersions' {} a -> s {versions = a} :: DisposePackageVersions) Prelude.. Lens.coerced

instance Core.AWSRequest DisposePackageVersions where
  type
    AWSResponse DisposePackageVersions =
      DisposePackageVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisposePackageVersionsResponse'
            Prelude.<$> (x Data..?> "failedVersions" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "successfulVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisposePackageVersions where
  hashWithSalt _salt DisposePackageVersions' {..} =
    _salt
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` expectedStatus
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` versionRevisions
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` package
      `Prelude.hashWithSalt` versions

instance Prelude.NFData DisposePackageVersions where
  rnf DisposePackageVersions' {..} =
    Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf expectedStatus
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf versionRevisions
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf package
      `Prelude.seq` Prelude.rnf versions

instance Data.ToHeaders DisposePackageVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisposePackageVersions where
  toJSON DisposePackageVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("expectedStatus" Data..=)
              Prelude.<$> expectedStatus,
            ("versionRevisions" Data..=)
              Prelude.<$> versionRevisions,
            Prelude.Just ("versions" Data..= versions)
          ]
      )

instance Data.ToPath DisposePackageVersions where
  toPath = Prelude.const "/v1/package/versions/dispose"

instance Data.ToQuery DisposePackageVersions where
  toQuery DisposePackageVersions' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "namespace" Data.=: namespace,
        "domain" Data.=: domain,
        "repository" Data.=: repository,
        "format" Data.=: format,
        "package" Data.=: package
      ]

-- | /See:/ 'newDisposePackageVersionsResponse' smart constructor.
data DisposePackageVersionsResponse = DisposePackageVersionsResponse'
  { -- | A @PackageVersionError@ object that contains a map of errors codes for
    -- the disposed package versions that failed. The possible error codes are:
    --
    -- -   @ALREADY_EXISTS@
    --
    -- -   @MISMATCHED_REVISION@
    --
    -- -   @MISMATCHED_STATUS@
    --
    -- -   @NOT_ALLOWED@
    --
    -- -   @NOT_FOUND@
    --
    -- -   @SKIPPED@
    failedVersions :: Prelude.Maybe (Prelude.HashMap Prelude.Text PackageVersionError),
    -- | A list of the package versions that were successfully disposed.
    successfulVersions :: Prelude.Maybe (Prelude.HashMap Prelude.Text SuccessfulPackageVersionInfo),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisposePackageVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedVersions', 'disposePackageVersionsResponse_failedVersions' - A @PackageVersionError@ object that contains a map of errors codes for
-- the disposed package versions that failed. The possible error codes are:
--
-- -   @ALREADY_EXISTS@
--
-- -   @MISMATCHED_REVISION@
--
-- -   @MISMATCHED_STATUS@
--
-- -   @NOT_ALLOWED@
--
-- -   @NOT_FOUND@
--
-- -   @SKIPPED@
--
-- 'successfulVersions', 'disposePackageVersionsResponse_successfulVersions' - A list of the package versions that were successfully disposed.
--
-- 'httpStatus', 'disposePackageVersionsResponse_httpStatus' - The response's http status code.
newDisposePackageVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisposePackageVersionsResponse
newDisposePackageVersionsResponse pHttpStatus_ =
  DisposePackageVersionsResponse'
    { failedVersions =
        Prelude.Nothing,
      successfulVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @PackageVersionError@ object that contains a map of errors codes for
-- the disposed package versions that failed. The possible error codes are:
--
-- -   @ALREADY_EXISTS@
--
-- -   @MISMATCHED_REVISION@
--
-- -   @MISMATCHED_STATUS@
--
-- -   @NOT_ALLOWED@
--
-- -   @NOT_FOUND@
--
-- -   @SKIPPED@
disposePackageVersionsResponse_failedVersions :: Lens.Lens' DisposePackageVersionsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text PackageVersionError))
disposePackageVersionsResponse_failedVersions = Lens.lens (\DisposePackageVersionsResponse' {failedVersions} -> failedVersions) (\s@DisposePackageVersionsResponse' {} a -> s {failedVersions = a} :: DisposePackageVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the package versions that were successfully disposed.
disposePackageVersionsResponse_successfulVersions :: Lens.Lens' DisposePackageVersionsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text SuccessfulPackageVersionInfo))
disposePackageVersionsResponse_successfulVersions = Lens.lens (\DisposePackageVersionsResponse' {successfulVersions} -> successfulVersions) (\s@DisposePackageVersionsResponse' {} a -> s {successfulVersions = a} :: DisposePackageVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
disposePackageVersionsResponse_httpStatus :: Lens.Lens' DisposePackageVersionsResponse Prelude.Int
disposePackageVersionsResponse_httpStatus = Lens.lens (\DisposePackageVersionsResponse' {httpStatus} -> httpStatus) (\s@DisposePackageVersionsResponse' {} a -> s {httpStatus = a} :: DisposePackageVersionsResponse)

instance
  Prelude.NFData
    DisposePackageVersionsResponse
  where
  rnf DisposePackageVersionsResponse' {..} =
    Prelude.rnf failedVersions
      `Prelude.seq` Prelude.rnf successfulVersions
      `Prelude.seq` Prelude.rnf httpStatus
