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
-- Module      : Amazonka.CodeArtifact.UpdatePackageVersionsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of one or more versions of a package. Using
-- @UpdatePackageVersionsStatus@, you can update the status of package
-- versions to @Archived@, @Published@, or @Unlisted@. To set the status of
-- a package version to @Disposed@, use
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_DisposePackageVersions.html DisposePackageVersions>.
module Amazonka.CodeArtifact.UpdatePackageVersionsStatus
  ( -- * Creating a Request
    UpdatePackageVersionsStatus (..),
    newUpdatePackageVersionsStatus,

    -- * Request Lenses
    updatePackageVersionsStatus_domainOwner,
    updatePackageVersionsStatus_expectedStatus,
    updatePackageVersionsStatus_namespace,
    updatePackageVersionsStatus_versionRevisions,
    updatePackageVersionsStatus_domain,
    updatePackageVersionsStatus_repository,
    updatePackageVersionsStatus_format,
    updatePackageVersionsStatus_package,
    updatePackageVersionsStatus_versions,
    updatePackageVersionsStatus_targetStatus,

    -- * Destructuring the Response
    UpdatePackageVersionsStatusResponse (..),
    newUpdatePackageVersionsStatusResponse,

    -- * Response Lenses
    updatePackageVersionsStatusResponse_failedVersions,
    updatePackageVersionsStatusResponse_successfulVersions,
    updatePackageVersionsStatusResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePackageVersionsStatus' smart constructor.
data UpdatePackageVersionsStatus = UpdatePackageVersionsStatus'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The package version’s expected status before it is updated. If
    -- @expectedStatus@ is provided, the package version\'s status is updated
    -- only if its status at the time @UpdatePackageVersionsStatus@ is called
    -- matches @expectedStatus@.
    expectedStatus :: Prelude.Maybe PackageVersionStatus,
    -- | The namespace of the package version to be updated. The package version
    -- component that specifies its namespace depends on its type. For example:
    --
    -- -   The namespace of a Maven package version is its @groupId@.
    --
    -- -   The namespace of an npm package version is its @scope@.
    --
    -- -   Python and NuGet package versions do not contain a corresponding
    --     component, package versions of those formats do not have a
    --     namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | A map of package versions and package version revisions. The map @key@
    -- is the package version (for example, @3.5.2@), and the map @value@ is
    -- the package version revision.
    versionRevisions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the domain that contains the repository that contains the
    -- package versions with a status to be updated.
    domain :: Prelude.Text,
    -- | The repository that contains the package versions with the status you
    -- want to update.
    repository :: Prelude.Text,
    -- | A format that specifies the type of the package with the statuses to
    -- update.
    format :: PackageFormat,
    -- | The name of the package with the version statuses to update.
    package :: Prelude.Text,
    -- | An array of strings that specify the versions of the package with the
    -- statuses to update.
    versions :: [Prelude.Text],
    -- | The status you want to change the package version status to.
    targetStatus :: PackageVersionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePackageVersionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'updatePackageVersionsStatus_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'expectedStatus', 'updatePackageVersionsStatus_expectedStatus' - The package version’s expected status before it is updated. If
-- @expectedStatus@ is provided, the package version\'s status is updated
-- only if its status at the time @UpdatePackageVersionsStatus@ is called
-- matches @expectedStatus@.
--
-- 'namespace', 'updatePackageVersionsStatus_namespace' - The namespace of the package version to be updated. The package version
-- component that specifies its namespace depends on its type. For example:
--
-- -   The namespace of a Maven package version is its @groupId@.
--
-- -   The namespace of an npm package version is its @scope@.
--
-- -   Python and NuGet package versions do not contain a corresponding
--     component, package versions of those formats do not have a
--     namespace.
--
-- 'versionRevisions', 'updatePackageVersionsStatus_versionRevisions' - A map of package versions and package version revisions. The map @key@
-- is the package version (for example, @3.5.2@), and the map @value@ is
-- the package version revision.
--
-- 'domain', 'updatePackageVersionsStatus_domain' - The name of the domain that contains the repository that contains the
-- package versions with a status to be updated.
--
-- 'repository', 'updatePackageVersionsStatus_repository' - The repository that contains the package versions with the status you
-- want to update.
--
-- 'format', 'updatePackageVersionsStatus_format' - A format that specifies the type of the package with the statuses to
-- update.
--
-- 'package', 'updatePackageVersionsStatus_package' - The name of the package with the version statuses to update.
--
-- 'versions', 'updatePackageVersionsStatus_versions' - An array of strings that specify the versions of the package with the
-- statuses to update.
--
-- 'targetStatus', 'updatePackageVersionsStatus_targetStatus' - The status you want to change the package version status to.
newUpdatePackageVersionsStatus ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  -- | 'format'
  PackageFormat ->
  -- | 'package'
  Prelude.Text ->
  -- | 'targetStatus'
  PackageVersionStatus ->
  UpdatePackageVersionsStatus
newUpdatePackageVersionsStatus
  pDomain_
  pRepository_
  pFormat_
  pPackage_
  pTargetStatus_ =
    UpdatePackageVersionsStatus'
      { domainOwner =
          Prelude.Nothing,
        expectedStatus = Prelude.Nothing,
        namespace = Prelude.Nothing,
        versionRevisions = Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        format = pFormat_,
        package = pPackage_,
        versions = Prelude.mempty,
        targetStatus = pTargetStatus_
      }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
updatePackageVersionsStatus_domainOwner :: Lens.Lens' UpdatePackageVersionsStatus (Prelude.Maybe Prelude.Text)
updatePackageVersionsStatus_domainOwner = Lens.lens (\UpdatePackageVersionsStatus' {domainOwner} -> domainOwner) (\s@UpdatePackageVersionsStatus' {} a -> s {domainOwner = a} :: UpdatePackageVersionsStatus)

-- | The package version’s expected status before it is updated. If
-- @expectedStatus@ is provided, the package version\'s status is updated
-- only if its status at the time @UpdatePackageVersionsStatus@ is called
-- matches @expectedStatus@.
updatePackageVersionsStatus_expectedStatus :: Lens.Lens' UpdatePackageVersionsStatus (Prelude.Maybe PackageVersionStatus)
updatePackageVersionsStatus_expectedStatus = Lens.lens (\UpdatePackageVersionsStatus' {expectedStatus} -> expectedStatus) (\s@UpdatePackageVersionsStatus' {} a -> s {expectedStatus = a} :: UpdatePackageVersionsStatus)

-- | The namespace of the package version to be updated. The package version
-- component that specifies its namespace depends on its type. For example:
--
-- -   The namespace of a Maven package version is its @groupId@.
--
-- -   The namespace of an npm package version is its @scope@.
--
-- -   Python and NuGet package versions do not contain a corresponding
--     component, package versions of those formats do not have a
--     namespace.
updatePackageVersionsStatus_namespace :: Lens.Lens' UpdatePackageVersionsStatus (Prelude.Maybe Prelude.Text)
updatePackageVersionsStatus_namespace = Lens.lens (\UpdatePackageVersionsStatus' {namespace} -> namespace) (\s@UpdatePackageVersionsStatus' {} a -> s {namespace = a} :: UpdatePackageVersionsStatus)

-- | A map of package versions and package version revisions. The map @key@
-- is the package version (for example, @3.5.2@), and the map @value@ is
-- the package version revision.
updatePackageVersionsStatus_versionRevisions :: Lens.Lens' UpdatePackageVersionsStatus (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updatePackageVersionsStatus_versionRevisions = Lens.lens (\UpdatePackageVersionsStatus' {versionRevisions} -> versionRevisions) (\s@UpdatePackageVersionsStatus' {} a -> s {versionRevisions = a} :: UpdatePackageVersionsStatus) Prelude.. Lens.mapping Lens.coerced

-- | The name of the domain that contains the repository that contains the
-- package versions with a status to be updated.
updatePackageVersionsStatus_domain :: Lens.Lens' UpdatePackageVersionsStatus Prelude.Text
updatePackageVersionsStatus_domain = Lens.lens (\UpdatePackageVersionsStatus' {domain} -> domain) (\s@UpdatePackageVersionsStatus' {} a -> s {domain = a} :: UpdatePackageVersionsStatus)

-- | The repository that contains the package versions with the status you
-- want to update.
updatePackageVersionsStatus_repository :: Lens.Lens' UpdatePackageVersionsStatus Prelude.Text
updatePackageVersionsStatus_repository = Lens.lens (\UpdatePackageVersionsStatus' {repository} -> repository) (\s@UpdatePackageVersionsStatus' {} a -> s {repository = a} :: UpdatePackageVersionsStatus)

-- | A format that specifies the type of the package with the statuses to
-- update.
updatePackageVersionsStatus_format :: Lens.Lens' UpdatePackageVersionsStatus PackageFormat
updatePackageVersionsStatus_format = Lens.lens (\UpdatePackageVersionsStatus' {format} -> format) (\s@UpdatePackageVersionsStatus' {} a -> s {format = a} :: UpdatePackageVersionsStatus)

-- | The name of the package with the version statuses to update.
updatePackageVersionsStatus_package :: Lens.Lens' UpdatePackageVersionsStatus Prelude.Text
updatePackageVersionsStatus_package = Lens.lens (\UpdatePackageVersionsStatus' {package} -> package) (\s@UpdatePackageVersionsStatus' {} a -> s {package = a} :: UpdatePackageVersionsStatus)

-- | An array of strings that specify the versions of the package with the
-- statuses to update.
updatePackageVersionsStatus_versions :: Lens.Lens' UpdatePackageVersionsStatus [Prelude.Text]
updatePackageVersionsStatus_versions = Lens.lens (\UpdatePackageVersionsStatus' {versions} -> versions) (\s@UpdatePackageVersionsStatus' {} a -> s {versions = a} :: UpdatePackageVersionsStatus) Prelude.. Lens.coerced

-- | The status you want to change the package version status to.
updatePackageVersionsStatus_targetStatus :: Lens.Lens' UpdatePackageVersionsStatus PackageVersionStatus
updatePackageVersionsStatus_targetStatus = Lens.lens (\UpdatePackageVersionsStatus' {targetStatus} -> targetStatus) (\s@UpdatePackageVersionsStatus' {} a -> s {targetStatus = a} :: UpdatePackageVersionsStatus)

instance Core.AWSRequest UpdatePackageVersionsStatus where
  type
    AWSResponse UpdatePackageVersionsStatus =
      UpdatePackageVersionsStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePackageVersionsStatusResponse'
            Prelude.<$> (x Data..?> "failedVersions" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "successfulVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePackageVersionsStatus where
  hashWithSalt _salt UpdatePackageVersionsStatus' {..} =
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
      `Prelude.hashWithSalt` targetStatus

instance Prelude.NFData UpdatePackageVersionsStatus where
  rnf UpdatePackageVersionsStatus' {..} =
    Prelude.rnf domainOwner `Prelude.seq`
      Prelude.rnf expectedStatus `Prelude.seq`
        Prelude.rnf namespace `Prelude.seq`
          Prelude.rnf versionRevisions `Prelude.seq`
            Prelude.rnf domain `Prelude.seq`
              Prelude.rnf repository `Prelude.seq`
                Prelude.rnf format `Prelude.seq`
                  Prelude.rnf package `Prelude.seq`
                    Prelude.rnf versions `Prelude.seq`
                      Prelude.rnf targetStatus

instance Data.ToHeaders UpdatePackageVersionsStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePackageVersionsStatus where
  toJSON UpdatePackageVersionsStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("expectedStatus" Data..=)
              Prelude.<$> expectedStatus,
            ("versionRevisions" Data..=)
              Prelude.<$> versionRevisions,
            Prelude.Just ("versions" Data..= versions),
            Prelude.Just ("targetStatus" Data..= targetStatus)
          ]
      )

instance Data.ToPath UpdatePackageVersionsStatus where
  toPath =
    Prelude.const "/v1/package/versions/update_status"

instance Data.ToQuery UpdatePackageVersionsStatus where
  toQuery UpdatePackageVersionsStatus' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "namespace" Data.=: namespace,
        "domain" Data.=: domain,
        "repository" Data.=: repository,
        "format" Data.=: format,
        "package" Data.=: package
      ]

-- | /See:/ 'newUpdatePackageVersionsStatusResponse' smart constructor.
data UpdatePackageVersionsStatusResponse = UpdatePackageVersionsStatusResponse'
  { -- | A list of @SuccessfulPackageVersionInfo@ objects, one for each package
    -- version with a status that successfully updated.
    failedVersions :: Prelude.Maybe (Prelude.HashMap Prelude.Text PackageVersionError),
    -- | A list of @PackageVersionError@ objects, one for each package version
    -- with a status that failed to update.
    successfulVersions :: Prelude.Maybe (Prelude.HashMap Prelude.Text SuccessfulPackageVersionInfo),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePackageVersionsStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedVersions', 'updatePackageVersionsStatusResponse_failedVersions' - A list of @SuccessfulPackageVersionInfo@ objects, one for each package
-- version with a status that successfully updated.
--
-- 'successfulVersions', 'updatePackageVersionsStatusResponse_successfulVersions' - A list of @PackageVersionError@ objects, one for each package version
-- with a status that failed to update.
--
-- 'httpStatus', 'updatePackageVersionsStatusResponse_httpStatus' - The response's http status code.
newUpdatePackageVersionsStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePackageVersionsStatusResponse
newUpdatePackageVersionsStatusResponse pHttpStatus_ =
  UpdatePackageVersionsStatusResponse'
    { failedVersions =
        Prelude.Nothing,
      successfulVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @SuccessfulPackageVersionInfo@ objects, one for each package
-- version with a status that successfully updated.
updatePackageVersionsStatusResponse_failedVersions :: Lens.Lens' UpdatePackageVersionsStatusResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text PackageVersionError))
updatePackageVersionsStatusResponse_failedVersions = Lens.lens (\UpdatePackageVersionsStatusResponse' {failedVersions} -> failedVersions) (\s@UpdatePackageVersionsStatusResponse' {} a -> s {failedVersions = a} :: UpdatePackageVersionsStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of @PackageVersionError@ objects, one for each package version
-- with a status that failed to update.
updatePackageVersionsStatusResponse_successfulVersions :: Lens.Lens' UpdatePackageVersionsStatusResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text SuccessfulPackageVersionInfo))
updatePackageVersionsStatusResponse_successfulVersions = Lens.lens (\UpdatePackageVersionsStatusResponse' {successfulVersions} -> successfulVersions) (\s@UpdatePackageVersionsStatusResponse' {} a -> s {successfulVersions = a} :: UpdatePackageVersionsStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updatePackageVersionsStatusResponse_httpStatus :: Lens.Lens' UpdatePackageVersionsStatusResponse Prelude.Int
updatePackageVersionsStatusResponse_httpStatus = Lens.lens (\UpdatePackageVersionsStatusResponse' {httpStatus} -> httpStatus) (\s@UpdatePackageVersionsStatusResponse' {} a -> s {httpStatus = a} :: UpdatePackageVersionsStatusResponse)

instance
  Prelude.NFData
    UpdatePackageVersionsStatusResponse
  where
  rnf UpdatePackageVersionsStatusResponse' {..} =
    Prelude.rnf failedVersions `Prelude.seq`
      Prelude.rnf successfulVersions `Prelude.seq`
        Prelude.rnf httpStatus
