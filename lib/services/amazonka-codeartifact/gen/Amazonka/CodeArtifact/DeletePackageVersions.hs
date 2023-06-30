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
-- Module      : Amazonka.CodeArtifact.DeletePackageVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more versions of a package. A deleted package version
-- cannot be restored in your repository. If you want to remove a package
-- version from your repository and be able to restore it later, set its
-- status to @Archived@. Archived packages cannot be downloaded from a
-- repository and don\'t show up with list package APIs (for example,
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_ListPackageVersions.html ListackageVersions>),
-- but you can restore them using
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_UpdatePackageVersionsStatus.html UpdatePackageVersionsStatus>.
module Amazonka.CodeArtifact.DeletePackageVersions
  ( -- * Creating a Request
    DeletePackageVersions (..),
    newDeletePackageVersions,

    -- * Request Lenses
    deletePackageVersions_domainOwner,
    deletePackageVersions_expectedStatus,
    deletePackageVersions_namespace,
    deletePackageVersions_domain,
    deletePackageVersions_repository,
    deletePackageVersions_format,
    deletePackageVersions_package,
    deletePackageVersions_versions,

    -- * Destructuring the Response
    DeletePackageVersionsResponse (..),
    newDeletePackageVersionsResponse,

    -- * Response Lenses
    deletePackageVersionsResponse_failedVersions,
    deletePackageVersionsResponse_successfulVersions,
    deletePackageVersionsResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePackageVersions' smart constructor.
data DeletePackageVersions = DeletePackageVersions'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The expected status of the package version to delete.
    expectedStatus :: Prelude.Maybe PackageVersionStatus,
    -- | The namespace of the package versions to be deleted. The package version
    -- component that specifies its namespace depends on its type. For example:
    --
    -- -   The namespace of a Maven package version is its @groupId@. The
    --     namespace is required when deleting Maven package versions.
    --
    -- -   The namespace of an npm package version is its @scope@.
    --
    -- -   Python and NuGet package versions do not contain a corresponding
    --     component, package versions of those formats do not have a
    --     namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the package to delete.
    domain :: Prelude.Text,
    -- | The name of the repository that contains the package versions to delete.
    repository :: Prelude.Text,
    -- | The format of the package versions to delete.
    format :: PackageFormat,
    -- | The name of the package with the versions to delete.
    package :: Prelude.Text,
    -- | An array of strings that specify the versions of the package to delete.
    versions :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePackageVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'deletePackageVersions_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'expectedStatus', 'deletePackageVersions_expectedStatus' - The expected status of the package version to delete.
--
-- 'namespace', 'deletePackageVersions_namespace' - The namespace of the package versions to be deleted. The package version
-- component that specifies its namespace depends on its type. For example:
--
-- -   The namespace of a Maven package version is its @groupId@. The
--     namespace is required when deleting Maven package versions.
--
-- -   The namespace of an npm package version is its @scope@.
--
-- -   Python and NuGet package versions do not contain a corresponding
--     component, package versions of those formats do not have a
--     namespace.
--
-- 'domain', 'deletePackageVersions_domain' - The name of the domain that contains the package to delete.
--
-- 'repository', 'deletePackageVersions_repository' - The name of the repository that contains the package versions to delete.
--
-- 'format', 'deletePackageVersions_format' - The format of the package versions to delete.
--
-- 'package', 'deletePackageVersions_package' - The name of the package with the versions to delete.
--
-- 'versions', 'deletePackageVersions_versions' - An array of strings that specify the versions of the package to delete.
newDeletePackageVersions ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  -- | 'format'
  PackageFormat ->
  -- | 'package'
  Prelude.Text ->
  DeletePackageVersions
newDeletePackageVersions
  pDomain_
  pRepository_
  pFormat_
  pPackage_ =
    DeletePackageVersions'
      { domainOwner =
          Prelude.Nothing,
        expectedStatus = Prelude.Nothing,
        namespace = Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        format = pFormat_,
        package = pPackage_,
        versions = Prelude.mempty
      }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
deletePackageVersions_domainOwner :: Lens.Lens' DeletePackageVersions (Prelude.Maybe Prelude.Text)
deletePackageVersions_domainOwner = Lens.lens (\DeletePackageVersions' {domainOwner} -> domainOwner) (\s@DeletePackageVersions' {} a -> s {domainOwner = a} :: DeletePackageVersions)

-- | The expected status of the package version to delete.
deletePackageVersions_expectedStatus :: Lens.Lens' DeletePackageVersions (Prelude.Maybe PackageVersionStatus)
deletePackageVersions_expectedStatus = Lens.lens (\DeletePackageVersions' {expectedStatus} -> expectedStatus) (\s@DeletePackageVersions' {} a -> s {expectedStatus = a} :: DeletePackageVersions)

-- | The namespace of the package versions to be deleted. The package version
-- component that specifies its namespace depends on its type. For example:
--
-- -   The namespace of a Maven package version is its @groupId@. The
--     namespace is required when deleting Maven package versions.
--
-- -   The namespace of an npm package version is its @scope@.
--
-- -   Python and NuGet package versions do not contain a corresponding
--     component, package versions of those formats do not have a
--     namespace.
deletePackageVersions_namespace :: Lens.Lens' DeletePackageVersions (Prelude.Maybe Prelude.Text)
deletePackageVersions_namespace = Lens.lens (\DeletePackageVersions' {namespace} -> namespace) (\s@DeletePackageVersions' {} a -> s {namespace = a} :: DeletePackageVersions)

-- | The name of the domain that contains the package to delete.
deletePackageVersions_domain :: Lens.Lens' DeletePackageVersions Prelude.Text
deletePackageVersions_domain = Lens.lens (\DeletePackageVersions' {domain} -> domain) (\s@DeletePackageVersions' {} a -> s {domain = a} :: DeletePackageVersions)

-- | The name of the repository that contains the package versions to delete.
deletePackageVersions_repository :: Lens.Lens' DeletePackageVersions Prelude.Text
deletePackageVersions_repository = Lens.lens (\DeletePackageVersions' {repository} -> repository) (\s@DeletePackageVersions' {} a -> s {repository = a} :: DeletePackageVersions)

-- | The format of the package versions to delete.
deletePackageVersions_format :: Lens.Lens' DeletePackageVersions PackageFormat
deletePackageVersions_format = Lens.lens (\DeletePackageVersions' {format} -> format) (\s@DeletePackageVersions' {} a -> s {format = a} :: DeletePackageVersions)

-- | The name of the package with the versions to delete.
deletePackageVersions_package :: Lens.Lens' DeletePackageVersions Prelude.Text
deletePackageVersions_package = Lens.lens (\DeletePackageVersions' {package} -> package) (\s@DeletePackageVersions' {} a -> s {package = a} :: DeletePackageVersions)

-- | An array of strings that specify the versions of the package to delete.
deletePackageVersions_versions :: Lens.Lens' DeletePackageVersions [Prelude.Text]
deletePackageVersions_versions = Lens.lens (\DeletePackageVersions' {versions} -> versions) (\s@DeletePackageVersions' {} a -> s {versions = a} :: DeletePackageVersions) Prelude.. Lens.coerced

instance Core.AWSRequest DeletePackageVersions where
  type
    AWSResponse DeletePackageVersions =
      DeletePackageVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePackageVersionsResponse'
            Prelude.<$> (x Data..?> "failedVersions" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "successfulVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePackageVersions where
  hashWithSalt _salt DeletePackageVersions' {..} =
    _salt
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` expectedStatus
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` package
      `Prelude.hashWithSalt` versions

instance Prelude.NFData DeletePackageVersions where
  rnf DeletePackageVersions' {..} =
    Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf expectedStatus
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf package
      `Prelude.seq` Prelude.rnf versions

instance Data.ToHeaders DeletePackageVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePackageVersions where
  toJSON DeletePackageVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("expectedStatus" Data..=)
              Prelude.<$> expectedStatus,
            Prelude.Just ("versions" Data..= versions)
          ]
      )

instance Data.ToPath DeletePackageVersions where
  toPath = Prelude.const "/v1/package/versions/delete"

instance Data.ToQuery DeletePackageVersions where
  toQuery DeletePackageVersions' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "namespace" Data.=: namespace,
        "domain" Data.=: domain,
        "repository" Data.=: repository,
        "format" Data.=: format,
        "package" Data.=: package
      ]

-- | /See:/ 'newDeletePackageVersionsResponse' smart constructor.
data DeletePackageVersionsResponse = DeletePackageVersionsResponse'
  { -- | A @PackageVersionError@ object that contains a map of errors codes for
    -- the deleted package that failed. The possible error codes are:
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
    -- | A list of the package versions that were successfully deleted. The
    -- status of every successful version will be @Deleted@.
    successfulVersions :: Prelude.Maybe (Prelude.HashMap Prelude.Text SuccessfulPackageVersionInfo),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePackageVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedVersions', 'deletePackageVersionsResponse_failedVersions' - A @PackageVersionError@ object that contains a map of errors codes for
-- the deleted package that failed. The possible error codes are:
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
-- 'successfulVersions', 'deletePackageVersionsResponse_successfulVersions' - A list of the package versions that were successfully deleted. The
-- status of every successful version will be @Deleted@.
--
-- 'httpStatus', 'deletePackageVersionsResponse_httpStatus' - The response's http status code.
newDeletePackageVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePackageVersionsResponse
newDeletePackageVersionsResponse pHttpStatus_ =
  DeletePackageVersionsResponse'
    { failedVersions =
        Prelude.Nothing,
      successfulVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @PackageVersionError@ object that contains a map of errors codes for
-- the deleted package that failed. The possible error codes are:
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
deletePackageVersionsResponse_failedVersions :: Lens.Lens' DeletePackageVersionsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text PackageVersionError))
deletePackageVersionsResponse_failedVersions = Lens.lens (\DeletePackageVersionsResponse' {failedVersions} -> failedVersions) (\s@DeletePackageVersionsResponse' {} a -> s {failedVersions = a} :: DeletePackageVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the package versions that were successfully deleted. The
-- status of every successful version will be @Deleted@.
deletePackageVersionsResponse_successfulVersions :: Lens.Lens' DeletePackageVersionsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text SuccessfulPackageVersionInfo))
deletePackageVersionsResponse_successfulVersions = Lens.lens (\DeletePackageVersionsResponse' {successfulVersions} -> successfulVersions) (\s@DeletePackageVersionsResponse' {} a -> s {successfulVersions = a} :: DeletePackageVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deletePackageVersionsResponse_httpStatus :: Lens.Lens' DeletePackageVersionsResponse Prelude.Int
deletePackageVersionsResponse_httpStatus = Lens.lens (\DeletePackageVersionsResponse' {httpStatus} -> httpStatus) (\s@DeletePackageVersionsResponse' {} a -> s {httpStatus = a} :: DeletePackageVersionsResponse)

instance Prelude.NFData DeletePackageVersionsResponse where
  rnf DeletePackageVersionsResponse' {..} =
    Prelude.rnf failedVersions
      `Prelude.seq` Prelude.rnf successfulVersions
      `Prelude.seq` Prelude.rnf httpStatus
