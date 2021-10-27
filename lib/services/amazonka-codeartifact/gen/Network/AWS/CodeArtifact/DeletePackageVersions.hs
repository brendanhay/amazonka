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
-- Module      : Network.AWS.CodeArtifact.DeletePackageVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.CodeArtifact.DeletePackageVersions
  ( -- * Creating a Request
    DeletePackageVersions (..),
    newDeletePackageVersions,

    -- * Request Lenses
    deletePackageVersions_expectedStatus,
    deletePackageVersions_namespace,
    deletePackageVersions_domainOwner,
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

import Network.AWS.CodeArtifact.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePackageVersions' smart constructor.
data DeletePackageVersions = DeletePackageVersions'
  { -- | The expected status of the package version to delete. Valid values are:
    --
    -- -   @Published@
    --
    -- -   @Unfinished@
    --
    -- -   @Unlisted@
    --
    -- -   @Archived@
    --
    -- -   @Disposed@
    expectedStatus :: Prelude.Maybe PackageVersionStatus,
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
    -- | The name of the domain that contains the package to delete.
    domain :: Prelude.Text,
    -- | The name of the repository that contains the package versions to delete.
    repository :: Prelude.Text,
    -- | The format of the package versions to delete. The valid values are:
    --
    -- -   @npm@
    --
    -- -   @pypi@
    --
    -- -   @maven@
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
-- 'expectedStatus', 'deletePackageVersions_expectedStatus' - The expected status of the package version to delete. Valid values are:
--
-- -   @Published@
--
-- -   @Unfinished@
--
-- -   @Unlisted@
--
-- -   @Archived@
--
-- -   @Disposed@
--
-- 'namespace', 'deletePackageVersions_namespace' - The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   A Python package does not contain a corresponding component, so
--     Python packages do not have a namespace.
--
-- 'domainOwner', 'deletePackageVersions_domainOwner' - The 12-digit account number of the AWS account that owns the domain. It
-- does not include dashes or spaces.
--
-- 'domain', 'deletePackageVersions_domain' - The name of the domain that contains the package to delete.
--
-- 'repository', 'deletePackageVersions_repository' - The name of the repository that contains the package versions to delete.
--
-- 'format', 'deletePackageVersions_format' - The format of the package versions to delete. The valid values are:
--
-- -   @npm@
--
-- -   @pypi@
--
-- -   @maven@
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
      { expectedStatus =
          Prelude.Nothing,
        namespace = Prelude.Nothing,
        domainOwner = Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        format = pFormat_,
        package = pPackage_,
        versions = Prelude.mempty
      }

-- | The expected status of the package version to delete. Valid values are:
--
-- -   @Published@
--
-- -   @Unfinished@
--
-- -   @Unlisted@
--
-- -   @Archived@
--
-- -   @Disposed@
deletePackageVersions_expectedStatus :: Lens.Lens' DeletePackageVersions (Prelude.Maybe PackageVersionStatus)
deletePackageVersions_expectedStatus = Lens.lens (\DeletePackageVersions' {expectedStatus} -> expectedStatus) (\s@DeletePackageVersions' {} a -> s {expectedStatus = a} :: DeletePackageVersions)

-- | The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   A Python package does not contain a corresponding component, so
--     Python packages do not have a namespace.
deletePackageVersions_namespace :: Lens.Lens' DeletePackageVersions (Prelude.Maybe Prelude.Text)
deletePackageVersions_namespace = Lens.lens (\DeletePackageVersions' {namespace} -> namespace) (\s@DeletePackageVersions' {} a -> s {namespace = a} :: DeletePackageVersions)

-- | The 12-digit account number of the AWS account that owns the domain. It
-- does not include dashes or spaces.
deletePackageVersions_domainOwner :: Lens.Lens' DeletePackageVersions (Prelude.Maybe Prelude.Text)
deletePackageVersions_domainOwner = Lens.lens (\DeletePackageVersions' {domainOwner} -> domainOwner) (\s@DeletePackageVersions' {} a -> s {domainOwner = a} :: DeletePackageVersions)

-- | The name of the domain that contains the package to delete.
deletePackageVersions_domain :: Lens.Lens' DeletePackageVersions Prelude.Text
deletePackageVersions_domain = Lens.lens (\DeletePackageVersions' {domain} -> domain) (\s@DeletePackageVersions' {} a -> s {domain = a} :: DeletePackageVersions)

-- | The name of the repository that contains the package versions to delete.
deletePackageVersions_repository :: Lens.Lens' DeletePackageVersions Prelude.Text
deletePackageVersions_repository = Lens.lens (\DeletePackageVersions' {repository} -> repository) (\s@DeletePackageVersions' {} a -> s {repository = a} :: DeletePackageVersions)

-- | The format of the package versions to delete. The valid values are:
--
-- -   @npm@
--
-- -   @pypi@
--
-- -   @maven@
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePackageVersionsResponse'
            Prelude.<$> (x Core..?> "failedVersions" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "successfulVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePackageVersions

instance Prelude.NFData DeletePackageVersions

instance Core.ToHeaders DeletePackageVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeletePackageVersions where
  toJSON DeletePackageVersions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("expectedStatus" Core..=)
              Prelude.<$> expectedStatus,
            Prelude.Just ("versions" Core..= versions)
          ]
      )

instance Core.ToPath DeletePackageVersions where
  toPath = Prelude.const "/v1/package/versions/delete"

instance Core.ToQuery DeletePackageVersions where
  toQuery DeletePackageVersions' {..} =
    Prelude.mconcat
      [ "namespace" Core.=: namespace,
        "domain-owner" Core.=: domainOwner,
        "domain" Core.=: domain,
        "repository" Core.=: repository,
        "format" Core.=: format,
        "package" Core.=: package
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
    -- | A list of the package versions that were successfully deleted.
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
-- 'successfulVersions', 'deletePackageVersionsResponse_successfulVersions' - A list of the package versions that were successfully deleted.
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

-- | A list of the package versions that were successfully deleted.
deletePackageVersionsResponse_successfulVersions :: Lens.Lens' DeletePackageVersionsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text SuccessfulPackageVersionInfo))
deletePackageVersionsResponse_successfulVersions = Lens.lens (\DeletePackageVersionsResponse' {successfulVersions} -> successfulVersions) (\s@DeletePackageVersionsResponse' {} a -> s {successfulVersions = a} :: DeletePackageVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deletePackageVersionsResponse_httpStatus :: Lens.Lens' DeletePackageVersionsResponse Prelude.Int
deletePackageVersionsResponse_httpStatus = Lens.lens (\DeletePackageVersionsResponse' {httpStatus} -> httpStatus) (\s@DeletePackageVersionsResponse' {} a -> s {httpStatus = a} :: DeletePackageVersionsResponse)

instance Prelude.NFData DeletePackageVersionsResponse
