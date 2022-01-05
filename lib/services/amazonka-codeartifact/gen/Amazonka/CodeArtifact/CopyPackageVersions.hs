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
-- Module      : Amazonka.CodeArtifact.CopyPackageVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies package versions from one repository to another repository in the
-- same domain.
--
-- You must specify @versions@ or @versionRevisions@. You cannot specify
-- both.
module Amazonka.CodeArtifact.CopyPackageVersions
  ( -- * Creating a Request
    CopyPackageVersions (..),
    newCopyPackageVersions,

    -- * Request Lenses
    copyPackageVersions_allowOverwrite,
    copyPackageVersions_versionRevisions,
    copyPackageVersions_namespace,
    copyPackageVersions_domainOwner,
    copyPackageVersions_versions,
    copyPackageVersions_includeFromUpstream,
    copyPackageVersions_domain,
    copyPackageVersions_sourceRepository,
    copyPackageVersions_destinationRepository,
    copyPackageVersions_format,
    copyPackageVersions_package,

    -- * Destructuring the Response
    CopyPackageVersionsResponse (..),
    newCopyPackageVersionsResponse,

    -- * Response Lenses
    copyPackageVersionsResponse_failedVersions,
    copyPackageVersionsResponse_successfulVersions,
    copyPackageVersionsResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCopyPackageVersions' smart constructor.
data CopyPackageVersions = CopyPackageVersions'
  { -- | Set to true to overwrite a package version that already exists in the
    -- destination repository. If set to false and the package version already
    -- exists in the destination repository, the package version is returned in
    -- the @failedVersions@ field of the response with an @ALREADY_EXISTS@
    -- error code.
    allowOverwrite :: Prelude.Maybe Prelude.Bool,
    -- | A list of key-value pairs. The keys are package versions and the values
    -- are package version revisions. A @CopyPackageVersion@ operation succeeds
    -- if the specified versions in the source repository match the specified
    -- package version revision.
    --
    -- You must specify @versions@ or @versionRevisions@. You cannot specify
    -- both.
    versionRevisions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
    -- | The versions of the package to copy.
    --
    -- You must specify @versions@ or @versionRevisions@. You cannot specify
    -- both.
    versions :: Prelude.Maybe [Prelude.Text],
    -- | Set to true to copy packages from repositories that are upstream from
    -- the source repository to the destination repository. The default setting
    -- is false. For more information, see
    -- <https://docs.aws.amazon.com/codeartifact/latest/ug/repos-upstream.html Working with upstream repositories>.
    includeFromUpstream :: Prelude.Maybe Prelude.Bool,
    -- | The name of the domain that contains the source and destination
    -- repositories.
    domain :: Prelude.Text,
    -- | The name of the repository that contains the package versions to copy.
    sourceRepository :: Prelude.Text,
    -- | The name of the repository into which package versions are copied.
    destinationRepository :: Prelude.Text,
    -- | The format of the package that is copied. The valid package types are:
    --
    -- -   @npm@: A Node Package Manager (npm) package.
    --
    -- -   @pypi@: A Python Package Index (PyPI) package.
    --
    -- -   @maven@: A Maven package that contains compiled code in a
    --     distributable format, such as a JAR file.
    format :: PackageFormat,
    -- | The name of the package that is copied.
    package :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyPackageVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowOverwrite', 'copyPackageVersions_allowOverwrite' - Set to true to overwrite a package version that already exists in the
-- destination repository. If set to false and the package version already
-- exists in the destination repository, the package version is returned in
-- the @failedVersions@ field of the response with an @ALREADY_EXISTS@
-- error code.
--
-- 'versionRevisions', 'copyPackageVersions_versionRevisions' - A list of key-value pairs. The keys are package versions and the values
-- are package version revisions. A @CopyPackageVersion@ operation succeeds
-- if the specified versions in the source repository match the specified
-- package version revision.
--
-- You must specify @versions@ or @versionRevisions@. You cannot specify
-- both.
--
-- 'namespace', 'copyPackageVersions_namespace' - The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   A Python package does not contain a corresponding component, so
--     Python packages do not have a namespace.
--
-- 'domainOwner', 'copyPackageVersions_domainOwner' - The 12-digit account number of the AWS account that owns the domain. It
-- does not include dashes or spaces.
--
-- 'versions', 'copyPackageVersions_versions' - The versions of the package to copy.
--
-- You must specify @versions@ or @versionRevisions@. You cannot specify
-- both.
--
-- 'includeFromUpstream', 'copyPackageVersions_includeFromUpstream' - Set to true to copy packages from repositories that are upstream from
-- the source repository to the destination repository. The default setting
-- is false. For more information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/repos-upstream.html Working with upstream repositories>.
--
-- 'domain', 'copyPackageVersions_domain' - The name of the domain that contains the source and destination
-- repositories.
--
-- 'sourceRepository', 'copyPackageVersions_sourceRepository' - The name of the repository that contains the package versions to copy.
--
-- 'destinationRepository', 'copyPackageVersions_destinationRepository' - The name of the repository into which package versions are copied.
--
-- 'format', 'copyPackageVersions_format' - The format of the package that is copied. The valid package types are:
--
-- -   @npm@: A Node Package Manager (npm) package.
--
-- -   @pypi@: A Python Package Index (PyPI) package.
--
-- -   @maven@: A Maven package that contains compiled code in a
--     distributable format, such as a JAR file.
--
-- 'package', 'copyPackageVersions_package' - The name of the package that is copied.
newCopyPackageVersions ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'sourceRepository'
  Prelude.Text ->
  -- | 'destinationRepository'
  Prelude.Text ->
  -- | 'format'
  PackageFormat ->
  -- | 'package'
  Prelude.Text ->
  CopyPackageVersions
newCopyPackageVersions
  pDomain_
  pSourceRepository_
  pDestinationRepository_
  pFormat_
  pPackage_ =
    CopyPackageVersions'
      { allowOverwrite =
          Prelude.Nothing,
        versionRevisions = Prelude.Nothing,
        namespace = Prelude.Nothing,
        domainOwner = Prelude.Nothing,
        versions = Prelude.Nothing,
        includeFromUpstream = Prelude.Nothing,
        domain = pDomain_,
        sourceRepository = pSourceRepository_,
        destinationRepository = pDestinationRepository_,
        format = pFormat_,
        package = pPackage_
      }

-- | Set to true to overwrite a package version that already exists in the
-- destination repository. If set to false and the package version already
-- exists in the destination repository, the package version is returned in
-- the @failedVersions@ field of the response with an @ALREADY_EXISTS@
-- error code.
copyPackageVersions_allowOverwrite :: Lens.Lens' CopyPackageVersions (Prelude.Maybe Prelude.Bool)
copyPackageVersions_allowOverwrite = Lens.lens (\CopyPackageVersions' {allowOverwrite} -> allowOverwrite) (\s@CopyPackageVersions' {} a -> s {allowOverwrite = a} :: CopyPackageVersions)

-- | A list of key-value pairs. The keys are package versions and the values
-- are package version revisions. A @CopyPackageVersion@ operation succeeds
-- if the specified versions in the source repository match the specified
-- package version revision.
--
-- You must specify @versions@ or @versionRevisions@. You cannot specify
-- both.
copyPackageVersions_versionRevisions :: Lens.Lens' CopyPackageVersions (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
copyPackageVersions_versionRevisions = Lens.lens (\CopyPackageVersions' {versionRevisions} -> versionRevisions) (\s@CopyPackageVersions' {} a -> s {versionRevisions = a} :: CopyPackageVersions) Prelude.. Lens.mapping Lens.coerced

-- | The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   A Python package does not contain a corresponding component, so
--     Python packages do not have a namespace.
copyPackageVersions_namespace :: Lens.Lens' CopyPackageVersions (Prelude.Maybe Prelude.Text)
copyPackageVersions_namespace = Lens.lens (\CopyPackageVersions' {namespace} -> namespace) (\s@CopyPackageVersions' {} a -> s {namespace = a} :: CopyPackageVersions)

-- | The 12-digit account number of the AWS account that owns the domain. It
-- does not include dashes or spaces.
copyPackageVersions_domainOwner :: Lens.Lens' CopyPackageVersions (Prelude.Maybe Prelude.Text)
copyPackageVersions_domainOwner = Lens.lens (\CopyPackageVersions' {domainOwner} -> domainOwner) (\s@CopyPackageVersions' {} a -> s {domainOwner = a} :: CopyPackageVersions)

-- | The versions of the package to copy.
--
-- You must specify @versions@ or @versionRevisions@. You cannot specify
-- both.
copyPackageVersions_versions :: Lens.Lens' CopyPackageVersions (Prelude.Maybe [Prelude.Text])
copyPackageVersions_versions = Lens.lens (\CopyPackageVersions' {versions} -> versions) (\s@CopyPackageVersions' {} a -> s {versions = a} :: CopyPackageVersions) Prelude.. Lens.mapping Lens.coerced

-- | Set to true to copy packages from repositories that are upstream from
-- the source repository to the destination repository. The default setting
-- is false. For more information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/repos-upstream.html Working with upstream repositories>.
copyPackageVersions_includeFromUpstream :: Lens.Lens' CopyPackageVersions (Prelude.Maybe Prelude.Bool)
copyPackageVersions_includeFromUpstream = Lens.lens (\CopyPackageVersions' {includeFromUpstream} -> includeFromUpstream) (\s@CopyPackageVersions' {} a -> s {includeFromUpstream = a} :: CopyPackageVersions)

-- | The name of the domain that contains the source and destination
-- repositories.
copyPackageVersions_domain :: Lens.Lens' CopyPackageVersions Prelude.Text
copyPackageVersions_domain = Lens.lens (\CopyPackageVersions' {domain} -> domain) (\s@CopyPackageVersions' {} a -> s {domain = a} :: CopyPackageVersions)

-- | The name of the repository that contains the package versions to copy.
copyPackageVersions_sourceRepository :: Lens.Lens' CopyPackageVersions Prelude.Text
copyPackageVersions_sourceRepository = Lens.lens (\CopyPackageVersions' {sourceRepository} -> sourceRepository) (\s@CopyPackageVersions' {} a -> s {sourceRepository = a} :: CopyPackageVersions)

-- | The name of the repository into which package versions are copied.
copyPackageVersions_destinationRepository :: Lens.Lens' CopyPackageVersions Prelude.Text
copyPackageVersions_destinationRepository = Lens.lens (\CopyPackageVersions' {destinationRepository} -> destinationRepository) (\s@CopyPackageVersions' {} a -> s {destinationRepository = a} :: CopyPackageVersions)

-- | The format of the package that is copied. The valid package types are:
--
-- -   @npm@: A Node Package Manager (npm) package.
--
-- -   @pypi@: A Python Package Index (PyPI) package.
--
-- -   @maven@: A Maven package that contains compiled code in a
--     distributable format, such as a JAR file.
copyPackageVersions_format :: Lens.Lens' CopyPackageVersions PackageFormat
copyPackageVersions_format = Lens.lens (\CopyPackageVersions' {format} -> format) (\s@CopyPackageVersions' {} a -> s {format = a} :: CopyPackageVersions)

-- | The name of the package that is copied.
copyPackageVersions_package :: Lens.Lens' CopyPackageVersions Prelude.Text
copyPackageVersions_package = Lens.lens (\CopyPackageVersions' {package} -> package) (\s@CopyPackageVersions' {} a -> s {package = a} :: CopyPackageVersions)

instance Core.AWSRequest CopyPackageVersions where
  type
    AWSResponse CopyPackageVersions =
      CopyPackageVersionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CopyPackageVersionsResponse'
            Prelude.<$> (x Core..?> "failedVersions" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "successfulVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyPackageVersions where
  hashWithSalt _salt CopyPackageVersions' {..} =
    _salt `Prelude.hashWithSalt` allowOverwrite
      `Prelude.hashWithSalt` versionRevisions
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` versions
      `Prelude.hashWithSalt` includeFromUpstream
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` sourceRepository
      `Prelude.hashWithSalt` destinationRepository
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` package

instance Prelude.NFData CopyPackageVersions where
  rnf CopyPackageVersions' {..} =
    Prelude.rnf allowOverwrite
      `Prelude.seq` Prelude.rnf versionRevisions
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf versions
      `Prelude.seq` Prelude.rnf includeFromUpstream
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf sourceRepository
      `Prelude.seq` Prelude.rnf destinationRepository
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf package

instance Core.ToHeaders CopyPackageVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CopyPackageVersions where
  toJSON CopyPackageVersions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("allowOverwrite" Core..=)
              Prelude.<$> allowOverwrite,
            ("versionRevisions" Core..=)
              Prelude.<$> versionRevisions,
            ("versions" Core..=) Prelude.<$> versions,
            ("includeFromUpstream" Core..=)
              Prelude.<$> includeFromUpstream
          ]
      )

instance Core.ToPath CopyPackageVersions where
  toPath = Prelude.const "/v1/package/versions/copy"

instance Core.ToQuery CopyPackageVersions where
  toQuery CopyPackageVersions' {..} =
    Prelude.mconcat
      [ "namespace" Core.=: namespace,
        "domain-owner" Core.=: domainOwner,
        "domain" Core.=: domain,
        "source-repository" Core.=: sourceRepository,
        "destination-repository"
          Core.=: destinationRepository,
        "format" Core.=: format,
        "package" Core.=: package
      ]

-- | /See:/ 'newCopyPackageVersionsResponse' smart constructor.
data CopyPackageVersionsResponse = CopyPackageVersionsResponse'
  { -- | A map of package versions that failed to copy and their error codes. The
    -- possible error codes are in the @PackageVersionError@ data type. They
    -- are:
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
    -- | A list of the package versions that were successfully copied to your
    -- repository.
    successfulVersions :: Prelude.Maybe (Prelude.HashMap Prelude.Text SuccessfulPackageVersionInfo),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyPackageVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedVersions', 'copyPackageVersionsResponse_failedVersions' - A map of package versions that failed to copy and their error codes. The
-- possible error codes are in the @PackageVersionError@ data type. They
-- are:
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
-- 'successfulVersions', 'copyPackageVersionsResponse_successfulVersions' - A list of the package versions that were successfully copied to your
-- repository.
--
-- 'httpStatus', 'copyPackageVersionsResponse_httpStatus' - The response's http status code.
newCopyPackageVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CopyPackageVersionsResponse
newCopyPackageVersionsResponse pHttpStatus_ =
  CopyPackageVersionsResponse'
    { failedVersions =
        Prelude.Nothing,
      successfulVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map of package versions that failed to copy and their error codes. The
-- possible error codes are in the @PackageVersionError@ data type. They
-- are:
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
copyPackageVersionsResponse_failedVersions :: Lens.Lens' CopyPackageVersionsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text PackageVersionError))
copyPackageVersionsResponse_failedVersions = Lens.lens (\CopyPackageVersionsResponse' {failedVersions} -> failedVersions) (\s@CopyPackageVersionsResponse' {} a -> s {failedVersions = a} :: CopyPackageVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the package versions that were successfully copied to your
-- repository.
copyPackageVersionsResponse_successfulVersions :: Lens.Lens' CopyPackageVersionsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text SuccessfulPackageVersionInfo))
copyPackageVersionsResponse_successfulVersions = Lens.lens (\CopyPackageVersionsResponse' {successfulVersions} -> successfulVersions) (\s@CopyPackageVersionsResponse' {} a -> s {successfulVersions = a} :: CopyPackageVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
copyPackageVersionsResponse_httpStatus :: Lens.Lens' CopyPackageVersionsResponse Prelude.Int
copyPackageVersionsResponse_httpStatus = Lens.lens (\CopyPackageVersionsResponse' {httpStatus} -> httpStatus) (\s@CopyPackageVersionsResponse' {} a -> s {httpStatus = a} :: CopyPackageVersionsResponse)

instance Prelude.NFData CopyPackageVersionsResponse where
  rnf CopyPackageVersionsResponse' {..} =
    Prelude.rnf failedVersions
      `Prelude.seq` Prelude.rnf successfulVersions
      `Prelude.seq` Prelude.rnf httpStatus
