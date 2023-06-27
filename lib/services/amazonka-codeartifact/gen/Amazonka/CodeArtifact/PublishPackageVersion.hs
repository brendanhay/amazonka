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
-- Module      : Amazonka.CodeArtifact.PublishPackageVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new package version containing one or more assets (or files).
--
-- The @unfinished@ flag can be used to keep the package version in the
-- @Unfinished@ state until all of its assets have been uploaded (see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/packages-overview.html#package-version-status.html#package-version-status Package version status>
-- in the /CodeArtifact user guide/). To set the package version’s status
-- to @Published@, omit the @unfinished@ flag when uploading the final
-- asset, or set the status using
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_UpdatePackageVersionsStatus.html UpdatePackageVersionStatus>.
-- Once a package version’s status is set to @Published@, it cannot change
-- back to @Unfinished@.
--
-- Only generic packages can be published using this API. For more
-- information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/using-generic.html Using generic packages>
-- in the /CodeArtifact User Guide/.
module Amazonka.CodeArtifact.PublishPackageVersion
  ( -- * Creating a Request
    PublishPackageVersion (..),
    newPublishPackageVersion,

    -- * Request Lenses
    publishPackageVersion_domainOwner,
    publishPackageVersion_namespace,
    publishPackageVersion_unfinished,
    publishPackageVersion_domain,
    publishPackageVersion_repository,
    publishPackageVersion_format,
    publishPackageVersion_package,
    publishPackageVersion_packageVersion,
    publishPackageVersion_assetName,
    publishPackageVersion_assetSHA256,
    publishPackageVersion_assetContent,

    -- * Destructuring the Response
    PublishPackageVersionResponse (..),
    newPublishPackageVersionResponse,

    -- * Response Lenses
    publishPackageVersionResponse_asset,
    publishPackageVersionResponse_format,
    publishPackageVersionResponse_namespace,
    publishPackageVersionResponse_package,
    publishPackageVersionResponse_status,
    publishPackageVersionResponse_version,
    publishPackageVersionResponse_versionRevision,
    publishPackageVersionResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPublishPackageVersion' smart constructor.
data PublishPackageVersion = PublishPackageVersion'
  { -- | The 12-digit account number of the AWS account that owns the domain. It
    -- does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the package version to publish.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the package version should remain in the @unfinished@
    -- state. If omitted, the package version status will be set to @Published@
    -- (see
    -- <https://docs.aws.amazon.com/codeartifact/latest/ug/packages-overview.html#package-version-status Package version status>
    -- in the /CodeArtifact User Guide/).
    --
    -- Valid values: @unfinished@
    unfinished :: Prelude.Maybe Prelude.Bool,
    -- | The name of the domain that contains the repository that contains the
    -- package version to publish.
    domain :: Prelude.Text,
    -- | The name of the repository that the package version will be published
    -- to.
    repository :: Prelude.Text,
    -- | A format that specifies the type of the package version with the
    -- requested asset file.
    format :: PackageFormat,
    -- | The name of the package version to publish.
    package :: Prelude.Text,
    -- | The package version to publish (for example, @3.5.2@).
    packageVersion :: Prelude.Text,
    -- | The name of the asset to publish. Asset names can include Unicode
    -- letters and numbers, and the following special characters:
    -- @~ ! \@ ^ & ( ) - \` _ + [ ] { } ; , . \`@
    assetName :: Prelude.Text,
    -- | The SHA256 hash of the @assetContent@ to publish. This value must be
    -- calculated by the caller and provided with the request (see
    -- <https://docs.aws.amazon.com/codeartifact/latest/ug/using-generic.html#publishing-generic-packages Publishing a generic package>
    -- in the /CodeArtifact User Guide/).
    --
    -- This value is used as an integrity check to verify that the
    -- @assetContent@ has not changed after it was originally sent.
    assetSHA256 :: Prelude.Text,
    -- | The content of the asset to publish.
    assetContent :: Data.HashedBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishPackageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'publishPackageVersion_domainOwner' - The 12-digit account number of the AWS account that owns the domain. It
-- does not include dashes or spaces.
--
-- 'namespace', 'publishPackageVersion_namespace' - The namespace of the package version to publish.
--
-- 'unfinished', 'publishPackageVersion_unfinished' - Specifies whether the package version should remain in the @unfinished@
-- state. If omitted, the package version status will be set to @Published@
-- (see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/packages-overview.html#package-version-status Package version status>
-- in the /CodeArtifact User Guide/).
--
-- Valid values: @unfinished@
--
-- 'domain', 'publishPackageVersion_domain' - The name of the domain that contains the repository that contains the
-- package version to publish.
--
-- 'repository', 'publishPackageVersion_repository' - The name of the repository that the package version will be published
-- to.
--
-- 'format', 'publishPackageVersion_format' - A format that specifies the type of the package version with the
-- requested asset file.
--
-- 'package', 'publishPackageVersion_package' - The name of the package version to publish.
--
-- 'packageVersion', 'publishPackageVersion_packageVersion' - The package version to publish (for example, @3.5.2@).
--
-- 'assetName', 'publishPackageVersion_assetName' - The name of the asset to publish. Asset names can include Unicode
-- letters and numbers, and the following special characters:
-- @~ ! \@ ^ & ( ) - \` _ + [ ] { } ; , . \`@
--
-- 'assetSHA256', 'publishPackageVersion_assetSHA256' - The SHA256 hash of the @assetContent@ to publish. This value must be
-- calculated by the caller and provided with the request (see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/using-generic.html#publishing-generic-packages Publishing a generic package>
-- in the /CodeArtifact User Guide/).
--
-- This value is used as an integrity check to verify that the
-- @assetContent@ has not changed after it was originally sent.
--
-- 'assetContent', 'publishPackageVersion_assetContent' - The content of the asset to publish.
newPublishPackageVersion ::
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
  -- | 'assetName'
  Prelude.Text ->
  -- | 'assetSHA256'
  Prelude.Text ->
  -- | 'assetContent'
  Data.HashedBody ->
  PublishPackageVersion
newPublishPackageVersion
  pDomain_
  pRepository_
  pFormat_
  pPackage_
  pPackageVersion_
  pAssetName_
  pAssetSHA256_
  pAssetContent_ =
    PublishPackageVersion'
      { domainOwner =
          Prelude.Nothing,
        namespace = Prelude.Nothing,
        unfinished = Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        format = pFormat_,
        package = pPackage_,
        packageVersion = pPackageVersion_,
        assetName = pAssetName_,
        assetSHA256 = pAssetSHA256_,
        assetContent = pAssetContent_
      }

-- | The 12-digit account number of the AWS account that owns the domain. It
-- does not include dashes or spaces.
publishPackageVersion_domainOwner :: Lens.Lens' PublishPackageVersion (Prelude.Maybe Prelude.Text)
publishPackageVersion_domainOwner = Lens.lens (\PublishPackageVersion' {domainOwner} -> domainOwner) (\s@PublishPackageVersion' {} a -> s {domainOwner = a} :: PublishPackageVersion)

-- | The namespace of the package version to publish.
publishPackageVersion_namespace :: Lens.Lens' PublishPackageVersion (Prelude.Maybe Prelude.Text)
publishPackageVersion_namespace = Lens.lens (\PublishPackageVersion' {namespace} -> namespace) (\s@PublishPackageVersion' {} a -> s {namespace = a} :: PublishPackageVersion)

-- | Specifies whether the package version should remain in the @unfinished@
-- state. If omitted, the package version status will be set to @Published@
-- (see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/packages-overview.html#package-version-status Package version status>
-- in the /CodeArtifact User Guide/).
--
-- Valid values: @unfinished@
publishPackageVersion_unfinished :: Lens.Lens' PublishPackageVersion (Prelude.Maybe Prelude.Bool)
publishPackageVersion_unfinished = Lens.lens (\PublishPackageVersion' {unfinished} -> unfinished) (\s@PublishPackageVersion' {} a -> s {unfinished = a} :: PublishPackageVersion)

-- | The name of the domain that contains the repository that contains the
-- package version to publish.
publishPackageVersion_domain :: Lens.Lens' PublishPackageVersion Prelude.Text
publishPackageVersion_domain = Lens.lens (\PublishPackageVersion' {domain} -> domain) (\s@PublishPackageVersion' {} a -> s {domain = a} :: PublishPackageVersion)

-- | The name of the repository that the package version will be published
-- to.
publishPackageVersion_repository :: Lens.Lens' PublishPackageVersion Prelude.Text
publishPackageVersion_repository = Lens.lens (\PublishPackageVersion' {repository} -> repository) (\s@PublishPackageVersion' {} a -> s {repository = a} :: PublishPackageVersion)

-- | A format that specifies the type of the package version with the
-- requested asset file.
publishPackageVersion_format :: Lens.Lens' PublishPackageVersion PackageFormat
publishPackageVersion_format = Lens.lens (\PublishPackageVersion' {format} -> format) (\s@PublishPackageVersion' {} a -> s {format = a} :: PublishPackageVersion)

-- | The name of the package version to publish.
publishPackageVersion_package :: Lens.Lens' PublishPackageVersion Prelude.Text
publishPackageVersion_package = Lens.lens (\PublishPackageVersion' {package} -> package) (\s@PublishPackageVersion' {} a -> s {package = a} :: PublishPackageVersion)

-- | The package version to publish (for example, @3.5.2@).
publishPackageVersion_packageVersion :: Lens.Lens' PublishPackageVersion Prelude.Text
publishPackageVersion_packageVersion = Lens.lens (\PublishPackageVersion' {packageVersion} -> packageVersion) (\s@PublishPackageVersion' {} a -> s {packageVersion = a} :: PublishPackageVersion)

-- | The name of the asset to publish. Asset names can include Unicode
-- letters and numbers, and the following special characters:
-- @~ ! \@ ^ & ( ) - \` _ + [ ] { } ; , . \`@
publishPackageVersion_assetName :: Lens.Lens' PublishPackageVersion Prelude.Text
publishPackageVersion_assetName = Lens.lens (\PublishPackageVersion' {assetName} -> assetName) (\s@PublishPackageVersion' {} a -> s {assetName = a} :: PublishPackageVersion)

-- | The SHA256 hash of the @assetContent@ to publish. This value must be
-- calculated by the caller and provided with the request (see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/using-generic.html#publishing-generic-packages Publishing a generic package>
-- in the /CodeArtifact User Guide/).
--
-- This value is used as an integrity check to verify that the
-- @assetContent@ has not changed after it was originally sent.
publishPackageVersion_assetSHA256 :: Lens.Lens' PublishPackageVersion Prelude.Text
publishPackageVersion_assetSHA256 = Lens.lens (\PublishPackageVersion' {assetSHA256} -> assetSHA256) (\s@PublishPackageVersion' {} a -> s {assetSHA256 = a} :: PublishPackageVersion)

-- | The content of the asset to publish.
publishPackageVersion_assetContent :: Lens.Lens' PublishPackageVersion Data.HashedBody
publishPackageVersion_assetContent = Lens.lens (\PublishPackageVersion' {assetContent} -> assetContent) (\s@PublishPackageVersion' {} a -> s {assetContent = a} :: PublishPackageVersion)

instance Core.AWSRequest PublishPackageVersion where
  type
    AWSResponse PublishPackageVersion =
      PublishPackageVersionResponse
  request overrides =
    Request.postBody (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PublishPackageVersionResponse'
            Prelude.<$> (x Data..?> "asset")
            Prelude.<*> (x Data..?> "format")
            Prelude.<*> (x Data..?> "namespace")
            Prelude.<*> (x Data..?> "package")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (x Data..?> "versionRevision")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Data.ToBody PublishPackageVersion where
  toBody PublishPackageVersion' {..} =
    Data.toBody assetContent

instance Data.ToHeaders PublishPackageVersion where
  toHeaders PublishPackageVersion' {..} =
    Prelude.mconcat
      [ "x-amz-content-sha256" Data.=# assetSHA256,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath PublishPackageVersion where
  toPath = Prelude.const "/v1/package/version/publish"

instance Data.ToQuery PublishPackageVersion where
  toQuery PublishPackageVersion' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "namespace" Data.=: namespace,
        "unfinished" Data.=: unfinished,
        "domain" Data.=: domain,
        "repository" Data.=: repository,
        "format" Data.=: format,
        "package" Data.=: package,
        "version" Data.=: packageVersion,
        "asset" Data.=: assetName
      ]

-- | /See:/ 'newPublishPackageVersionResponse' smart constructor.
data PublishPackageVersionResponse = PublishPackageVersionResponse'
  { -- | An
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_AssetSummary.html AssetSummary>
    -- for the published asset.
    asset :: Prelude.Maybe AssetSummary,
    -- | The format of the package version.
    format :: Prelude.Maybe PackageFormat,
    -- | The namespace of the package version.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The name of the package.
    package :: Prelude.Maybe Prelude.Text,
    -- | A string that contains the status of the package version. For more
    -- information, see
    -- <https://docs.aws.amazon.com/codeartifact/latest/ug/packages-overview.html#package-version-status.html#package-version-status Package version status>
    -- in the /CodeArtifact User Guide/.
    status :: Prelude.Maybe PackageVersionStatus,
    -- | The version of the package.
    version :: Prelude.Maybe Prelude.Text,
    -- | The revision of the package version.
    versionRevision :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishPackageVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'asset', 'publishPackageVersionResponse_asset' - An
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_AssetSummary.html AssetSummary>
-- for the published asset.
--
-- 'format', 'publishPackageVersionResponse_format' - The format of the package version.
--
-- 'namespace', 'publishPackageVersionResponse_namespace' - The namespace of the package version.
--
-- 'package', 'publishPackageVersionResponse_package' - The name of the package.
--
-- 'status', 'publishPackageVersionResponse_status' - A string that contains the status of the package version. For more
-- information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/packages-overview.html#package-version-status.html#package-version-status Package version status>
-- in the /CodeArtifact User Guide/.
--
-- 'version', 'publishPackageVersionResponse_version' - The version of the package.
--
-- 'versionRevision', 'publishPackageVersionResponse_versionRevision' - The revision of the package version.
--
-- 'httpStatus', 'publishPackageVersionResponse_httpStatus' - The response's http status code.
newPublishPackageVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PublishPackageVersionResponse
newPublishPackageVersionResponse pHttpStatus_ =
  PublishPackageVersionResponse'
    { asset =
        Prelude.Nothing,
      format = Prelude.Nothing,
      namespace = Prelude.Nothing,
      package = Prelude.Nothing,
      status = Prelude.Nothing,
      version = Prelude.Nothing,
      versionRevision = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_AssetSummary.html AssetSummary>
-- for the published asset.
publishPackageVersionResponse_asset :: Lens.Lens' PublishPackageVersionResponse (Prelude.Maybe AssetSummary)
publishPackageVersionResponse_asset = Lens.lens (\PublishPackageVersionResponse' {asset} -> asset) (\s@PublishPackageVersionResponse' {} a -> s {asset = a} :: PublishPackageVersionResponse)

-- | The format of the package version.
publishPackageVersionResponse_format :: Lens.Lens' PublishPackageVersionResponse (Prelude.Maybe PackageFormat)
publishPackageVersionResponse_format = Lens.lens (\PublishPackageVersionResponse' {format} -> format) (\s@PublishPackageVersionResponse' {} a -> s {format = a} :: PublishPackageVersionResponse)

-- | The namespace of the package version.
publishPackageVersionResponse_namespace :: Lens.Lens' PublishPackageVersionResponse (Prelude.Maybe Prelude.Text)
publishPackageVersionResponse_namespace = Lens.lens (\PublishPackageVersionResponse' {namespace} -> namespace) (\s@PublishPackageVersionResponse' {} a -> s {namespace = a} :: PublishPackageVersionResponse)

-- | The name of the package.
publishPackageVersionResponse_package :: Lens.Lens' PublishPackageVersionResponse (Prelude.Maybe Prelude.Text)
publishPackageVersionResponse_package = Lens.lens (\PublishPackageVersionResponse' {package} -> package) (\s@PublishPackageVersionResponse' {} a -> s {package = a} :: PublishPackageVersionResponse)

-- | A string that contains the status of the package version. For more
-- information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/packages-overview.html#package-version-status.html#package-version-status Package version status>
-- in the /CodeArtifact User Guide/.
publishPackageVersionResponse_status :: Lens.Lens' PublishPackageVersionResponse (Prelude.Maybe PackageVersionStatus)
publishPackageVersionResponse_status = Lens.lens (\PublishPackageVersionResponse' {status} -> status) (\s@PublishPackageVersionResponse' {} a -> s {status = a} :: PublishPackageVersionResponse)

-- | The version of the package.
publishPackageVersionResponse_version :: Lens.Lens' PublishPackageVersionResponse (Prelude.Maybe Prelude.Text)
publishPackageVersionResponse_version = Lens.lens (\PublishPackageVersionResponse' {version} -> version) (\s@PublishPackageVersionResponse' {} a -> s {version = a} :: PublishPackageVersionResponse)

-- | The revision of the package version.
publishPackageVersionResponse_versionRevision :: Lens.Lens' PublishPackageVersionResponse (Prelude.Maybe Prelude.Text)
publishPackageVersionResponse_versionRevision = Lens.lens (\PublishPackageVersionResponse' {versionRevision} -> versionRevision) (\s@PublishPackageVersionResponse' {} a -> s {versionRevision = a} :: PublishPackageVersionResponse)

-- | The response's http status code.
publishPackageVersionResponse_httpStatus :: Lens.Lens' PublishPackageVersionResponse Prelude.Int
publishPackageVersionResponse_httpStatus = Lens.lens (\PublishPackageVersionResponse' {httpStatus} -> httpStatus) (\s@PublishPackageVersionResponse' {} a -> s {httpStatus = a} :: PublishPackageVersionResponse)

instance Prelude.NFData PublishPackageVersionResponse where
  rnf PublishPackageVersionResponse' {..} =
    Prelude.rnf asset
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf package
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf versionRevision
      `Prelude.seq` Prelude.rnf httpStatus
