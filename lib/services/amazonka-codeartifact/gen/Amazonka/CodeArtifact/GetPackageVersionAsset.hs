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
-- Module      : Amazonka.CodeArtifact.GetPackageVersionAsset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an asset (or file) that is in a package. For example, for a
-- Maven package version, use @GetPackageVersionAsset@ to download a @JAR@
-- file, a @POM@ file, or any other assets in the package version.
module Amazonka.CodeArtifact.GetPackageVersionAsset
  ( -- * Creating a Request
    GetPackageVersionAsset (..),
    newGetPackageVersionAsset,

    -- * Request Lenses
    getPackageVersionAsset_domainOwner,
    getPackageVersionAsset_namespace,
    getPackageVersionAsset_packageVersionRevision,
    getPackageVersionAsset_domain,
    getPackageVersionAsset_repository,
    getPackageVersionAsset_format,
    getPackageVersionAsset_package,
    getPackageVersionAsset_packageVersion,
    getPackageVersionAsset_asset,

    -- * Destructuring the Response
    GetPackageVersionAssetResponse (..),
    newGetPackageVersionAssetResponse,

    -- * Response Lenses
    getPackageVersionAssetResponse_assetName,
    getPackageVersionAssetResponse_packageVersion,
    getPackageVersionAssetResponse_packageVersionRevision,
    getPackageVersionAssetResponse_httpStatus,
    getPackageVersionAssetResponse_asset,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPackageVersionAsset' smart constructor.
data GetPackageVersionAsset = GetPackageVersionAsset'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the package version with the requested asset file. The
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
    -- | The name of the package version revision that contains the requested
    -- asset.
    packageVersionRevision :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the repository that contains the
    -- package version with the requested asset.
    domain :: Prelude.Text,
    -- | The repository that contains the package version with the requested
    -- asset.
    repository :: Prelude.Text,
    -- | A format that specifies the type of the package version with the
    -- requested asset file.
    format :: PackageFormat,
    -- | The name of the package that contains the requested asset.
    package :: Prelude.Text,
    -- | A string that contains the package version (for example, @3.5.2@).
    packageVersion :: Prelude.Text,
    -- | The name of the requested asset.
    asset :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPackageVersionAsset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'getPackageVersionAsset_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'namespace', 'getPackageVersionAsset_namespace' - The namespace of the package version with the requested asset file. The
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
-- 'packageVersionRevision', 'getPackageVersionAsset_packageVersionRevision' - The name of the package version revision that contains the requested
-- asset.
--
-- 'domain', 'getPackageVersionAsset_domain' - The name of the domain that contains the repository that contains the
-- package version with the requested asset.
--
-- 'repository', 'getPackageVersionAsset_repository' - The repository that contains the package version with the requested
-- asset.
--
-- 'format', 'getPackageVersionAsset_format' - A format that specifies the type of the package version with the
-- requested asset file.
--
-- 'package', 'getPackageVersionAsset_package' - The name of the package that contains the requested asset.
--
-- 'packageVersion', 'getPackageVersionAsset_packageVersion' - A string that contains the package version (for example, @3.5.2@).
--
-- 'asset', 'getPackageVersionAsset_asset' - The name of the requested asset.
newGetPackageVersionAsset ::
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
  -- | 'asset'
  Prelude.Text ->
  GetPackageVersionAsset
newGetPackageVersionAsset
  pDomain_
  pRepository_
  pFormat_
  pPackage_
  pPackageVersion_
  pAsset_ =
    GetPackageVersionAsset'
      { domainOwner =
          Prelude.Nothing,
        namespace = Prelude.Nothing,
        packageVersionRevision = Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        format = pFormat_,
        package = pPackage_,
        packageVersion = pPackageVersion_,
        asset = pAsset_
      }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
getPackageVersionAsset_domainOwner :: Lens.Lens' GetPackageVersionAsset (Prelude.Maybe Prelude.Text)
getPackageVersionAsset_domainOwner = Lens.lens (\GetPackageVersionAsset' {domainOwner} -> domainOwner) (\s@GetPackageVersionAsset' {} a -> s {domainOwner = a} :: GetPackageVersionAsset)

-- | The namespace of the package version with the requested asset file. The
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
getPackageVersionAsset_namespace :: Lens.Lens' GetPackageVersionAsset (Prelude.Maybe Prelude.Text)
getPackageVersionAsset_namespace = Lens.lens (\GetPackageVersionAsset' {namespace} -> namespace) (\s@GetPackageVersionAsset' {} a -> s {namespace = a} :: GetPackageVersionAsset)

-- | The name of the package version revision that contains the requested
-- asset.
getPackageVersionAsset_packageVersionRevision :: Lens.Lens' GetPackageVersionAsset (Prelude.Maybe Prelude.Text)
getPackageVersionAsset_packageVersionRevision = Lens.lens (\GetPackageVersionAsset' {packageVersionRevision} -> packageVersionRevision) (\s@GetPackageVersionAsset' {} a -> s {packageVersionRevision = a} :: GetPackageVersionAsset)

-- | The name of the domain that contains the repository that contains the
-- package version with the requested asset.
getPackageVersionAsset_domain :: Lens.Lens' GetPackageVersionAsset Prelude.Text
getPackageVersionAsset_domain = Lens.lens (\GetPackageVersionAsset' {domain} -> domain) (\s@GetPackageVersionAsset' {} a -> s {domain = a} :: GetPackageVersionAsset)

-- | The repository that contains the package version with the requested
-- asset.
getPackageVersionAsset_repository :: Lens.Lens' GetPackageVersionAsset Prelude.Text
getPackageVersionAsset_repository = Lens.lens (\GetPackageVersionAsset' {repository} -> repository) (\s@GetPackageVersionAsset' {} a -> s {repository = a} :: GetPackageVersionAsset)

-- | A format that specifies the type of the package version with the
-- requested asset file.
getPackageVersionAsset_format :: Lens.Lens' GetPackageVersionAsset PackageFormat
getPackageVersionAsset_format = Lens.lens (\GetPackageVersionAsset' {format} -> format) (\s@GetPackageVersionAsset' {} a -> s {format = a} :: GetPackageVersionAsset)

-- | The name of the package that contains the requested asset.
getPackageVersionAsset_package :: Lens.Lens' GetPackageVersionAsset Prelude.Text
getPackageVersionAsset_package = Lens.lens (\GetPackageVersionAsset' {package} -> package) (\s@GetPackageVersionAsset' {} a -> s {package = a} :: GetPackageVersionAsset)

-- | A string that contains the package version (for example, @3.5.2@).
getPackageVersionAsset_packageVersion :: Lens.Lens' GetPackageVersionAsset Prelude.Text
getPackageVersionAsset_packageVersion = Lens.lens (\GetPackageVersionAsset' {packageVersion} -> packageVersion) (\s@GetPackageVersionAsset' {} a -> s {packageVersion = a} :: GetPackageVersionAsset)

-- | The name of the requested asset.
getPackageVersionAsset_asset :: Lens.Lens' GetPackageVersionAsset Prelude.Text
getPackageVersionAsset_asset = Lens.lens (\GetPackageVersionAsset' {asset} -> asset) (\s@GetPackageVersionAsset' {} a -> s {asset = a} :: GetPackageVersionAsset)

instance Core.AWSRequest GetPackageVersionAsset where
  type
    AWSResponse GetPackageVersionAsset =
      GetPackageVersionAssetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBody
      ( \s h x ->
          GetPackageVersionAssetResponse'
            Prelude.<$> (h Data..#? "X-AssetName")
            Prelude.<*> (h Data..#? "X-PackageVersion")
            Prelude.<*> (h Data..#? "X-PackageVersionRevision")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetPackageVersionAsset where
  hashWithSalt _salt GetPackageVersionAsset' {..} =
    _salt
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` packageVersionRevision
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` package
      `Prelude.hashWithSalt` packageVersion
      `Prelude.hashWithSalt` asset

instance Prelude.NFData GetPackageVersionAsset where
  rnf GetPackageVersionAsset' {..} =
    Prelude.rnf domainOwner `Prelude.seq`
      Prelude.rnf namespace `Prelude.seq`
        Prelude.rnf packageVersionRevision `Prelude.seq`
          Prelude.rnf domain `Prelude.seq`
            Prelude.rnf repository `Prelude.seq`
              Prelude.rnf format `Prelude.seq`
                Prelude.rnf package `Prelude.seq`
                  Prelude.rnf packageVersion `Prelude.seq`
                    Prelude.rnf asset

instance Data.ToHeaders GetPackageVersionAsset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetPackageVersionAsset where
  toPath = Prelude.const "/v1/package/version/asset"

instance Data.ToQuery GetPackageVersionAsset where
  toQuery GetPackageVersionAsset' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "namespace" Data.=: namespace,
        "revision" Data.=: packageVersionRevision,
        "domain" Data.=: domain,
        "repository" Data.=: repository,
        "format" Data.=: format,
        "package" Data.=: package,
        "version" Data.=: packageVersion,
        "asset" Data.=: asset
      ]

-- | /See:/ 'newGetPackageVersionAssetResponse' smart constructor.
data GetPackageVersionAssetResponse = GetPackageVersionAssetResponse'
  { -- | The name of the asset that is downloaded.
    assetName :: Prelude.Maybe Prelude.Text,
    -- | A string that contains the package version (for example, @3.5.2@).
    packageVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the package version revision that contains the downloaded
    -- asset.
    packageVersionRevision :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The binary file, or asset, that is downloaded.
    asset :: Data.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPackageVersionAssetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetName', 'getPackageVersionAssetResponse_assetName' - The name of the asset that is downloaded.
--
-- 'packageVersion', 'getPackageVersionAssetResponse_packageVersion' - A string that contains the package version (for example, @3.5.2@).
--
-- 'packageVersionRevision', 'getPackageVersionAssetResponse_packageVersionRevision' - The name of the package version revision that contains the downloaded
-- asset.
--
-- 'httpStatus', 'getPackageVersionAssetResponse_httpStatus' - The response's http status code.
--
-- 'asset', 'getPackageVersionAssetResponse_asset' - The binary file, or asset, that is downloaded.
newGetPackageVersionAssetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'asset'
  Data.ResponseBody ->
  GetPackageVersionAssetResponse
newGetPackageVersionAssetResponse
  pHttpStatus_
  pAsset_ =
    GetPackageVersionAssetResponse'
      { assetName =
          Prelude.Nothing,
        packageVersion = Prelude.Nothing,
        packageVersionRevision = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        asset = pAsset_
      }

-- | The name of the asset that is downloaded.
getPackageVersionAssetResponse_assetName :: Lens.Lens' GetPackageVersionAssetResponse (Prelude.Maybe Prelude.Text)
getPackageVersionAssetResponse_assetName = Lens.lens (\GetPackageVersionAssetResponse' {assetName} -> assetName) (\s@GetPackageVersionAssetResponse' {} a -> s {assetName = a} :: GetPackageVersionAssetResponse)

-- | A string that contains the package version (for example, @3.5.2@).
getPackageVersionAssetResponse_packageVersion :: Lens.Lens' GetPackageVersionAssetResponse (Prelude.Maybe Prelude.Text)
getPackageVersionAssetResponse_packageVersion = Lens.lens (\GetPackageVersionAssetResponse' {packageVersion} -> packageVersion) (\s@GetPackageVersionAssetResponse' {} a -> s {packageVersion = a} :: GetPackageVersionAssetResponse)

-- | The name of the package version revision that contains the downloaded
-- asset.
getPackageVersionAssetResponse_packageVersionRevision :: Lens.Lens' GetPackageVersionAssetResponse (Prelude.Maybe Prelude.Text)
getPackageVersionAssetResponse_packageVersionRevision = Lens.lens (\GetPackageVersionAssetResponse' {packageVersionRevision} -> packageVersionRevision) (\s@GetPackageVersionAssetResponse' {} a -> s {packageVersionRevision = a} :: GetPackageVersionAssetResponse)

-- | The response's http status code.
getPackageVersionAssetResponse_httpStatus :: Lens.Lens' GetPackageVersionAssetResponse Prelude.Int
getPackageVersionAssetResponse_httpStatus = Lens.lens (\GetPackageVersionAssetResponse' {httpStatus} -> httpStatus) (\s@GetPackageVersionAssetResponse' {} a -> s {httpStatus = a} :: GetPackageVersionAssetResponse)

-- | The binary file, or asset, that is downloaded.
getPackageVersionAssetResponse_asset :: Lens.Lens' GetPackageVersionAssetResponse Data.ResponseBody
getPackageVersionAssetResponse_asset = Lens.lens (\GetPackageVersionAssetResponse' {asset} -> asset) (\s@GetPackageVersionAssetResponse' {} a -> s {asset = a} :: GetPackageVersionAssetResponse)
