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
-- Module      : Amazonka.CodeArtifact.DescribePackageVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageVersionDescription.html PackageVersionDescription>
-- object that contains information about the requested package version.
module Amazonka.CodeArtifact.DescribePackageVersion
  ( -- * Creating a Request
    DescribePackageVersion (..),
    newDescribePackageVersion,

    -- * Request Lenses
    describePackageVersion_domainOwner,
    describePackageVersion_namespace,
    describePackageVersion_domain,
    describePackageVersion_repository,
    describePackageVersion_format,
    describePackageVersion_package,
    describePackageVersion_packageVersion,

    -- * Destructuring the Response
    DescribePackageVersionResponse (..),
    newDescribePackageVersionResponse,

    -- * Response Lenses
    describePackageVersionResponse_httpStatus,
    describePackageVersionResponse_packageVersion,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePackageVersion' smart constructor.
data DescribePackageVersion = DescribePackageVersion'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the requested package version. The package version
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
    -- | The name of the domain that contains the repository that contains the
    -- package version.
    domain :: Prelude.Text,
    -- | The name of the repository that contains the package version.
    repository :: Prelude.Text,
    -- | A format that specifies the type of the requested package version.
    format :: PackageFormat,
    -- | The name of the requested package version.
    package :: Prelude.Text,
    -- | A string that contains the package version (for example, @3.5.2@).
    packageVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePackageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'describePackageVersion_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'namespace', 'describePackageVersion_namespace' - The namespace of the requested package version. The package version
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
-- 'domain', 'describePackageVersion_domain' - The name of the domain that contains the repository that contains the
-- package version.
--
-- 'repository', 'describePackageVersion_repository' - The name of the repository that contains the package version.
--
-- 'format', 'describePackageVersion_format' - A format that specifies the type of the requested package version.
--
-- 'package', 'describePackageVersion_package' - The name of the requested package version.
--
-- 'packageVersion', 'describePackageVersion_packageVersion' - A string that contains the package version (for example, @3.5.2@).
newDescribePackageVersion ::
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
  DescribePackageVersion
newDescribePackageVersion
  pDomain_
  pRepository_
  pFormat_
  pPackage_
  pPackageVersion_ =
    DescribePackageVersion'
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
describePackageVersion_domainOwner :: Lens.Lens' DescribePackageVersion (Prelude.Maybe Prelude.Text)
describePackageVersion_domainOwner = Lens.lens (\DescribePackageVersion' {domainOwner} -> domainOwner) (\s@DescribePackageVersion' {} a -> s {domainOwner = a} :: DescribePackageVersion)

-- | The namespace of the requested package version. The package version
-- component that specifies its namespace depends on its type. For example:
--
-- -   The namespace of a Maven package version is its @groupId@.
--
-- -   The namespace of an npm package version is its @scope@.
--
-- -   Python and NuGet package versions do not contain a corresponding
--     component, package versions of those formats do not have a
--     namespace.
describePackageVersion_namespace :: Lens.Lens' DescribePackageVersion (Prelude.Maybe Prelude.Text)
describePackageVersion_namespace = Lens.lens (\DescribePackageVersion' {namespace} -> namespace) (\s@DescribePackageVersion' {} a -> s {namespace = a} :: DescribePackageVersion)

-- | The name of the domain that contains the repository that contains the
-- package version.
describePackageVersion_domain :: Lens.Lens' DescribePackageVersion Prelude.Text
describePackageVersion_domain = Lens.lens (\DescribePackageVersion' {domain} -> domain) (\s@DescribePackageVersion' {} a -> s {domain = a} :: DescribePackageVersion)

-- | The name of the repository that contains the package version.
describePackageVersion_repository :: Lens.Lens' DescribePackageVersion Prelude.Text
describePackageVersion_repository = Lens.lens (\DescribePackageVersion' {repository} -> repository) (\s@DescribePackageVersion' {} a -> s {repository = a} :: DescribePackageVersion)

-- | A format that specifies the type of the requested package version.
describePackageVersion_format :: Lens.Lens' DescribePackageVersion PackageFormat
describePackageVersion_format = Lens.lens (\DescribePackageVersion' {format} -> format) (\s@DescribePackageVersion' {} a -> s {format = a} :: DescribePackageVersion)

-- | The name of the requested package version.
describePackageVersion_package :: Lens.Lens' DescribePackageVersion Prelude.Text
describePackageVersion_package = Lens.lens (\DescribePackageVersion' {package} -> package) (\s@DescribePackageVersion' {} a -> s {package = a} :: DescribePackageVersion)

-- | A string that contains the package version (for example, @3.5.2@).
describePackageVersion_packageVersion :: Lens.Lens' DescribePackageVersion Prelude.Text
describePackageVersion_packageVersion = Lens.lens (\DescribePackageVersion' {packageVersion} -> packageVersion) (\s@DescribePackageVersion' {} a -> s {packageVersion = a} :: DescribePackageVersion)

instance Core.AWSRequest DescribePackageVersion where
  type
    AWSResponse DescribePackageVersion =
      DescribePackageVersionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePackageVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "packageVersion")
      )

instance Prelude.Hashable DescribePackageVersion where
  hashWithSalt _salt DescribePackageVersion' {..} =
    _salt
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` package
      `Prelude.hashWithSalt` packageVersion

instance Prelude.NFData DescribePackageVersion where
  rnf DescribePackageVersion' {..} =
    Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf package
      `Prelude.seq` Prelude.rnf packageVersion

instance Data.ToHeaders DescribePackageVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribePackageVersion where
  toPath = Prelude.const "/v1/package/version"

instance Data.ToQuery DescribePackageVersion where
  toQuery DescribePackageVersion' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "namespace" Data.=: namespace,
        "domain" Data.=: domain,
        "repository" Data.=: repository,
        "format" Data.=: format,
        "package" Data.=: package,
        "version" Data.=: packageVersion
      ]

-- | /See:/ 'newDescribePackageVersionResponse' smart constructor.
data DescribePackageVersionResponse = DescribePackageVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageVersionDescription.html PackageVersionDescription>
    -- object that contains information about the requested package version.
    packageVersion :: PackageVersionDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePackageVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describePackageVersionResponse_httpStatus' - The response's http status code.
--
-- 'packageVersion', 'describePackageVersionResponse_packageVersion' - A
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageVersionDescription.html PackageVersionDescription>
-- object that contains information about the requested package version.
newDescribePackageVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'packageVersion'
  PackageVersionDescription ->
  DescribePackageVersionResponse
newDescribePackageVersionResponse
  pHttpStatus_
  pPackageVersion_ =
    DescribePackageVersionResponse'
      { httpStatus =
          pHttpStatus_,
        packageVersion = pPackageVersion_
      }

-- | The response's http status code.
describePackageVersionResponse_httpStatus :: Lens.Lens' DescribePackageVersionResponse Prelude.Int
describePackageVersionResponse_httpStatus = Lens.lens (\DescribePackageVersionResponse' {httpStatus} -> httpStatus) (\s@DescribePackageVersionResponse' {} a -> s {httpStatus = a} :: DescribePackageVersionResponse)

-- | A
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageVersionDescription.html PackageVersionDescription>
-- object that contains information about the requested package version.
describePackageVersionResponse_packageVersion :: Lens.Lens' DescribePackageVersionResponse PackageVersionDescription
describePackageVersionResponse_packageVersion = Lens.lens (\DescribePackageVersionResponse' {packageVersion} -> packageVersion) (\s@DescribePackageVersionResponse' {} a -> s {packageVersion = a} :: DescribePackageVersionResponse)

instance
  Prelude.NFData
    DescribePackageVersionResponse
  where
  rnf DescribePackageVersionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf packageVersion
