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
-- Module      : Amazonka.CodeArtifact.DescribePackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageDescription.html PackageDescription>
-- object that contains information about the requested package.
module Amazonka.CodeArtifact.DescribePackage
  ( -- * Creating a Request
    DescribePackage (..),
    newDescribePackage,

    -- * Request Lenses
    describePackage_domainOwner,
    describePackage_namespace,
    describePackage_domain,
    describePackage_repository,
    describePackage_format,
    describePackage_package,

    -- * Destructuring the Response
    DescribePackageResponse (..),
    newDescribePackageResponse,

    -- * Response Lenses
    describePackageResponse_httpStatus,
    describePackageResponse_package,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePackage' smart constructor.
data DescribePackage = DescribePackage'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the requested package. The package component that
    -- specifies its namespace depends on its type. For example:
    --
    -- -   The namespace of a Maven package is its @groupId@. The namespace is
    --     required when requesting Maven packages.
    --
    -- -   The namespace of an npm package is its @scope@.
    --
    -- -   Python and NuGet packages do not contain a corresponding component,
    --     packages of those formats do not have a namespace.
    --
    -- -   The namespace of a generic package is its @namespace@.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the repository that contains the
    -- package.
    domain :: Prelude.Text,
    -- | The name of the repository that contains the requested package.
    repository :: Prelude.Text,
    -- | A format that specifies the type of the requested package.
    format :: PackageFormat,
    -- | The name of the requested package.
    package :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'describePackage_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'namespace', 'describePackage_namespace' - The namespace of the requested package. The package component that
-- specifies its namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@. The namespace is
--     required when requesting Maven packages.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   Python and NuGet packages do not contain a corresponding component,
--     packages of those formats do not have a namespace.
--
-- -   The namespace of a generic package is its @namespace@.
--
-- 'domain', 'describePackage_domain' - The name of the domain that contains the repository that contains the
-- package.
--
-- 'repository', 'describePackage_repository' - The name of the repository that contains the requested package.
--
-- 'format', 'describePackage_format' - A format that specifies the type of the requested package.
--
-- 'package', 'describePackage_package' - The name of the requested package.
newDescribePackage ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  -- | 'format'
  PackageFormat ->
  -- | 'package'
  Prelude.Text ->
  DescribePackage
newDescribePackage
  pDomain_
  pRepository_
  pFormat_
  pPackage_ =
    DescribePackage'
      { domainOwner = Prelude.Nothing,
        namespace = Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        format = pFormat_,
        package = pPackage_
      }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
describePackage_domainOwner :: Lens.Lens' DescribePackage (Prelude.Maybe Prelude.Text)
describePackage_domainOwner = Lens.lens (\DescribePackage' {domainOwner} -> domainOwner) (\s@DescribePackage' {} a -> s {domainOwner = a} :: DescribePackage)

-- | The namespace of the requested package. The package component that
-- specifies its namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@. The namespace is
--     required when requesting Maven packages.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   Python and NuGet packages do not contain a corresponding component,
--     packages of those formats do not have a namespace.
--
-- -   The namespace of a generic package is its @namespace@.
describePackage_namespace :: Lens.Lens' DescribePackage (Prelude.Maybe Prelude.Text)
describePackage_namespace = Lens.lens (\DescribePackage' {namespace} -> namespace) (\s@DescribePackage' {} a -> s {namespace = a} :: DescribePackage)

-- | The name of the domain that contains the repository that contains the
-- package.
describePackage_domain :: Lens.Lens' DescribePackage Prelude.Text
describePackage_domain = Lens.lens (\DescribePackage' {domain} -> domain) (\s@DescribePackage' {} a -> s {domain = a} :: DescribePackage)

-- | The name of the repository that contains the requested package.
describePackage_repository :: Lens.Lens' DescribePackage Prelude.Text
describePackage_repository = Lens.lens (\DescribePackage' {repository} -> repository) (\s@DescribePackage' {} a -> s {repository = a} :: DescribePackage)

-- | A format that specifies the type of the requested package.
describePackage_format :: Lens.Lens' DescribePackage PackageFormat
describePackage_format = Lens.lens (\DescribePackage' {format} -> format) (\s@DescribePackage' {} a -> s {format = a} :: DescribePackage)

-- | The name of the requested package.
describePackage_package :: Lens.Lens' DescribePackage Prelude.Text
describePackage_package = Lens.lens (\DescribePackage' {package} -> package) (\s@DescribePackage' {} a -> s {package = a} :: DescribePackage)

instance Core.AWSRequest DescribePackage where
  type
    AWSResponse DescribePackage =
      DescribePackageResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePackageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "package")
      )

instance Prelude.Hashable DescribePackage where
  hashWithSalt _salt DescribePackage' {..} =
    _salt
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` package

instance Prelude.NFData DescribePackage where
  rnf DescribePackage' {..} =
    Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf package

instance Data.ToHeaders DescribePackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribePackage where
  toPath = Prelude.const "/v1/package"

instance Data.ToQuery DescribePackage where
  toQuery DescribePackage' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "namespace" Data.=: namespace,
        "domain" Data.=: domain,
        "repository" Data.=: repository,
        "format" Data.=: format,
        "package" Data.=: package
      ]

-- | /See:/ 'newDescribePackageResponse' smart constructor.
data DescribePackageResponse = DescribePackageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageDescription.html PackageDescription>
    -- object that contains information about the requested package.
    package :: PackageDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describePackageResponse_httpStatus' - The response's http status code.
--
-- 'package', 'describePackageResponse_package' - A
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageDescription.html PackageDescription>
-- object that contains information about the requested package.
newDescribePackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'package'
  PackageDescription ->
  DescribePackageResponse
newDescribePackageResponse pHttpStatus_ pPackage_ =
  DescribePackageResponse'
    { httpStatus = pHttpStatus_,
      package = pPackage_
    }

-- | The response's http status code.
describePackageResponse_httpStatus :: Lens.Lens' DescribePackageResponse Prelude.Int
describePackageResponse_httpStatus = Lens.lens (\DescribePackageResponse' {httpStatus} -> httpStatus) (\s@DescribePackageResponse' {} a -> s {httpStatus = a} :: DescribePackageResponse)

-- | A
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageDescription.html PackageDescription>
-- object that contains information about the requested package.
describePackageResponse_package :: Lens.Lens' DescribePackageResponse PackageDescription
describePackageResponse_package = Lens.lens (\DescribePackageResponse' {package} -> package) (\s@DescribePackageResponse' {} a -> s {package = a} :: DescribePackageResponse)

instance Prelude.NFData DescribePackageResponse where
  rnf DescribePackageResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf package
