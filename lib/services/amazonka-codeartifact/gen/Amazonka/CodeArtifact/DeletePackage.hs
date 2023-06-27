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
-- Module      : Amazonka.CodeArtifact.DeletePackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a package and all associated package versions. A deleted package
-- cannot be restored. To delete one or more package versions, use the
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_DeletePackageVersions.html DeletePackageVersions>
-- API.
module Amazonka.CodeArtifact.DeletePackage
  ( -- * Creating a Request
    DeletePackage (..),
    newDeletePackage,

    -- * Request Lenses
    deletePackage_domainOwner,
    deletePackage_namespace,
    deletePackage_domain,
    deletePackage_repository,
    deletePackage_format,
    deletePackage_package,

    -- * Destructuring the Response
    DeletePackageResponse (..),
    newDeletePackageResponse,

    -- * Response Lenses
    deletePackageResponse_deletedPackage,
    deletePackageResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePackage' smart constructor.
data DeletePackage = DeletePackage'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the package to delete. The package component that
    -- specifies its namespace depends on its type. For example:
    --
    -- -   The namespace of a Maven package is its @groupId@. The namespace is
    --     required when deleting Maven package versions.
    --
    -- -   The namespace of an npm package is its @scope@.
    --
    -- -   Python and NuGet packages do not contain corresponding components,
    --     packages of those formats do not have a namespace.
    --
    -- -   The namespace of a generic package is its @namespace@.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the package to delete.
    domain :: Prelude.Text,
    -- | The name of the repository that contains the package to delete.
    repository :: Prelude.Text,
    -- | The format of the requested package to delete.
    format :: PackageFormat,
    -- | The name of the package to delete.
    package :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'deletePackage_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'namespace', 'deletePackage_namespace' - The namespace of the package to delete. The package component that
-- specifies its namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@. The namespace is
--     required when deleting Maven package versions.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   Python and NuGet packages do not contain corresponding components,
--     packages of those formats do not have a namespace.
--
-- -   The namespace of a generic package is its @namespace@.
--
-- 'domain', 'deletePackage_domain' - The name of the domain that contains the package to delete.
--
-- 'repository', 'deletePackage_repository' - The name of the repository that contains the package to delete.
--
-- 'format', 'deletePackage_format' - The format of the requested package to delete.
--
-- 'package', 'deletePackage_package' - The name of the package to delete.
newDeletePackage ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  -- | 'format'
  PackageFormat ->
  -- | 'package'
  Prelude.Text ->
  DeletePackage
newDeletePackage
  pDomain_
  pRepository_
  pFormat_
  pPackage_ =
    DeletePackage'
      { domainOwner = Prelude.Nothing,
        namespace = Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        format = pFormat_,
        package = pPackage_
      }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
deletePackage_domainOwner :: Lens.Lens' DeletePackage (Prelude.Maybe Prelude.Text)
deletePackage_domainOwner = Lens.lens (\DeletePackage' {domainOwner} -> domainOwner) (\s@DeletePackage' {} a -> s {domainOwner = a} :: DeletePackage)

-- | The namespace of the package to delete. The package component that
-- specifies its namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@. The namespace is
--     required when deleting Maven package versions.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   Python and NuGet packages do not contain corresponding components,
--     packages of those formats do not have a namespace.
--
-- -   The namespace of a generic package is its @namespace@.
deletePackage_namespace :: Lens.Lens' DeletePackage (Prelude.Maybe Prelude.Text)
deletePackage_namespace = Lens.lens (\DeletePackage' {namespace} -> namespace) (\s@DeletePackage' {} a -> s {namespace = a} :: DeletePackage)

-- | The name of the domain that contains the package to delete.
deletePackage_domain :: Lens.Lens' DeletePackage Prelude.Text
deletePackage_domain = Lens.lens (\DeletePackage' {domain} -> domain) (\s@DeletePackage' {} a -> s {domain = a} :: DeletePackage)

-- | The name of the repository that contains the package to delete.
deletePackage_repository :: Lens.Lens' DeletePackage Prelude.Text
deletePackage_repository = Lens.lens (\DeletePackage' {repository} -> repository) (\s@DeletePackage' {} a -> s {repository = a} :: DeletePackage)

-- | The format of the requested package to delete.
deletePackage_format :: Lens.Lens' DeletePackage PackageFormat
deletePackage_format = Lens.lens (\DeletePackage' {format} -> format) (\s@DeletePackage' {} a -> s {format = a} :: DeletePackage)

-- | The name of the package to delete.
deletePackage_package :: Lens.Lens' DeletePackage Prelude.Text
deletePackage_package = Lens.lens (\DeletePackage' {package} -> package) (\s@DeletePackage' {} a -> s {package = a} :: DeletePackage)

instance Core.AWSRequest DeletePackage where
  type
    AWSResponse DeletePackage =
      DeletePackageResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePackageResponse'
            Prelude.<$> (x Data..?> "deletedPackage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePackage where
  hashWithSalt _salt DeletePackage' {..} =
    _salt
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` package

instance Prelude.NFData DeletePackage where
  rnf DeletePackage' {..} =
    Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf package

instance Data.ToHeaders DeletePackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeletePackage where
  toPath = Prelude.const "/v1/package"

instance Data.ToQuery DeletePackage where
  toQuery DeletePackage' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "namespace" Data.=: namespace,
        "domain" Data.=: domain,
        "repository" Data.=: repository,
        "format" Data.=: format,
        "package" Data.=: package
      ]

-- | /See:/ 'newDeletePackageResponse' smart constructor.
data DeletePackageResponse = DeletePackageResponse'
  { deletedPackage :: Prelude.Maybe PackageSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletedPackage', 'deletePackageResponse_deletedPackage' - Undocumented member.
--
-- 'httpStatus', 'deletePackageResponse_httpStatus' - The response's http status code.
newDeletePackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePackageResponse
newDeletePackageResponse pHttpStatus_ =
  DeletePackageResponse'
    { deletedPackage =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deletePackageResponse_deletedPackage :: Lens.Lens' DeletePackageResponse (Prelude.Maybe PackageSummary)
deletePackageResponse_deletedPackage = Lens.lens (\DeletePackageResponse' {deletedPackage} -> deletedPackage) (\s@DeletePackageResponse' {} a -> s {deletedPackage = a} :: DeletePackageResponse)

-- | The response's http status code.
deletePackageResponse_httpStatus :: Lens.Lens' DeletePackageResponse Prelude.Int
deletePackageResponse_httpStatus = Lens.lens (\DeletePackageResponse' {httpStatus} -> httpStatus) (\s@DeletePackageResponse' {} a -> s {httpStatus = a} :: DeletePackageResponse)

instance Prelude.NFData DeletePackageResponse where
  rnf DeletePackageResponse' {..} =
    Prelude.rnf deletedPackage
      `Prelude.seq` Prelude.rnf httpStatus
