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
-- Module      : Amazonka.CodeArtifact.PutPackageOriginConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the package origin configuration for a package.
--
-- The package origin configuration determines how new versions of a
-- package can be added to a repository. You can allow or block direct
-- publishing of new package versions, or ingestion and retaining of new
-- package versions from an external connection or upstream source. For
-- more information about package origin controls and configuration, see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/package-origin-controls.html Editing package origin controls>
-- in the /CodeArtifact User Guide/.
--
-- @PutPackageOriginConfiguration@ can be called on a package that doesn\'t
-- yet exist in the repository. When called on a package that does not
-- exist, a package is created in the repository with no versions and the
-- requested restrictions are set on the package. This can be used to
-- preemptively block ingesting or retaining any versions from external
-- connections or upstream repositories, or to block publishing any
-- versions of the package into the repository before connecting any
-- package managers or publishers to the repository.
module Amazonka.CodeArtifact.PutPackageOriginConfiguration
  ( -- * Creating a Request
    PutPackageOriginConfiguration (..),
    newPutPackageOriginConfiguration,

    -- * Request Lenses
    putPackageOriginConfiguration_domainOwner,
    putPackageOriginConfiguration_namespace,
    putPackageOriginConfiguration_domain,
    putPackageOriginConfiguration_repository,
    putPackageOriginConfiguration_format,
    putPackageOriginConfiguration_package,
    putPackageOriginConfiguration_restrictions,

    -- * Destructuring the Response
    PutPackageOriginConfigurationResponse (..),
    newPutPackageOriginConfigurationResponse,

    -- * Response Lenses
    putPackageOriginConfigurationResponse_originConfiguration,
    putPackageOriginConfigurationResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutPackageOriginConfiguration' smart constructor.
data PutPackageOriginConfiguration = PutPackageOriginConfiguration'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the package to be updated. The package component that
    -- specifies its namespace depends on its type. For example:
    --
    -- -   The namespace of a Maven package is its @groupId@.
    --
    -- -   The namespace of an npm package is its @scope@.
    --
    -- -   Python and NuGet packages do not contain a corresponding component,
    --     packages of those formats do not have a namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the repository that contains the
    -- package.
    domain :: Prelude.Text,
    -- | The name of the repository that contains the package.
    repository :: Prelude.Text,
    -- | A format that specifies the type of the package to be updated.
    format :: PackageFormat,
    -- | The name of the package to be updated.
    package :: Prelude.Text,
    -- | A
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginRestrictions.html PackageOriginRestrictions>
    -- object that contains information about the @upstream@ and @publish@
    -- package origin restrictions. The @upstream@ restriction determines if
    -- new package versions can be ingested or retained from external
    -- connections or upstream repositories. The @publish@ restriction
    -- determines if new package versions can be published directly to the
    -- repository.
    --
    -- You must include both the desired @upstream@ and @publish@ restrictions.
    restrictions :: PackageOriginRestrictions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPackageOriginConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'putPackageOriginConfiguration_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'namespace', 'putPackageOriginConfiguration_namespace' - The namespace of the package to be updated. The package component that
-- specifies its namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   Python and NuGet packages do not contain a corresponding component,
--     packages of those formats do not have a namespace.
--
-- 'domain', 'putPackageOriginConfiguration_domain' - The name of the domain that contains the repository that contains the
-- package.
--
-- 'repository', 'putPackageOriginConfiguration_repository' - The name of the repository that contains the package.
--
-- 'format', 'putPackageOriginConfiguration_format' - A format that specifies the type of the package to be updated.
--
-- 'package', 'putPackageOriginConfiguration_package' - The name of the package to be updated.
--
-- 'restrictions', 'putPackageOriginConfiguration_restrictions' - A
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginRestrictions.html PackageOriginRestrictions>
-- object that contains information about the @upstream@ and @publish@
-- package origin restrictions. The @upstream@ restriction determines if
-- new package versions can be ingested or retained from external
-- connections or upstream repositories. The @publish@ restriction
-- determines if new package versions can be published directly to the
-- repository.
--
-- You must include both the desired @upstream@ and @publish@ restrictions.
newPutPackageOriginConfiguration ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  -- | 'format'
  PackageFormat ->
  -- | 'package'
  Prelude.Text ->
  -- | 'restrictions'
  PackageOriginRestrictions ->
  PutPackageOriginConfiguration
newPutPackageOriginConfiguration
  pDomain_
  pRepository_
  pFormat_
  pPackage_
  pRestrictions_ =
    PutPackageOriginConfiguration'
      { domainOwner =
          Prelude.Nothing,
        namespace = Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        format = pFormat_,
        package = pPackage_,
        restrictions = pRestrictions_
      }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
putPackageOriginConfiguration_domainOwner :: Lens.Lens' PutPackageOriginConfiguration (Prelude.Maybe Prelude.Text)
putPackageOriginConfiguration_domainOwner = Lens.lens (\PutPackageOriginConfiguration' {domainOwner} -> domainOwner) (\s@PutPackageOriginConfiguration' {} a -> s {domainOwner = a} :: PutPackageOriginConfiguration)

-- | The namespace of the package to be updated. The package component that
-- specifies its namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   Python and NuGet packages do not contain a corresponding component,
--     packages of those formats do not have a namespace.
putPackageOriginConfiguration_namespace :: Lens.Lens' PutPackageOriginConfiguration (Prelude.Maybe Prelude.Text)
putPackageOriginConfiguration_namespace = Lens.lens (\PutPackageOriginConfiguration' {namespace} -> namespace) (\s@PutPackageOriginConfiguration' {} a -> s {namespace = a} :: PutPackageOriginConfiguration)

-- | The name of the domain that contains the repository that contains the
-- package.
putPackageOriginConfiguration_domain :: Lens.Lens' PutPackageOriginConfiguration Prelude.Text
putPackageOriginConfiguration_domain = Lens.lens (\PutPackageOriginConfiguration' {domain} -> domain) (\s@PutPackageOriginConfiguration' {} a -> s {domain = a} :: PutPackageOriginConfiguration)

-- | The name of the repository that contains the package.
putPackageOriginConfiguration_repository :: Lens.Lens' PutPackageOriginConfiguration Prelude.Text
putPackageOriginConfiguration_repository = Lens.lens (\PutPackageOriginConfiguration' {repository} -> repository) (\s@PutPackageOriginConfiguration' {} a -> s {repository = a} :: PutPackageOriginConfiguration)

-- | A format that specifies the type of the package to be updated.
putPackageOriginConfiguration_format :: Lens.Lens' PutPackageOriginConfiguration PackageFormat
putPackageOriginConfiguration_format = Lens.lens (\PutPackageOriginConfiguration' {format} -> format) (\s@PutPackageOriginConfiguration' {} a -> s {format = a} :: PutPackageOriginConfiguration)

-- | The name of the package to be updated.
putPackageOriginConfiguration_package :: Lens.Lens' PutPackageOriginConfiguration Prelude.Text
putPackageOriginConfiguration_package = Lens.lens (\PutPackageOriginConfiguration' {package} -> package) (\s@PutPackageOriginConfiguration' {} a -> s {package = a} :: PutPackageOriginConfiguration)

-- | A
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginRestrictions.html PackageOriginRestrictions>
-- object that contains information about the @upstream@ and @publish@
-- package origin restrictions. The @upstream@ restriction determines if
-- new package versions can be ingested or retained from external
-- connections or upstream repositories. The @publish@ restriction
-- determines if new package versions can be published directly to the
-- repository.
--
-- You must include both the desired @upstream@ and @publish@ restrictions.
putPackageOriginConfiguration_restrictions :: Lens.Lens' PutPackageOriginConfiguration PackageOriginRestrictions
putPackageOriginConfiguration_restrictions = Lens.lens (\PutPackageOriginConfiguration' {restrictions} -> restrictions) (\s@PutPackageOriginConfiguration' {} a -> s {restrictions = a} :: PutPackageOriginConfiguration)

instance
  Core.AWSRequest
    PutPackageOriginConfiguration
  where
  type
    AWSResponse PutPackageOriginConfiguration =
      PutPackageOriginConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutPackageOriginConfigurationResponse'
            Prelude.<$> (x Data..?> "originConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutPackageOriginConfiguration
  where
  hashWithSalt _salt PutPackageOriginConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` package
      `Prelude.hashWithSalt` restrictions

instance Prelude.NFData PutPackageOriginConfiguration where
  rnf PutPackageOriginConfiguration' {..} =
    Prelude.rnf domainOwner `Prelude.seq`
      Prelude.rnf namespace `Prelude.seq`
        Prelude.rnf domain `Prelude.seq`
          Prelude.rnf repository `Prelude.seq`
            Prelude.rnf format `Prelude.seq`
              Prelude.rnf package `Prelude.seq`
                Prelude.rnf restrictions

instance Data.ToHeaders PutPackageOriginConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutPackageOriginConfiguration where
  toJSON PutPackageOriginConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("restrictions" Data..= restrictions)]
      )

instance Data.ToPath PutPackageOriginConfiguration where
  toPath = Prelude.const "/v1/package"

instance Data.ToQuery PutPackageOriginConfiguration where
  toQuery PutPackageOriginConfiguration' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "namespace" Data.=: namespace,
        "domain" Data.=: domain,
        "repository" Data.=: repository,
        "format" Data.=: format,
        "package" Data.=: package
      ]

-- | /See:/ 'newPutPackageOriginConfigurationResponse' smart constructor.
data PutPackageOriginConfigurationResponse = PutPackageOriginConfigurationResponse'
  { -- | A
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginConfiguration.html PackageOriginConfiguration>
    -- object that describes the origin configuration set for the package. It
    -- contains a
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginRestrictions.html PackageOriginRestrictions>
    -- object that describes how new versions of the package can be introduced
    -- to the repository.
    originConfiguration :: Prelude.Maybe PackageOriginConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPackageOriginConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originConfiguration', 'putPackageOriginConfigurationResponse_originConfiguration' - A
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginConfiguration.html PackageOriginConfiguration>
-- object that describes the origin configuration set for the package. It
-- contains a
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginRestrictions.html PackageOriginRestrictions>
-- object that describes how new versions of the package can be introduced
-- to the repository.
--
-- 'httpStatus', 'putPackageOriginConfigurationResponse_httpStatus' - The response's http status code.
newPutPackageOriginConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutPackageOriginConfigurationResponse
newPutPackageOriginConfigurationResponse pHttpStatus_ =
  PutPackageOriginConfigurationResponse'
    { originConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginConfiguration.html PackageOriginConfiguration>
-- object that describes the origin configuration set for the package. It
-- contains a
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_PackageOriginRestrictions.html PackageOriginRestrictions>
-- object that describes how new versions of the package can be introduced
-- to the repository.
putPackageOriginConfigurationResponse_originConfiguration :: Lens.Lens' PutPackageOriginConfigurationResponse (Prelude.Maybe PackageOriginConfiguration)
putPackageOriginConfigurationResponse_originConfiguration = Lens.lens (\PutPackageOriginConfigurationResponse' {originConfiguration} -> originConfiguration) (\s@PutPackageOriginConfigurationResponse' {} a -> s {originConfiguration = a} :: PutPackageOriginConfigurationResponse)

-- | The response's http status code.
putPackageOriginConfigurationResponse_httpStatus :: Lens.Lens' PutPackageOriginConfigurationResponse Prelude.Int
putPackageOriginConfigurationResponse_httpStatus = Lens.lens (\PutPackageOriginConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutPackageOriginConfigurationResponse' {} a -> s {httpStatus = a} :: PutPackageOriginConfigurationResponse)

instance
  Prelude.NFData
    PutPackageOriginConfigurationResponse
  where
  rnf PutPackageOriginConfigurationResponse' {..} =
    Prelude.rnf originConfiguration `Prelude.seq`
      Prelude.rnf httpStatus
