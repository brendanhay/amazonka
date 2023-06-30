{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeArtifact.Types.PackageVersionOrigin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.PackageVersionOrigin where

import Amazonka.CodeArtifact.Types.DomainEntryPoint
import Amazonka.CodeArtifact.Types.PackageVersionOriginType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about how a package version was added to a repository.
--
-- /See:/ 'newPackageVersionOrigin' smart constructor.
data PackageVersionOrigin = PackageVersionOrigin'
  { -- | A
    -- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_DomainEntryPoint.html DomainEntryPoint>
    -- object that contains information about from which repository or external
    -- connection the package version was added to the domain.
    domainEntryPoint :: Prelude.Maybe DomainEntryPoint,
    -- | Describes how the package version was originally added to the domain. An
    -- @INTERNAL@ origin type means the package version was published directly
    -- to a repository in the domain. An @EXTERNAL@ origin type means the
    -- package version was ingested from an external connection.
    originType :: Prelude.Maybe PackageVersionOriginType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageVersionOrigin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainEntryPoint', 'packageVersionOrigin_domainEntryPoint' - A
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_DomainEntryPoint.html DomainEntryPoint>
-- object that contains information about from which repository or external
-- connection the package version was added to the domain.
--
-- 'originType', 'packageVersionOrigin_originType' - Describes how the package version was originally added to the domain. An
-- @INTERNAL@ origin type means the package version was published directly
-- to a repository in the domain. An @EXTERNAL@ origin type means the
-- package version was ingested from an external connection.
newPackageVersionOrigin ::
  PackageVersionOrigin
newPackageVersionOrigin =
  PackageVersionOrigin'
    { domainEntryPoint =
        Prelude.Nothing,
      originType = Prelude.Nothing
    }

-- | A
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_DomainEntryPoint.html DomainEntryPoint>
-- object that contains information about from which repository or external
-- connection the package version was added to the domain.
packageVersionOrigin_domainEntryPoint :: Lens.Lens' PackageVersionOrigin (Prelude.Maybe DomainEntryPoint)
packageVersionOrigin_domainEntryPoint = Lens.lens (\PackageVersionOrigin' {domainEntryPoint} -> domainEntryPoint) (\s@PackageVersionOrigin' {} a -> s {domainEntryPoint = a} :: PackageVersionOrigin)

-- | Describes how the package version was originally added to the domain. An
-- @INTERNAL@ origin type means the package version was published directly
-- to a repository in the domain. An @EXTERNAL@ origin type means the
-- package version was ingested from an external connection.
packageVersionOrigin_originType :: Lens.Lens' PackageVersionOrigin (Prelude.Maybe PackageVersionOriginType)
packageVersionOrigin_originType = Lens.lens (\PackageVersionOrigin' {originType} -> originType) (\s@PackageVersionOrigin' {} a -> s {originType = a} :: PackageVersionOrigin)

instance Data.FromJSON PackageVersionOrigin where
  parseJSON =
    Data.withObject
      "PackageVersionOrigin"
      ( \x ->
          PackageVersionOrigin'
            Prelude.<$> (x Data..:? "domainEntryPoint")
            Prelude.<*> (x Data..:? "originType")
      )

instance Prelude.Hashable PackageVersionOrigin where
  hashWithSalt _salt PackageVersionOrigin' {..} =
    _salt
      `Prelude.hashWithSalt` domainEntryPoint
      `Prelude.hashWithSalt` originType

instance Prelude.NFData PackageVersionOrigin where
  rnf PackageVersionOrigin' {..} =
    Prelude.rnf domainEntryPoint
      `Prelude.seq` Prelude.rnf originType
