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
-- Module      : Amazonka.CodeArtifact.Types.PackageOriginConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.PackageOriginConfiguration where

import Amazonka.CodeArtifact.Types.PackageOriginRestrictions
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the package origin configuration of a package.
--
-- /See:/ 'newPackageOriginConfiguration' smart constructor.
data PackageOriginConfiguration = PackageOriginConfiguration'
  { -- | A @PackageOriginRestrictions@ object that contains information about the
    -- upstream and publish package origin configuration for the package.
    restrictions :: Prelude.Maybe PackageOriginRestrictions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageOriginConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restrictions', 'packageOriginConfiguration_restrictions' - A @PackageOriginRestrictions@ object that contains information about the
-- upstream and publish package origin configuration for the package.
newPackageOriginConfiguration ::
  PackageOriginConfiguration
newPackageOriginConfiguration =
  PackageOriginConfiguration'
    { restrictions =
        Prelude.Nothing
    }

-- | A @PackageOriginRestrictions@ object that contains information about the
-- upstream and publish package origin configuration for the package.
packageOriginConfiguration_restrictions :: Lens.Lens' PackageOriginConfiguration (Prelude.Maybe PackageOriginRestrictions)
packageOriginConfiguration_restrictions = Lens.lens (\PackageOriginConfiguration' {restrictions} -> restrictions) (\s@PackageOriginConfiguration' {} a -> s {restrictions = a} :: PackageOriginConfiguration)

instance Data.FromJSON PackageOriginConfiguration where
  parseJSON =
    Data.withObject
      "PackageOriginConfiguration"
      ( \x ->
          PackageOriginConfiguration'
            Prelude.<$> (x Data..:? "restrictions")
      )

instance Prelude.Hashable PackageOriginConfiguration where
  hashWithSalt _salt PackageOriginConfiguration' {..} =
    _salt `Prelude.hashWithSalt` restrictions

instance Prelude.NFData PackageOriginConfiguration where
  rnf PackageOriginConfiguration' {..} =
    Prelude.rnf restrictions
