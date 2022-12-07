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
-- Module      : Amazonka.GreengrassV2.Types.ComponentCandidate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.ComponentCandidate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a component that is a candidate to deploy to
-- a Greengrass core device.
--
-- /See:/ 'newComponentCandidate' smart constructor.
data ComponentCandidate = ComponentCandidate'
  { -- | The version of the component.
    componentVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the component.
    componentName :: Prelude.Maybe Prelude.Text,
    -- | The version requirements for the component\'s dependencies. Greengrass
    -- core devices get the version requirements from component recipes.
    --
    -- IoT Greengrass V2 uses semantic version constraints. For more
    -- information, see <https://semver.org/ Semantic Versioning>.
    versionRequirements :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentCandidate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentVersion', 'componentCandidate_componentVersion' - The version of the component.
--
-- 'componentName', 'componentCandidate_componentName' - The name of the component.
--
-- 'versionRequirements', 'componentCandidate_versionRequirements' - The version requirements for the component\'s dependencies. Greengrass
-- core devices get the version requirements from component recipes.
--
-- IoT Greengrass V2 uses semantic version constraints. For more
-- information, see <https://semver.org/ Semantic Versioning>.
newComponentCandidate ::
  ComponentCandidate
newComponentCandidate =
  ComponentCandidate'
    { componentVersion =
        Prelude.Nothing,
      componentName = Prelude.Nothing,
      versionRequirements = Prelude.Nothing
    }

-- | The version of the component.
componentCandidate_componentVersion :: Lens.Lens' ComponentCandidate (Prelude.Maybe Prelude.Text)
componentCandidate_componentVersion = Lens.lens (\ComponentCandidate' {componentVersion} -> componentVersion) (\s@ComponentCandidate' {} a -> s {componentVersion = a} :: ComponentCandidate)

-- | The name of the component.
componentCandidate_componentName :: Lens.Lens' ComponentCandidate (Prelude.Maybe Prelude.Text)
componentCandidate_componentName = Lens.lens (\ComponentCandidate' {componentName} -> componentName) (\s@ComponentCandidate' {} a -> s {componentName = a} :: ComponentCandidate)

-- | The version requirements for the component\'s dependencies. Greengrass
-- core devices get the version requirements from component recipes.
--
-- IoT Greengrass V2 uses semantic version constraints. For more
-- information, see <https://semver.org/ Semantic Versioning>.
componentCandidate_versionRequirements :: Lens.Lens' ComponentCandidate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
componentCandidate_versionRequirements = Lens.lens (\ComponentCandidate' {versionRequirements} -> versionRequirements) (\s@ComponentCandidate' {} a -> s {versionRequirements = a} :: ComponentCandidate) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ComponentCandidate where
  hashWithSalt _salt ComponentCandidate' {..} =
    _salt `Prelude.hashWithSalt` componentVersion
      `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` versionRequirements

instance Prelude.NFData ComponentCandidate where
  rnf ComponentCandidate' {..} =
    Prelude.rnf componentVersion
      `Prelude.seq` Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf versionRequirements

instance Data.ToJSON ComponentCandidate where
  toJSON ComponentCandidate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("componentVersion" Data..=)
              Prelude.<$> componentVersion,
            ("componentName" Data..=) Prelude.<$> componentName,
            ("versionRequirements" Data..=)
              Prelude.<$> versionRequirements
          ]
      )
