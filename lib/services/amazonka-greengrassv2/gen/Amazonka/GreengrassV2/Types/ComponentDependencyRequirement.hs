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
-- Module      : Amazonka.GreengrassV2.Types.ComponentDependencyRequirement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.ComponentDependencyRequirement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GreengrassV2.Types.ComponentDependencyType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a component dependency for a Lambda function
-- component.
--
-- /See:/ 'newComponentDependencyRequirement' smart constructor.
data ComponentDependencyRequirement = ComponentDependencyRequirement'
  { -- | The component version requirement for the component dependency.
    --
    -- IoT Greengrass V2 uses semantic version constraints. For more
    -- information, see <https://semver.org/ Semantic Versioning>.
    versionRequirement :: Prelude.Maybe Prelude.Text,
    -- | The type of this dependency. Choose from the following options:
    --
    -- -   @SOFT@ – The component doesn\'t restart if the dependency changes
    --     state.
    --
    -- -   @HARD@ – The component restarts if the dependency changes state.
    --
    -- Default: @HARD@
    dependencyType :: Prelude.Maybe ComponentDependencyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentDependencyRequirement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionRequirement', 'componentDependencyRequirement_versionRequirement' - The component version requirement for the component dependency.
--
-- IoT Greengrass V2 uses semantic version constraints. For more
-- information, see <https://semver.org/ Semantic Versioning>.
--
-- 'dependencyType', 'componentDependencyRequirement_dependencyType' - The type of this dependency. Choose from the following options:
--
-- -   @SOFT@ – The component doesn\'t restart if the dependency changes
--     state.
--
-- -   @HARD@ – The component restarts if the dependency changes state.
--
-- Default: @HARD@
newComponentDependencyRequirement ::
  ComponentDependencyRequirement
newComponentDependencyRequirement =
  ComponentDependencyRequirement'
    { versionRequirement =
        Prelude.Nothing,
      dependencyType = Prelude.Nothing
    }

-- | The component version requirement for the component dependency.
--
-- IoT Greengrass V2 uses semantic version constraints. For more
-- information, see <https://semver.org/ Semantic Versioning>.
componentDependencyRequirement_versionRequirement :: Lens.Lens' ComponentDependencyRequirement (Prelude.Maybe Prelude.Text)
componentDependencyRequirement_versionRequirement = Lens.lens (\ComponentDependencyRequirement' {versionRequirement} -> versionRequirement) (\s@ComponentDependencyRequirement' {} a -> s {versionRequirement = a} :: ComponentDependencyRequirement)

-- | The type of this dependency. Choose from the following options:
--
-- -   @SOFT@ – The component doesn\'t restart if the dependency changes
--     state.
--
-- -   @HARD@ – The component restarts if the dependency changes state.
--
-- Default: @HARD@
componentDependencyRequirement_dependencyType :: Lens.Lens' ComponentDependencyRequirement (Prelude.Maybe ComponentDependencyType)
componentDependencyRequirement_dependencyType = Lens.lens (\ComponentDependencyRequirement' {dependencyType} -> dependencyType) (\s@ComponentDependencyRequirement' {} a -> s {dependencyType = a} :: ComponentDependencyRequirement)

instance
  Prelude.Hashable
    ComponentDependencyRequirement
  where
  hashWithSalt
    _salt
    ComponentDependencyRequirement' {..} =
      _salt `Prelude.hashWithSalt` versionRequirement
        `Prelude.hashWithSalt` dependencyType

instance
  Prelude.NFData
    ComponentDependencyRequirement
  where
  rnf ComponentDependencyRequirement' {..} =
    Prelude.rnf versionRequirement
      `Prelude.seq` Prelude.rnf dependencyType

instance Core.ToJSON ComponentDependencyRequirement where
  toJSON ComponentDependencyRequirement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("versionRequirement" Core..=)
              Prelude.<$> versionRequirement,
            ("dependencyType" Core..=)
              Prelude.<$> dependencyType
          ]
      )
