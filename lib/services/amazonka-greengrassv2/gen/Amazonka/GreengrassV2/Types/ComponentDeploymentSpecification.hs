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
-- Module      : Amazonka.GreengrassV2.Types.ComponentDeploymentSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.ComponentDeploymentSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.ComponentConfigurationUpdate
import Amazonka.GreengrassV2.Types.ComponentRunWith
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a component to deploy.
--
-- /See:/ 'newComponentDeploymentSpecification' smart constructor.
data ComponentDeploymentSpecification = ComponentDeploymentSpecification'
  { -- | The version of the component.
    componentVersion :: Prelude.Maybe Prelude.Text,
    -- | The system user and group that the IoT Greengrass Core software uses to
    -- run component processes on the core device. If you omit this parameter,
    -- the IoT Greengrass Core software uses the system user and group that you
    -- configure for the core device. For more information, see
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/configure-greengrass-core-v2.html#configure-component-user Configure the user and group that run components>
    -- in the /IoT Greengrass V2 Developer Guide/.
    runWith :: Prelude.Maybe ComponentRunWith,
    -- | The configuration updates to deploy for the component. You can define
    -- /reset/ updates and /merge/ updates. A reset updates the keys that you
    -- specify to the default configuration for the component. A merge updates
    -- the core device\'s component configuration with the keys and values that
    -- you specify. The IoT Greengrass Core software applies reset updates
    -- before it applies merge updates. For more information, see
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/update-component-configurations.html Update component configurations>
    -- in the /IoT Greengrass V2 Developer Guide/.
    configurationUpdate :: Prelude.Maybe ComponentConfigurationUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentDeploymentSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentVersion', 'componentDeploymentSpecification_componentVersion' - The version of the component.
--
-- 'runWith', 'componentDeploymentSpecification_runWith' - The system user and group that the IoT Greengrass Core software uses to
-- run component processes on the core device. If you omit this parameter,
-- the IoT Greengrass Core software uses the system user and group that you
-- configure for the core device. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/configure-greengrass-core-v2.html#configure-component-user Configure the user and group that run components>
-- in the /IoT Greengrass V2 Developer Guide/.
--
-- 'configurationUpdate', 'componentDeploymentSpecification_configurationUpdate' - The configuration updates to deploy for the component. You can define
-- /reset/ updates and /merge/ updates. A reset updates the keys that you
-- specify to the default configuration for the component. A merge updates
-- the core device\'s component configuration with the keys and values that
-- you specify. The IoT Greengrass Core software applies reset updates
-- before it applies merge updates. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/update-component-configurations.html Update component configurations>
-- in the /IoT Greengrass V2 Developer Guide/.
newComponentDeploymentSpecification ::
  ComponentDeploymentSpecification
newComponentDeploymentSpecification =
  ComponentDeploymentSpecification'
    { componentVersion =
        Prelude.Nothing,
      runWith = Prelude.Nothing,
      configurationUpdate = Prelude.Nothing
    }

-- | The version of the component.
componentDeploymentSpecification_componentVersion :: Lens.Lens' ComponentDeploymentSpecification (Prelude.Maybe Prelude.Text)
componentDeploymentSpecification_componentVersion = Lens.lens (\ComponentDeploymentSpecification' {componentVersion} -> componentVersion) (\s@ComponentDeploymentSpecification' {} a -> s {componentVersion = a} :: ComponentDeploymentSpecification)

-- | The system user and group that the IoT Greengrass Core software uses to
-- run component processes on the core device. If you omit this parameter,
-- the IoT Greengrass Core software uses the system user and group that you
-- configure for the core device. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/configure-greengrass-core-v2.html#configure-component-user Configure the user and group that run components>
-- in the /IoT Greengrass V2 Developer Guide/.
componentDeploymentSpecification_runWith :: Lens.Lens' ComponentDeploymentSpecification (Prelude.Maybe ComponentRunWith)
componentDeploymentSpecification_runWith = Lens.lens (\ComponentDeploymentSpecification' {runWith} -> runWith) (\s@ComponentDeploymentSpecification' {} a -> s {runWith = a} :: ComponentDeploymentSpecification)

-- | The configuration updates to deploy for the component. You can define
-- /reset/ updates and /merge/ updates. A reset updates the keys that you
-- specify to the default configuration for the component. A merge updates
-- the core device\'s component configuration with the keys and values that
-- you specify. The IoT Greengrass Core software applies reset updates
-- before it applies merge updates. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/update-component-configurations.html Update component configurations>
-- in the /IoT Greengrass V2 Developer Guide/.
componentDeploymentSpecification_configurationUpdate :: Lens.Lens' ComponentDeploymentSpecification (Prelude.Maybe ComponentConfigurationUpdate)
componentDeploymentSpecification_configurationUpdate = Lens.lens (\ComponentDeploymentSpecification' {configurationUpdate} -> configurationUpdate) (\s@ComponentDeploymentSpecification' {} a -> s {configurationUpdate = a} :: ComponentDeploymentSpecification)

instance
  Data.FromJSON
    ComponentDeploymentSpecification
  where
  parseJSON =
    Data.withObject
      "ComponentDeploymentSpecification"
      ( \x ->
          ComponentDeploymentSpecification'
            Prelude.<$> (x Data..:? "componentVersion")
            Prelude.<*> (x Data..:? "runWith")
            Prelude.<*> (x Data..:? "configurationUpdate")
      )

instance
  Prelude.Hashable
    ComponentDeploymentSpecification
  where
  hashWithSalt
    _salt
    ComponentDeploymentSpecification' {..} =
      _salt `Prelude.hashWithSalt` componentVersion
        `Prelude.hashWithSalt` runWith
        `Prelude.hashWithSalt` configurationUpdate

instance
  Prelude.NFData
    ComponentDeploymentSpecification
  where
  rnf ComponentDeploymentSpecification' {..} =
    Prelude.rnf componentVersion
      `Prelude.seq` Prelude.rnf runWith
      `Prelude.seq` Prelude.rnf configurationUpdate

instance Data.ToJSON ComponentDeploymentSpecification where
  toJSON ComponentDeploymentSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("componentVersion" Data..=)
              Prelude.<$> componentVersion,
            ("runWith" Data..=) Prelude.<$> runWith,
            ("configurationUpdate" Data..=)
              Prelude.<$> configurationUpdate
          ]
      )
