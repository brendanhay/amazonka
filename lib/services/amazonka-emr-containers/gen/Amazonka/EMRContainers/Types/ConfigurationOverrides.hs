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
-- Module      : Amazonka.EMRContainers.Types.ConfigurationOverrides
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.ConfigurationOverrides where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types.Configuration
import Amazonka.EMRContainers.Types.MonitoringConfiguration
import qualified Amazonka.Prelude as Prelude

-- | A configuration specification to be used to override existing
-- configurations.
--
-- /See:/ 'newConfigurationOverrides' smart constructor.
data ConfigurationOverrides = ConfigurationOverrides'
  { -- | The configurations for the application running by the job run.
    applicationConfiguration :: Prelude.Maybe [Configuration],
    -- | The configurations for monitoring.
    monitoringConfiguration :: Prelude.Maybe MonitoringConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationConfiguration', 'configurationOverrides_applicationConfiguration' - The configurations for the application running by the job run.
--
-- 'monitoringConfiguration', 'configurationOverrides_monitoringConfiguration' - The configurations for monitoring.
newConfigurationOverrides ::
  ConfigurationOverrides
newConfigurationOverrides =
  ConfigurationOverrides'
    { applicationConfiguration =
        Prelude.Nothing,
      monitoringConfiguration = Prelude.Nothing
    }

-- | The configurations for the application running by the job run.
configurationOverrides_applicationConfiguration :: Lens.Lens' ConfigurationOverrides (Prelude.Maybe [Configuration])
configurationOverrides_applicationConfiguration = Lens.lens (\ConfigurationOverrides' {applicationConfiguration} -> applicationConfiguration) (\s@ConfigurationOverrides' {} a -> s {applicationConfiguration = a} :: ConfigurationOverrides) Prelude.. Lens.mapping Lens.coerced

-- | The configurations for monitoring.
configurationOverrides_monitoringConfiguration :: Lens.Lens' ConfigurationOverrides (Prelude.Maybe MonitoringConfiguration)
configurationOverrides_monitoringConfiguration = Lens.lens (\ConfigurationOverrides' {monitoringConfiguration} -> monitoringConfiguration) (\s@ConfigurationOverrides' {} a -> s {monitoringConfiguration = a} :: ConfigurationOverrides)

instance Data.FromJSON ConfigurationOverrides where
  parseJSON =
    Data.withObject
      "ConfigurationOverrides"
      ( \x ->
          ConfigurationOverrides'
            Prelude.<$> ( x Data..:? "applicationConfiguration"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "monitoringConfiguration")
      )

instance Prelude.Hashable ConfigurationOverrides where
  hashWithSalt _salt ConfigurationOverrides' {..} =
    _salt
      `Prelude.hashWithSalt` applicationConfiguration
      `Prelude.hashWithSalt` monitoringConfiguration

instance Prelude.NFData ConfigurationOverrides where
  rnf ConfigurationOverrides' {..} =
    Prelude.rnf applicationConfiguration
      `Prelude.seq` Prelude.rnf monitoringConfiguration

instance Data.ToJSON ConfigurationOverrides where
  toJSON ConfigurationOverrides' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("applicationConfiguration" Data..=)
              Prelude.<$> applicationConfiguration,
            ("monitoringConfiguration" Data..=)
              Prelude.<$> monitoringConfiguration
          ]
      )
