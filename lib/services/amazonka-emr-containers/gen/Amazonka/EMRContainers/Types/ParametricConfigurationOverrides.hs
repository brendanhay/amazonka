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
-- Module      : Amazonka.EMRContainers.Types.ParametricConfigurationOverrides
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.ParametricConfigurationOverrides where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types.Configuration
import Amazonka.EMRContainers.Types.ParametricMonitoringConfiguration
import qualified Amazonka.Prelude as Prelude

-- | A configuration specification to be used to override existing
-- configurations. This data type allows job template parameters to be
-- specified within.
--
-- /See:/ 'newParametricConfigurationOverrides' smart constructor.
data ParametricConfigurationOverrides = ParametricConfigurationOverrides'
  { -- | The configurations for the application running by the job run.
    applicationConfiguration :: Prelude.Maybe [Configuration],
    -- | The configurations for monitoring.
    monitoringConfiguration :: Prelude.Maybe ParametricMonitoringConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParametricConfigurationOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationConfiguration', 'parametricConfigurationOverrides_applicationConfiguration' - The configurations for the application running by the job run.
--
-- 'monitoringConfiguration', 'parametricConfigurationOverrides_monitoringConfiguration' - The configurations for monitoring.
newParametricConfigurationOverrides ::
  ParametricConfigurationOverrides
newParametricConfigurationOverrides =
  ParametricConfigurationOverrides'
    { applicationConfiguration =
        Prelude.Nothing,
      monitoringConfiguration = Prelude.Nothing
    }

-- | The configurations for the application running by the job run.
parametricConfigurationOverrides_applicationConfiguration :: Lens.Lens' ParametricConfigurationOverrides (Prelude.Maybe [Configuration])
parametricConfigurationOverrides_applicationConfiguration = Lens.lens (\ParametricConfigurationOverrides' {applicationConfiguration} -> applicationConfiguration) (\s@ParametricConfigurationOverrides' {} a -> s {applicationConfiguration = a} :: ParametricConfigurationOverrides) Prelude.. Lens.mapping Lens.coerced

-- | The configurations for monitoring.
parametricConfigurationOverrides_monitoringConfiguration :: Lens.Lens' ParametricConfigurationOverrides (Prelude.Maybe ParametricMonitoringConfiguration)
parametricConfigurationOverrides_monitoringConfiguration = Lens.lens (\ParametricConfigurationOverrides' {monitoringConfiguration} -> monitoringConfiguration) (\s@ParametricConfigurationOverrides' {} a -> s {monitoringConfiguration = a} :: ParametricConfigurationOverrides)

instance
  Data.FromJSON
    ParametricConfigurationOverrides
  where
  parseJSON =
    Data.withObject
      "ParametricConfigurationOverrides"
      ( \x ->
          ParametricConfigurationOverrides'
            Prelude.<$> ( x
                            Data..:? "applicationConfiguration"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "monitoringConfiguration")
      )

instance
  Prelude.Hashable
    ParametricConfigurationOverrides
  where
  hashWithSalt
    _salt
    ParametricConfigurationOverrides' {..} =
      _salt
        `Prelude.hashWithSalt` applicationConfiguration
        `Prelude.hashWithSalt` monitoringConfiguration

instance
  Prelude.NFData
    ParametricConfigurationOverrides
  where
  rnf ParametricConfigurationOverrides' {..} =
    Prelude.rnf applicationConfiguration
      `Prelude.seq` Prelude.rnf monitoringConfiguration

instance Data.ToJSON ParametricConfigurationOverrides where
  toJSON ParametricConfigurationOverrides' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("applicationConfiguration" Data..=)
              Prelude.<$> applicationConfiguration,
            ("monitoringConfiguration" Data..=)
              Prelude.<$> monitoringConfiguration
          ]
      )
