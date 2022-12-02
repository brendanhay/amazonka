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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.RunConfigurationDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.RunConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.ApplicationRestoreConfiguration
import Amazonka.KinesisAnalyticsV2.Types.FlinkRunConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes the starting properties for a Kinesis Data Analytics
-- application.
--
-- /See:/ 'newRunConfigurationDescription' smart constructor.
data RunConfigurationDescription = RunConfigurationDescription'
  { flinkRunConfigurationDescription :: Prelude.Maybe FlinkRunConfiguration,
    -- | Describes the restore behavior of a restarting application.
    applicationRestoreConfigurationDescription :: Prelude.Maybe ApplicationRestoreConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flinkRunConfigurationDescription', 'runConfigurationDescription_flinkRunConfigurationDescription' - Undocumented member.
--
-- 'applicationRestoreConfigurationDescription', 'runConfigurationDescription_applicationRestoreConfigurationDescription' - Describes the restore behavior of a restarting application.
newRunConfigurationDescription ::
  RunConfigurationDescription
newRunConfigurationDescription =
  RunConfigurationDescription'
    { flinkRunConfigurationDescription =
        Prelude.Nothing,
      applicationRestoreConfigurationDescription =
        Prelude.Nothing
    }

-- | Undocumented member.
runConfigurationDescription_flinkRunConfigurationDescription :: Lens.Lens' RunConfigurationDescription (Prelude.Maybe FlinkRunConfiguration)
runConfigurationDescription_flinkRunConfigurationDescription = Lens.lens (\RunConfigurationDescription' {flinkRunConfigurationDescription} -> flinkRunConfigurationDescription) (\s@RunConfigurationDescription' {} a -> s {flinkRunConfigurationDescription = a} :: RunConfigurationDescription)

-- | Describes the restore behavior of a restarting application.
runConfigurationDescription_applicationRestoreConfigurationDescription :: Lens.Lens' RunConfigurationDescription (Prelude.Maybe ApplicationRestoreConfiguration)
runConfigurationDescription_applicationRestoreConfigurationDescription = Lens.lens (\RunConfigurationDescription' {applicationRestoreConfigurationDescription} -> applicationRestoreConfigurationDescription) (\s@RunConfigurationDescription' {} a -> s {applicationRestoreConfigurationDescription = a} :: RunConfigurationDescription)

instance Data.FromJSON RunConfigurationDescription where
  parseJSON =
    Data.withObject
      "RunConfigurationDescription"
      ( \x ->
          RunConfigurationDescription'
            Prelude.<$> (x Data..:? "FlinkRunConfigurationDescription")
            Prelude.<*> ( x
                            Data..:? "ApplicationRestoreConfigurationDescription"
                        )
      )

instance Prelude.Hashable RunConfigurationDescription where
  hashWithSalt _salt RunConfigurationDescription' {..} =
    _salt
      `Prelude.hashWithSalt` flinkRunConfigurationDescription
      `Prelude.hashWithSalt` applicationRestoreConfigurationDescription

instance Prelude.NFData RunConfigurationDescription where
  rnf RunConfigurationDescription' {..} =
    Prelude.rnf flinkRunConfigurationDescription
      `Prelude.seq` Prelude.rnf
        applicationRestoreConfigurationDescription
