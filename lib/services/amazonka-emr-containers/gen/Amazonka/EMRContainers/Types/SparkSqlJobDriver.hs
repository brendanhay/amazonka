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
-- Module      : Amazonka.EMRContainers.Types.SparkSqlJobDriver
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.SparkSqlJobDriver where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The job driver for job type.
--
-- /See:/ 'newSparkSqlJobDriver' smart constructor.
data SparkSqlJobDriver = SparkSqlJobDriver'
  { -- | The SQL file to be executed.
    entryPoint :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Spark parameters to be included in the Spark SQL command.
    sparkSqlParameters :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SparkSqlJobDriver' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entryPoint', 'sparkSqlJobDriver_entryPoint' - The SQL file to be executed.
--
-- 'sparkSqlParameters', 'sparkSqlJobDriver_sparkSqlParameters' - The Spark parameters to be included in the Spark SQL command.
newSparkSqlJobDriver ::
  SparkSqlJobDriver
newSparkSqlJobDriver =
  SparkSqlJobDriver'
    { entryPoint = Prelude.Nothing,
      sparkSqlParameters = Prelude.Nothing
    }

-- | The SQL file to be executed.
sparkSqlJobDriver_entryPoint :: Lens.Lens' SparkSqlJobDriver (Prelude.Maybe Prelude.Text)
sparkSqlJobDriver_entryPoint = Lens.lens (\SparkSqlJobDriver' {entryPoint} -> entryPoint) (\s@SparkSqlJobDriver' {} a -> s {entryPoint = a} :: SparkSqlJobDriver) Prelude.. Lens.mapping Data._Sensitive

-- | The Spark parameters to be included in the Spark SQL command.
sparkSqlJobDriver_sparkSqlParameters :: Lens.Lens' SparkSqlJobDriver (Prelude.Maybe Prelude.Text)
sparkSqlJobDriver_sparkSqlParameters = Lens.lens (\SparkSqlJobDriver' {sparkSqlParameters} -> sparkSqlParameters) (\s@SparkSqlJobDriver' {} a -> s {sparkSqlParameters = a} :: SparkSqlJobDriver) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON SparkSqlJobDriver where
  parseJSON =
    Data.withObject
      "SparkSqlJobDriver"
      ( \x ->
          SparkSqlJobDriver'
            Prelude.<$> (x Data..:? "entryPoint")
            Prelude.<*> (x Data..:? "sparkSqlParameters")
      )

instance Prelude.Hashable SparkSqlJobDriver where
  hashWithSalt _salt SparkSqlJobDriver' {..} =
    _salt
      `Prelude.hashWithSalt` entryPoint
      `Prelude.hashWithSalt` sparkSqlParameters

instance Prelude.NFData SparkSqlJobDriver where
  rnf SparkSqlJobDriver' {..} =
    Prelude.rnf entryPoint `Prelude.seq`
      Prelude.rnf sparkSqlParameters

instance Data.ToJSON SparkSqlJobDriver where
  toJSON SparkSqlJobDriver' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("entryPoint" Data..=) Prelude.<$> entryPoint,
            ("sparkSqlParameters" Data..=)
              Prelude.<$> sparkSqlParameters
          ]
      )
