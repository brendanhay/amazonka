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
-- Module      : Amazonka.IoTFleetWise.Types.TimestreamResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.TimestreamResources where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The registered Amazon Timestream resources that Amazon Web Services IoT
-- FleetWise edge agent software can transfer your vehicle data to.
--
-- /See:/ 'newTimestreamResources' smart constructor.
data TimestreamResources = TimestreamResources'
  { -- | The name of the registered Amazon Timestream database.
    timestreamDatabaseName :: Prelude.Text,
    -- | The name of the registered Amazon Timestream database table.
    timestreamTableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimestreamResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestreamDatabaseName', 'timestreamResources_timestreamDatabaseName' - The name of the registered Amazon Timestream database.
--
-- 'timestreamTableName', 'timestreamResources_timestreamTableName' - The name of the registered Amazon Timestream database table.
newTimestreamResources ::
  -- | 'timestreamDatabaseName'
  Prelude.Text ->
  -- | 'timestreamTableName'
  Prelude.Text ->
  TimestreamResources
newTimestreamResources
  pTimestreamDatabaseName_
  pTimestreamTableName_ =
    TimestreamResources'
      { timestreamDatabaseName =
          pTimestreamDatabaseName_,
        timestreamTableName = pTimestreamTableName_
      }

-- | The name of the registered Amazon Timestream database.
timestreamResources_timestreamDatabaseName :: Lens.Lens' TimestreamResources Prelude.Text
timestreamResources_timestreamDatabaseName = Lens.lens (\TimestreamResources' {timestreamDatabaseName} -> timestreamDatabaseName) (\s@TimestreamResources' {} a -> s {timestreamDatabaseName = a} :: TimestreamResources)

-- | The name of the registered Amazon Timestream database table.
timestreamResources_timestreamTableName :: Lens.Lens' TimestreamResources Prelude.Text
timestreamResources_timestreamTableName = Lens.lens (\TimestreamResources' {timestreamTableName} -> timestreamTableName) (\s@TimestreamResources' {} a -> s {timestreamTableName = a} :: TimestreamResources)

instance Data.FromJSON TimestreamResources where
  parseJSON =
    Data.withObject
      "TimestreamResources"
      ( \x ->
          TimestreamResources'
            Prelude.<$> (x Data..: "timestreamDatabaseName")
            Prelude.<*> (x Data..: "timestreamTableName")
      )

instance Prelude.Hashable TimestreamResources where
  hashWithSalt _salt TimestreamResources' {..} =
    _salt
      `Prelude.hashWithSalt` timestreamDatabaseName
      `Prelude.hashWithSalt` timestreamTableName

instance Prelude.NFData TimestreamResources where
  rnf TimestreamResources' {..} =
    Prelude.rnf timestreamDatabaseName
      `Prelude.seq` Prelude.rnf timestreamTableName

instance Data.ToJSON TimestreamResources where
  toJSON TimestreamResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "timestreamDatabaseName"
                  Data..= timestreamDatabaseName
              ),
            Prelude.Just
              ("timestreamTableName" Data..= timestreamTableName)
          ]
      )
