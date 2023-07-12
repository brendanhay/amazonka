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
-- Module      : Amazonka.QuickSight.Types.SnowflakeParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SnowflakeParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for Snowflake.
--
-- /See:/ 'newSnowflakeParameters' smart constructor.
data SnowflakeParameters = SnowflakeParameters'
  { -- | Host.
    host :: Prelude.Text,
    -- | Database.
    database :: Prelude.Text,
    -- | Warehouse.
    warehouse :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnowflakeParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'snowflakeParameters_host' - Host.
--
-- 'database', 'snowflakeParameters_database' - Database.
--
-- 'warehouse', 'snowflakeParameters_warehouse' - Warehouse.
newSnowflakeParameters ::
  -- | 'host'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'warehouse'
  Prelude.Text ->
  SnowflakeParameters
newSnowflakeParameters pHost_ pDatabase_ pWarehouse_ =
  SnowflakeParameters'
    { host = pHost_,
      database = pDatabase_,
      warehouse = pWarehouse_
    }

-- | Host.
snowflakeParameters_host :: Lens.Lens' SnowflakeParameters Prelude.Text
snowflakeParameters_host = Lens.lens (\SnowflakeParameters' {host} -> host) (\s@SnowflakeParameters' {} a -> s {host = a} :: SnowflakeParameters)

-- | Database.
snowflakeParameters_database :: Lens.Lens' SnowflakeParameters Prelude.Text
snowflakeParameters_database = Lens.lens (\SnowflakeParameters' {database} -> database) (\s@SnowflakeParameters' {} a -> s {database = a} :: SnowflakeParameters)

-- | Warehouse.
snowflakeParameters_warehouse :: Lens.Lens' SnowflakeParameters Prelude.Text
snowflakeParameters_warehouse = Lens.lens (\SnowflakeParameters' {warehouse} -> warehouse) (\s@SnowflakeParameters' {} a -> s {warehouse = a} :: SnowflakeParameters)

instance Data.FromJSON SnowflakeParameters where
  parseJSON =
    Data.withObject
      "SnowflakeParameters"
      ( \x ->
          SnowflakeParameters'
            Prelude.<$> (x Data..: "Host")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Warehouse")
      )

instance Prelude.Hashable SnowflakeParameters where
  hashWithSalt _salt SnowflakeParameters' {..} =
    _salt
      `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` warehouse

instance Prelude.NFData SnowflakeParameters where
  rnf SnowflakeParameters' {..} =
    Prelude.rnf host
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf warehouse

instance Data.ToJSON SnowflakeParameters where
  toJSON SnowflakeParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Host" Data..= host),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Warehouse" Data..= warehouse)
          ]
      )
