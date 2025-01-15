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
-- Module      : Amazonka.AppSync.Types.DeltaSyncConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.DeltaSyncConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a Delta Sync configuration.
--
-- /See:/ 'newDeltaSyncConfig' smart constructor.
data DeltaSyncConfig = DeltaSyncConfig'
  { -- | The number of minutes that an Item is stored in the data source.
    baseTableTTL :: Prelude.Maybe Prelude.Integer,
    -- | The Delta Sync table name.
    deltaSyncTableName :: Prelude.Maybe Prelude.Text,
    -- | The number of minutes that a Delta Sync log entry is stored in the Delta
    -- Sync table.
    deltaSyncTableTTL :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeltaSyncConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseTableTTL', 'deltaSyncConfig_baseTableTTL' - The number of minutes that an Item is stored in the data source.
--
-- 'deltaSyncTableName', 'deltaSyncConfig_deltaSyncTableName' - The Delta Sync table name.
--
-- 'deltaSyncTableTTL', 'deltaSyncConfig_deltaSyncTableTTL' - The number of minutes that a Delta Sync log entry is stored in the Delta
-- Sync table.
newDeltaSyncConfig ::
  DeltaSyncConfig
newDeltaSyncConfig =
  DeltaSyncConfig'
    { baseTableTTL = Prelude.Nothing,
      deltaSyncTableName = Prelude.Nothing,
      deltaSyncTableTTL = Prelude.Nothing
    }

-- | The number of minutes that an Item is stored in the data source.
deltaSyncConfig_baseTableTTL :: Lens.Lens' DeltaSyncConfig (Prelude.Maybe Prelude.Integer)
deltaSyncConfig_baseTableTTL = Lens.lens (\DeltaSyncConfig' {baseTableTTL} -> baseTableTTL) (\s@DeltaSyncConfig' {} a -> s {baseTableTTL = a} :: DeltaSyncConfig)

-- | The Delta Sync table name.
deltaSyncConfig_deltaSyncTableName :: Lens.Lens' DeltaSyncConfig (Prelude.Maybe Prelude.Text)
deltaSyncConfig_deltaSyncTableName = Lens.lens (\DeltaSyncConfig' {deltaSyncTableName} -> deltaSyncTableName) (\s@DeltaSyncConfig' {} a -> s {deltaSyncTableName = a} :: DeltaSyncConfig)

-- | The number of minutes that a Delta Sync log entry is stored in the Delta
-- Sync table.
deltaSyncConfig_deltaSyncTableTTL :: Lens.Lens' DeltaSyncConfig (Prelude.Maybe Prelude.Integer)
deltaSyncConfig_deltaSyncTableTTL = Lens.lens (\DeltaSyncConfig' {deltaSyncTableTTL} -> deltaSyncTableTTL) (\s@DeltaSyncConfig' {} a -> s {deltaSyncTableTTL = a} :: DeltaSyncConfig)

instance Data.FromJSON DeltaSyncConfig where
  parseJSON =
    Data.withObject
      "DeltaSyncConfig"
      ( \x ->
          DeltaSyncConfig'
            Prelude.<$> (x Data..:? "baseTableTTL")
            Prelude.<*> (x Data..:? "deltaSyncTableName")
            Prelude.<*> (x Data..:? "deltaSyncTableTTL")
      )

instance Prelude.Hashable DeltaSyncConfig where
  hashWithSalt _salt DeltaSyncConfig' {..} =
    _salt
      `Prelude.hashWithSalt` baseTableTTL
      `Prelude.hashWithSalt` deltaSyncTableName
      `Prelude.hashWithSalt` deltaSyncTableTTL

instance Prelude.NFData DeltaSyncConfig where
  rnf DeltaSyncConfig' {..} =
    Prelude.rnf baseTableTTL `Prelude.seq`
      Prelude.rnf deltaSyncTableName `Prelude.seq`
        Prelude.rnf deltaSyncTableTTL

instance Data.ToJSON DeltaSyncConfig where
  toJSON DeltaSyncConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("baseTableTTL" Data..=) Prelude.<$> baseTableTTL,
            ("deltaSyncTableName" Data..=)
              Prelude.<$> deltaSyncTableName,
            ("deltaSyncTableTTL" Data..=)
              Prelude.<$> deltaSyncTableTTL
          ]
      )
