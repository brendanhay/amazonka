{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AppSync.Types.DeltaSyncConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.DeltaSyncConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a Delta Sync configuration.
--
-- /See:/ 'newDeltaSyncConfig' smart constructor.
data DeltaSyncConfig = DeltaSyncConfig'
  { -- | The number of minutes an Item is stored in the datasource.
    baseTableTTL :: Prelude.Maybe Prelude.Integer,
    -- | The Delta Sync table name.
    deltaSyncTableName :: Prelude.Maybe Prelude.Text,
    -- | The number of minutes a Delta Sync log entry is stored in the Delta Sync
    -- table.
    deltaSyncTableTTL :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeltaSyncConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseTableTTL', 'deltaSyncConfig_baseTableTTL' - The number of minutes an Item is stored in the datasource.
--
-- 'deltaSyncTableName', 'deltaSyncConfig_deltaSyncTableName' - The Delta Sync table name.
--
-- 'deltaSyncTableTTL', 'deltaSyncConfig_deltaSyncTableTTL' - The number of minutes a Delta Sync log entry is stored in the Delta Sync
-- table.
newDeltaSyncConfig ::
  DeltaSyncConfig
newDeltaSyncConfig =
  DeltaSyncConfig'
    { baseTableTTL = Prelude.Nothing,
      deltaSyncTableName = Prelude.Nothing,
      deltaSyncTableTTL = Prelude.Nothing
    }

-- | The number of minutes an Item is stored in the datasource.
deltaSyncConfig_baseTableTTL :: Lens.Lens' DeltaSyncConfig (Prelude.Maybe Prelude.Integer)
deltaSyncConfig_baseTableTTL = Lens.lens (\DeltaSyncConfig' {baseTableTTL} -> baseTableTTL) (\s@DeltaSyncConfig' {} a -> s {baseTableTTL = a} :: DeltaSyncConfig)

-- | The Delta Sync table name.
deltaSyncConfig_deltaSyncTableName :: Lens.Lens' DeltaSyncConfig (Prelude.Maybe Prelude.Text)
deltaSyncConfig_deltaSyncTableName = Lens.lens (\DeltaSyncConfig' {deltaSyncTableName} -> deltaSyncTableName) (\s@DeltaSyncConfig' {} a -> s {deltaSyncTableName = a} :: DeltaSyncConfig)

-- | The number of minutes a Delta Sync log entry is stored in the Delta Sync
-- table.
deltaSyncConfig_deltaSyncTableTTL :: Lens.Lens' DeltaSyncConfig (Prelude.Maybe Prelude.Integer)
deltaSyncConfig_deltaSyncTableTTL = Lens.lens (\DeltaSyncConfig' {deltaSyncTableTTL} -> deltaSyncTableTTL) (\s@DeltaSyncConfig' {} a -> s {deltaSyncTableTTL = a} :: DeltaSyncConfig)

instance Prelude.FromJSON DeltaSyncConfig where
  parseJSON =
    Prelude.withObject
      "DeltaSyncConfig"
      ( \x ->
          DeltaSyncConfig'
            Prelude.<$> (x Prelude..:? "baseTableTTL")
            Prelude.<*> (x Prelude..:? "deltaSyncTableName")
            Prelude.<*> (x Prelude..:? "deltaSyncTableTTL")
      )

instance Prelude.Hashable DeltaSyncConfig

instance Prelude.NFData DeltaSyncConfig

instance Prelude.ToJSON DeltaSyncConfig where
  toJSON DeltaSyncConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("baseTableTTL" Prelude..=)
              Prelude.<$> baseTableTTL,
            ("deltaSyncTableName" Prelude..=)
              Prelude.<$> deltaSyncTableName,
            ("deltaSyncTableTTL" Prelude..=)
              Prelude.<$> deltaSyncTableTTL
          ]
      )
