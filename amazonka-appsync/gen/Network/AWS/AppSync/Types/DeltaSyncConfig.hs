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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a Delta Sync configuration.
--
-- /See:/ 'newDeltaSyncConfig' smart constructor.
data DeltaSyncConfig = DeltaSyncConfig'
  { -- | The number of minutes an Item is stored in the datasource.
    baseTableTTL :: Core.Maybe Core.Integer,
    -- | The Delta Sync table name.
    deltaSyncTableName :: Core.Maybe Core.Text,
    -- | The number of minutes a Delta Sync log entry is stored in the Delta Sync
    -- table.
    deltaSyncTableTTL :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { baseTableTTL = Core.Nothing,
      deltaSyncTableName = Core.Nothing,
      deltaSyncTableTTL = Core.Nothing
    }

-- | The number of minutes an Item is stored in the datasource.
deltaSyncConfig_baseTableTTL :: Lens.Lens' DeltaSyncConfig (Core.Maybe Core.Integer)
deltaSyncConfig_baseTableTTL = Lens.lens (\DeltaSyncConfig' {baseTableTTL} -> baseTableTTL) (\s@DeltaSyncConfig' {} a -> s {baseTableTTL = a} :: DeltaSyncConfig)

-- | The Delta Sync table name.
deltaSyncConfig_deltaSyncTableName :: Lens.Lens' DeltaSyncConfig (Core.Maybe Core.Text)
deltaSyncConfig_deltaSyncTableName = Lens.lens (\DeltaSyncConfig' {deltaSyncTableName} -> deltaSyncTableName) (\s@DeltaSyncConfig' {} a -> s {deltaSyncTableName = a} :: DeltaSyncConfig)

-- | The number of minutes a Delta Sync log entry is stored in the Delta Sync
-- table.
deltaSyncConfig_deltaSyncTableTTL :: Lens.Lens' DeltaSyncConfig (Core.Maybe Core.Integer)
deltaSyncConfig_deltaSyncTableTTL = Lens.lens (\DeltaSyncConfig' {deltaSyncTableTTL} -> deltaSyncTableTTL) (\s@DeltaSyncConfig' {} a -> s {deltaSyncTableTTL = a} :: DeltaSyncConfig)

instance Core.FromJSON DeltaSyncConfig where
  parseJSON =
    Core.withObject
      "DeltaSyncConfig"
      ( \x ->
          DeltaSyncConfig'
            Core.<$> (x Core..:? "baseTableTTL")
            Core.<*> (x Core..:? "deltaSyncTableName")
            Core.<*> (x Core..:? "deltaSyncTableTTL")
      )

instance Core.Hashable DeltaSyncConfig

instance Core.NFData DeltaSyncConfig

instance Core.ToJSON DeltaSyncConfig where
  toJSON DeltaSyncConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("baseTableTTL" Core..=) Core.<$> baseTableTTL,
            ("deltaSyncTableName" Core..=)
              Core.<$> deltaSyncTableName,
            ("deltaSyncTableTTL" Core..=)
              Core.<$> deltaSyncTableTTL
          ]
      )
