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
-- Module      : Amazonka.AppSync.Types.RelationalDatabaseDataSourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.RelationalDatabaseDataSourceConfig where

import Amazonka.AppSync.Types.RdsHttpEndpointConfig
import Amazonka.AppSync.Types.RelationalDatabaseSourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a relational database data source configuration.
--
-- /See:/ 'newRelationalDatabaseDataSourceConfig' smart constructor.
data RelationalDatabaseDataSourceConfig = RelationalDatabaseDataSourceConfig'
  { -- | Amazon RDS HTTP endpoint settings.
    rdsHttpEndpointConfig :: Prelude.Maybe RdsHttpEndpointConfig,
    -- | Source type for the relational database.
    --
    -- -   __RDS_HTTP_ENDPOINT__: The relational database source type is an
    --     Amazon Relational Database Service (Amazon RDS) HTTP endpoint.
    relationalDatabaseSourceType :: Prelude.Maybe RelationalDatabaseSourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelationalDatabaseDataSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rdsHttpEndpointConfig', 'relationalDatabaseDataSourceConfig_rdsHttpEndpointConfig' - Amazon RDS HTTP endpoint settings.
--
-- 'relationalDatabaseSourceType', 'relationalDatabaseDataSourceConfig_relationalDatabaseSourceType' - Source type for the relational database.
--
-- -   __RDS_HTTP_ENDPOINT__: The relational database source type is an
--     Amazon Relational Database Service (Amazon RDS) HTTP endpoint.
newRelationalDatabaseDataSourceConfig ::
  RelationalDatabaseDataSourceConfig
newRelationalDatabaseDataSourceConfig =
  RelationalDatabaseDataSourceConfig'
    { rdsHttpEndpointConfig =
        Prelude.Nothing,
      relationalDatabaseSourceType =
        Prelude.Nothing
    }

-- | Amazon RDS HTTP endpoint settings.
relationalDatabaseDataSourceConfig_rdsHttpEndpointConfig :: Lens.Lens' RelationalDatabaseDataSourceConfig (Prelude.Maybe RdsHttpEndpointConfig)
relationalDatabaseDataSourceConfig_rdsHttpEndpointConfig = Lens.lens (\RelationalDatabaseDataSourceConfig' {rdsHttpEndpointConfig} -> rdsHttpEndpointConfig) (\s@RelationalDatabaseDataSourceConfig' {} a -> s {rdsHttpEndpointConfig = a} :: RelationalDatabaseDataSourceConfig)

-- | Source type for the relational database.
--
-- -   __RDS_HTTP_ENDPOINT__: The relational database source type is an
--     Amazon Relational Database Service (Amazon RDS) HTTP endpoint.
relationalDatabaseDataSourceConfig_relationalDatabaseSourceType :: Lens.Lens' RelationalDatabaseDataSourceConfig (Prelude.Maybe RelationalDatabaseSourceType)
relationalDatabaseDataSourceConfig_relationalDatabaseSourceType = Lens.lens (\RelationalDatabaseDataSourceConfig' {relationalDatabaseSourceType} -> relationalDatabaseSourceType) (\s@RelationalDatabaseDataSourceConfig' {} a -> s {relationalDatabaseSourceType = a} :: RelationalDatabaseDataSourceConfig)

instance
  Data.FromJSON
    RelationalDatabaseDataSourceConfig
  where
  parseJSON =
    Data.withObject
      "RelationalDatabaseDataSourceConfig"
      ( \x ->
          RelationalDatabaseDataSourceConfig'
            Prelude.<$> (x Data..:? "rdsHttpEndpointConfig")
            Prelude.<*> (x Data..:? "relationalDatabaseSourceType")
      )

instance
  Prelude.Hashable
    RelationalDatabaseDataSourceConfig
  where
  hashWithSalt
    _salt
    RelationalDatabaseDataSourceConfig' {..} =
      _salt
        `Prelude.hashWithSalt` rdsHttpEndpointConfig
        `Prelude.hashWithSalt` relationalDatabaseSourceType

instance
  Prelude.NFData
    RelationalDatabaseDataSourceConfig
  where
  rnf RelationalDatabaseDataSourceConfig' {..} =
    Prelude.rnf rdsHttpEndpointConfig
      `Prelude.seq` Prelude.rnf relationalDatabaseSourceType

instance
  Data.ToJSON
    RelationalDatabaseDataSourceConfig
  where
  toJSON RelationalDatabaseDataSourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("rdsHttpEndpointConfig" Data..=)
              Prelude.<$> rdsHttpEndpointConfig,
            ("relationalDatabaseSourceType" Data..=)
              Prelude.<$> relationalDatabaseSourceType
          ]
      )
