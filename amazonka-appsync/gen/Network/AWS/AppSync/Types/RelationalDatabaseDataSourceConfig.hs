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
-- Module      : Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig where

import Network.AWS.AppSync.Types.RdsHttpEndpointConfig
import Network.AWS.AppSync.Types.RelationalDatabaseSourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a relational database data source configuration.
--
-- /See:/ 'newRelationalDatabaseDataSourceConfig' smart constructor.
data RelationalDatabaseDataSourceConfig = RelationalDatabaseDataSourceConfig'
  { -- | Amazon RDS HTTP endpoint settings.
    rdsHttpEndpointConfig :: Prelude.Maybe RdsHttpEndpointConfig,
    -- | Source type for the relational database.
    --
    -- -   __RDS_HTTP_ENDPOINT__: The relational database source type is an
    --     Amazon RDS HTTP endpoint.
    relationalDatabaseSourceType :: Prelude.Maybe RelationalDatabaseSourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
--     Amazon RDS HTTP endpoint.
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
--     Amazon RDS HTTP endpoint.
relationalDatabaseDataSourceConfig_relationalDatabaseSourceType :: Lens.Lens' RelationalDatabaseDataSourceConfig (Prelude.Maybe RelationalDatabaseSourceType)
relationalDatabaseDataSourceConfig_relationalDatabaseSourceType = Lens.lens (\RelationalDatabaseDataSourceConfig' {relationalDatabaseSourceType} -> relationalDatabaseSourceType) (\s@RelationalDatabaseDataSourceConfig' {} a -> s {relationalDatabaseSourceType = a} :: RelationalDatabaseDataSourceConfig)

instance
  Prelude.FromJSON
    RelationalDatabaseDataSourceConfig
  where
  parseJSON =
    Prelude.withObject
      "RelationalDatabaseDataSourceConfig"
      ( \x ->
          RelationalDatabaseDataSourceConfig'
            Prelude.<$> (x Prelude..:? "rdsHttpEndpointConfig")
            Prelude.<*> (x Prelude..:? "relationalDatabaseSourceType")
      )

instance
  Prelude.Hashable
    RelationalDatabaseDataSourceConfig

instance
  Prelude.NFData
    RelationalDatabaseDataSourceConfig

instance
  Prelude.ToJSON
    RelationalDatabaseDataSourceConfig
  where
  toJSON RelationalDatabaseDataSourceConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("rdsHttpEndpointConfig" Prelude..=)
              Prelude.<$> rdsHttpEndpointConfig,
            ("relationalDatabaseSourceType" Prelude..=)
              Prelude.<$> relationalDatabaseSourceType
          ]
      )
