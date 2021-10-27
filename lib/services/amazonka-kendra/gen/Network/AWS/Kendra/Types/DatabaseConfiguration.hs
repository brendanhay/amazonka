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
-- Module      : Network.AWS.Kendra.Types.DatabaseConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.DatabaseConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.AclConfiguration
import Network.AWS.Kendra.Types.ColumnConfiguration
import Network.AWS.Kendra.Types.ConnectionConfiguration
import Network.AWS.Kendra.Types.DataSourceVpcConfiguration
import Network.AWS.Kendra.Types.DatabaseEngineType
import Network.AWS.Kendra.Types.SqlConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the information necessary to connect a database to an index.
--
-- /See:/ 'newDatabaseConfiguration' smart constructor.
data DatabaseConfiguration = DatabaseConfiguration'
  { -- | Provides information about how Amazon Kendra uses quote marks around SQL
    -- identifiers when querying a database data source.
    sqlConfiguration :: Prelude.Maybe SqlConfiguration,
    -- | Information about the database column that provides information for user
    -- context filtering.
    aclConfiguration :: Prelude.Maybe AclConfiguration,
    vpcConfiguration :: Prelude.Maybe DataSourceVpcConfiguration,
    -- | The type of database engine that runs the database.
    databaseEngineType :: DatabaseEngineType,
    -- | The information necessary to connect to a database.
    connectionConfiguration :: ConnectionConfiguration,
    -- | Information about where the index should get the document information
    -- from the database.
    columnConfiguration :: ColumnConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatabaseConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sqlConfiguration', 'databaseConfiguration_sqlConfiguration' - Provides information about how Amazon Kendra uses quote marks around SQL
-- identifiers when querying a database data source.
--
-- 'aclConfiguration', 'databaseConfiguration_aclConfiguration' - Information about the database column that provides information for user
-- context filtering.
--
-- 'vpcConfiguration', 'databaseConfiguration_vpcConfiguration' - Undocumented member.
--
-- 'databaseEngineType', 'databaseConfiguration_databaseEngineType' - The type of database engine that runs the database.
--
-- 'connectionConfiguration', 'databaseConfiguration_connectionConfiguration' - The information necessary to connect to a database.
--
-- 'columnConfiguration', 'databaseConfiguration_columnConfiguration' - Information about where the index should get the document information
-- from the database.
newDatabaseConfiguration ::
  -- | 'databaseEngineType'
  DatabaseEngineType ->
  -- | 'connectionConfiguration'
  ConnectionConfiguration ->
  -- | 'columnConfiguration'
  ColumnConfiguration ->
  DatabaseConfiguration
newDatabaseConfiguration
  pDatabaseEngineType_
  pConnectionConfiguration_
  pColumnConfiguration_ =
    DatabaseConfiguration'
      { sqlConfiguration =
          Prelude.Nothing,
        aclConfiguration = Prelude.Nothing,
        vpcConfiguration = Prelude.Nothing,
        databaseEngineType = pDatabaseEngineType_,
        connectionConfiguration = pConnectionConfiguration_,
        columnConfiguration = pColumnConfiguration_
      }

-- | Provides information about how Amazon Kendra uses quote marks around SQL
-- identifiers when querying a database data source.
databaseConfiguration_sqlConfiguration :: Lens.Lens' DatabaseConfiguration (Prelude.Maybe SqlConfiguration)
databaseConfiguration_sqlConfiguration = Lens.lens (\DatabaseConfiguration' {sqlConfiguration} -> sqlConfiguration) (\s@DatabaseConfiguration' {} a -> s {sqlConfiguration = a} :: DatabaseConfiguration)

-- | Information about the database column that provides information for user
-- context filtering.
databaseConfiguration_aclConfiguration :: Lens.Lens' DatabaseConfiguration (Prelude.Maybe AclConfiguration)
databaseConfiguration_aclConfiguration = Lens.lens (\DatabaseConfiguration' {aclConfiguration} -> aclConfiguration) (\s@DatabaseConfiguration' {} a -> s {aclConfiguration = a} :: DatabaseConfiguration)

-- | Undocumented member.
databaseConfiguration_vpcConfiguration :: Lens.Lens' DatabaseConfiguration (Prelude.Maybe DataSourceVpcConfiguration)
databaseConfiguration_vpcConfiguration = Lens.lens (\DatabaseConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@DatabaseConfiguration' {} a -> s {vpcConfiguration = a} :: DatabaseConfiguration)

-- | The type of database engine that runs the database.
databaseConfiguration_databaseEngineType :: Lens.Lens' DatabaseConfiguration DatabaseEngineType
databaseConfiguration_databaseEngineType = Lens.lens (\DatabaseConfiguration' {databaseEngineType} -> databaseEngineType) (\s@DatabaseConfiguration' {} a -> s {databaseEngineType = a} :: DatabaseConfiguration)

-- | The information necessary to connect to a database.
databaseConfiguration_connectionConfiguration :: Lens.Lens' DatabaseConfiguration ConnectionConfiguration
databaseConfiguration_connectionConfiguration = Lens.lens (\DatabaseConfiguration' {connectionConfiguration} -> connectionConfiguration) (\s@DatabaseConfiguration' {} a -> s {connectionConfiguration = a} :: DatabaseConfiguration)

-- | Information about where the index should get the document information
-- from the database.
databaseConfiguration_columnConfiguration :: Lens.Lens' DatabaseConfiguration ColumnConfiguration
databaseConfiguration_columnConfiguration = Lens.lens (\DatabaseConfiguration' {columnConfiguration} -> columnConfiguration) (\s@DatabaseConfiguration' {} a -> s {columnConfiguration = a} :: DatabaseConfiguration)

instance Core.FromJSON DatabaseConfiguration where
  parseJSON =
    Core.withObject
      "DatabaseConfiguration"
      ( \x ->
          DatabaseConfiguration'
            Prelude.<$> (x Core..:? "SqlConfiguration")
            Prelude.<*> (x Core..:? "AclConfiguration")
            Prelude.<*> (x Core..:? "VpcConfiguration")
            Prelude.<*> (x Core..: "DatabaseEngineType")
            Prelude.<*> (x Core..: "ConnectionConfiguration")
            Prelude.<*> (x Core..: "ColumnConfiguration")
      )

instance Prelude.Hashable DatabaseConfiguration

instance Prelude.NFData DatabaseConfiguration

instance Core.ToJSON DatabaseConfiguration where
  toJSON DatabaseConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SqlConfiguration" Core..=)
              Prelude.<$> sqlConfiguration,
            ("AclConfiguration" Core..=)
              Prelude.<$> aclConfiguration,
            ("VpcConfiguration" Core..=)
              Prelude.<$> vpcConfiguration,
            Prelude.Just
              ("DatabaseEngineType" Core..= databaseEngineType),
            Prelude.Just
              ( "ConnectionConfiguration"
                  Core..= connectionConfiguration
              ),
            Prelude.Just
              ("ColumnConfiguration" Core..= columnConfiguration)
          ]
      )
