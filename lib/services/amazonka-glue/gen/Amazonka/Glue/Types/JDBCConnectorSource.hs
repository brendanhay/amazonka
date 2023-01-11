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
-- Module      : Amazonka.Glue.Types.JDBCConnectorSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.JDBCConnectorSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.GlueSchema
import Amazonka.Glue.Types.JDBCConnectorOptions
import qualified Amazonka.Prelude as Prelude

-- | Specifies a connector to a JDBC data source.
--
-- /See:/ 'newJDBCConnectorSource' smart constructor.
data JDBCConnectorSource = JDBCConnectorSource'
  { -- | Additional connection options for the connector.
    additionalOptions :: Prelude.Maybe JDBCConnectorOptions,
    -- | The name of the table in the data source.
    connectionTable :: Prelude.Maybe Prelude.Text,
    -- | Specifies the data schema for the custom JDBC source.
    outputSchemas :: Prelude.Maybe [GlueSchema],
    -- | The table or SQL query to get the data from. You can specify either
    -- @ConnectionTable@ or @query@, but not both.
    query :: Prelude.Maybe Prelude.Text,
    -- | The name of the data source.
    name :: Prelude.Text,
    -- | The name of the connection that is associated with the connector.
    connectionName :: Prelude.Text,
    -- | The name of a connector that assists with accessing the data store in
    -- Glue Studio.
    connectorName :: Prelude.Text,
    -- | The type of connection, such as marketplace.jdbc or custom.jdbc,
    -- designating a connection to a JDBC data store.
    connectionType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JDBCConnectorSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalOptions', 'jDBCConnectorSource_additionalOptions' - Additional connection options for the connector.
--
-- 'connectionTable', 'jDBCConnectorSource_connectionTable' - The name of the table in the data source.
--
-- 'outputSchemas', 'jDBCConnectorSource_outputSchemas' - Specifies the data schema for the custom JDBC source.
--
-- 'query', 'jDBCConnectorSource_query' - The table or SQL query to get the data from. You can specify either
-- @ConnectionTable@ or @query@, but not both.
--
-- 'name', 'jDBCConnectorSource_name' - The name of the data source.
--
-- 'connectionName', 'jDBCConnectorSource_connectionName' - The name of the connection that is associated with the connector.
--
-- 'connectorName', 'jDBCConnectorSource_connectorName' - The name of a connector that assists with accessing the data store in
-- Glue Studio.
--
-- 'connectionType', 'jDBCConnectorSource_connectionType' - The type of connection, such as marketplace.jdbc or custom.jdbc,
-- designating a connection to a JDBC data store.
newJDBCConnectorSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'connectionName'
  Prelude.Text ->
  -- | 'connectorName'
  Prelude.Text ->
  -- | 'connectionType'
  Prelude.Text ->
  JDBCConnectorSource
newJDBCConnectorSource
  pName_
  pConnectionName_
  pConnectorName_
  pConnectionType_ =
    JDBCConnectorSource'
      { additionalOptions =
          Prelude.Nothing,
        connectionTable = Prelude.Nothing,
        outputSchemas = Prelude.Nothing,
        query = Prelude.Nothing,
        name = pName_,
        connectionName = pConnectionName_,
        connectorName = pConnectorName_,
        connectionType = pConnectionType_
      }

-- | Additional connection options for the connector.
jDBCConnectorSource_additionalOptions :: Lens.Lens' JDBCConnectorSource (Prelude.Maybe JDBCConnectorOptions)
jDBCConnectorSource_additionalOptions = Lens.lens (\JDBCConnectorSource' {additionalOptions} -> additionalOptions) (\s@JDBCConnectorSource' {} a -> s {additionalOptions = a} :: JDBCConnectorSource)

-- | The name of the table in the data source.
jDBCConnectorSource_connectionTable :: Lens.Lens' JDBCConnectorSource (Prelude.Maybe Prelude.Text)
jDBCConnectorSource_connectionTable = Lens.lens (\JDBCConnectorSource' {connectionTable} -> connectionTable) (\s@JDBCConnectorSource' {} a -> s {connectionTable = a} :: JDBCConnectorSource)

-- | Specifies the data schema for the custom JDBC source.
jDBCConnectorSource_outputSchemas :: Lens.Lens' JDBCConnectorSource (Prelude.Maybe [GlueSchema])
jDBCConnectorSource_outputSchemas = Lens.lens (\JDBCConnectorSource' {outputSchemas} -> outputSchemas) (\s@JDBCConnectorSource' {} a -> s {outputSchemas = a} :: JDBCConnectorSource) Prelude.. Lens.mapping Lens.coerced

-- | The table or SQL query to get the data from. You can specify either
-- @ConnectionTable@ or @query@, but not both.
jDBCConnectorSource_query :: Lens.Lens' JDBCConnectorSource (Prelude.Maybe Prelude.Text)
jDBCConnectorSource_query = Lens.lens (\JDBCConnectorSource' {query} -> query) (\s@JDBCConnectorSource' {} a -> s {query = a} :: JDBCConnectorSource)

-- | The name of the data source.
jDBCConnectorSource_name :: Lens.Lens' JDBCConnectorSource Prelude.Text
jDBCConnectorSource_name = Lens.lens (\JDBCConnectorSource' {name} -> name) (\s@JDBCConnectorSource' {} a -> s {name = a} :: JDBCConnectorSource)

-- | The name of the connection that is associated with the connector.
jDBCConnectorSource_connectionName :: Lens.Lens' JDBCConnectorSource Prelude.Text
jDBCConnectorSource_connectionName = Lens.lens (\JDBCConnectorSource' {connectionName} -> connectionName) (\s@JDBCConnectorSource' {} a -> s {connectionName = a} :: JDBCConnectorSource)

-- | The name of a connector that assists with accessing the data store in
-- Glue Studio.
jDBCConnectorSource_connectorName :: Lens.Lens' JDBCConnectorSource Prelude.Text
jDBCConnectorSource_connectorName = Lens.lens (\JDBCConnectorSource' {connectorName} -> connectorName) (\s@JDBCConnectorSource' {} a -> s {connectorName = a} :: JDBCConnectorSource)

-- | The type of connection, such as marketplace.jdbc or custom.jdbc,
-- designating a connection to a JDBC data store.
jDBCConnectorSource_connectionType :: Lens.Lens' JDBCConnectorSource Prelude.Text
jDBCConnectorSource_connectionType = Lens.lens (\JDBCConnectorSource' {connectionType} -> connectionType) (\s@JDBCConnectorSource' {} a -> s {connectionType = a} :: JDBCConnectorSource)

instance Data.FromJSON JDBCConnectorSource where
  parseJSON =
    Data.withObject
      "JDBCConnectorSource"
      ( \x ->
          JDBCConnectorSource'
            Prelude.<$> (x Data..:? "AdditionalOptions")
            Prelude.<*> (x Data..:? "ConnectionTable")
            Prelude.<*> (x Data..:? "OutputSchemas" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Query")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "ConnectionName")
            Prelude.<*> (x Data..: "ConnectorName")
            Prelude.<*> (x Data..: "ConnectionType")
      )

instance Prelude.Hashable JDBCConnectorSource where
  hashWithSalt _salt JDBCConnectorSource' {..} =
    _salt `Prelude.hashWithSalt` additionalOptions
      `Prelude.hashWithSalt` connectionTable
      `Prelude.hashWithSalt` outputSchemas
      `Prelude.hashWithSalt` query
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` connectorName
      `Prelude.hashWithSalt` connectionType

instance Prelude.NFData JDBCConnectorSource where
  rnf JDBCConnectorSource' {..} =
    Prelude.rnf additionalOptions
      `Prelude.seq` Prelude.rnf connectionTable
      `Prelude.seq` Prelude.rnf outputSchemas
      `Prelude.seq` Prelude.rnf query
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf connectionName
      `Prelude.seq` Prelude.rnf connectorName
      `Prelude.seq` Prelude.rnf connectionType

instance Data.ToJSON JDBCConnectorSource where
  toJSON JDBCConnectorSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalOptions" Data..=)
              Prelude.<$> additionalOptions,
            ("ConnectionTable" Data..=)
              Prelude.<$> connectionTable,
            ("OutputSchemas" Data..=) Prelude.<$> outputSchemas,
            ("Query" Data..=) Prelude.<$> query,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("ConnectionName" Data..= connectionName),
            Prelude.Just ("ConnectorName" Data..= connectorName),
            Prelude.Just
              ("ConnectionType" Data..= connectionType)
          ]
      )
