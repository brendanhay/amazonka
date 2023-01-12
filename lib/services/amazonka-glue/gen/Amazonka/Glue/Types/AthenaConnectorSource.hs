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
-- Module      : Amazonka.Glue.Types.AthenaConnectorSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.AthenaConnectorSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.GlueSchema
import qualified Amazonka.Prelude as Prelude

-- | Specifies a connector to an Amazon Athena data source.
--
-- /See:/ 'newAthenaConnectorSource' smart constructor.
data AthenaConnectorSource = AthenaConnectorSource'
  { -- | The name of the table in the data source.
    connectionTable :: Prelude.Maybe Prelude.Text,
    -- | Specifies the data schema for the custom Athena source.
    outputSchemas :: Prelude.Maybe [GlueSchema],
    -- | The name of the data source.
    name :: Prelude.Text,
    -- | The name of the connection that is associated with the connector.
    connectionName :: Prelude.Text,
    -- | The name of a connector that assists with accessing the data store in
    -- Glue Studio.
    connectorName :: Prelude.Text,
    -- | The type of connection, such as marketplace.athena or custom.athena,
    -- designating a connection to an Amazon Athena data store.
    connectionType :: Prelude.Text,
    -- | The name of the Cloudwatch log group to read from. For example,
    -- @\/aws-glue\/jobs\/output@.
    schemaName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AthenaConnectorSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionTable', 'athenaConnectorSource_connectionTable' - The name of the table in the data source.
--
-- 'outputSchemas', 'athenaConnectorSource_outputSchemas' - Specifies the data schema for the custom Athena source.
--
-- 'name', 'athenaConnectorSource_name' - The name of the data source.
--
-- 'connectionName', 'athenaConnectorSource_connectionName' - The name of the connection that is associated with the connector.
--
-- 'connectorName', 'athenaConnectorSource_connectorName' - The name of a connector that assists with accessing the data store in
-- Glue Studio.
--
-- 'connectionType', 'athenaConnectorSource_connectionType' - The type of connection, such as marketplace.athena or custom.athena,
-- designating a connection to an Amazon Athena data store.
--
-- 'schemaName', 'athenaConnectorSource_schemaName' - The name of the Cloudwatch log group to read from. For example,
-- @\/aws-glue\/jobs\/output@.
newAthenaConnectorSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'connectionName'
  Prelude.Text ->
  -- | 'connectorName'
  Prelude.Text ->
  -- | 'connectionType'
  Prelude.Text ->
  -- | 'schemaName'
  Prelude.Text ->
  AthenaConnectorSource
newAthenaConnectorSource
  pName_
  pConnectionName_
  pConnectorName_
  pConnectionType_
  pSchemaName_ =
    AthenaConnectorSource'
      { connectionTable =
          Prelude.Nothing,
        outputSchemas = Prelude.Nothing,
        name = pName_,
        connectionName = pConnectionName_,
        connectorName = pConnectorName_,
        connectionType = pConnectionType_,
        schemaName = pSchemaName_
      }

-- | The name of the table in the data source.
athenaConnectorSource_connectionTable :: Lens.Lens' AthenaConnectorSource (Prelude.Maybe Prelude.Text)
athenaConnectorSource_connectionTable = Lens.lens (\AthenaConnectorSource' {connectionTable} -> connectionTable) (\s@AthenaConnectorSource' {} a -> s {connectionTable = a} :: AthenaConnectorSource)

-- | Specifies the data schema for the custom Athena source.
athenaConnectorSource_outputSchemas :: Lens.Lens' AthenaConnectorSource (Prelude.Maybe [GlueSchema])
athenaConnectorSource_outputSchemas = Lens.lens (\AthenaConnectorSource' {outputSchemas} -> outputSchemas) (\s@AthenaConnectorSource' {} a -> s {outputSchemas = a} :: AthenaConnectorSource) Prelude.. Lens.mapping Lens.coerced

-- | The name of the data source.
athenaConnectorSource_name :: Lens.Lens' AthenaConnectorSource Prelude.Text
athenaConnectorSource_name = Lens.lens (\AthenaConnectorSource' {name} -> name) (\s@AthenaConnectorSource' {} a -> s {name = a} :: AthenaConnectorSource)

-- | The name of the connection that is associated with the connector.
athenaConnectorSource_connectionName :: Lens.Lens' AthenaConnectorSource Prelude.Text
athenaConnectorSource_connectionName = Lens.lens (\AthenaConnectorSource' {connectionName} -> connectionName) (\s@AthenaConnectorSource' {} a -> s {connectionName = a} :: AthenaConnectorSource)

-- | The name of a connector that assists with accessing the data store in
-- Glue Studio.
athenaConnectorSource_connectorName :: Lens.Lens' AthenaConnectorSource Prelude.Text
athenaConnectorSource_connectorName = Lens.lens (\AthenaConnectorSource' {connectorName} -> connectorName) (\s@AthenaConnectorSource' {} a -> s {connectorName = a} :: AthenaConnectorSource)

-- | The type of connection, such as marketplace.athena or custom.athena,
-- designating a connection to an Amazon Athena data store.
athenaConnectorSource_connectionType :: Lens.Lens' AthenaConnectorSource Prelude.Text
athenaConnectorSource_connectionType = Lens.lens (\AthenaConnectorSource' {connectionType} -> connectionType) (\s@AthenaConnectorSource' {} a -> s {connectionType = a} :: AthenaConnectorSource)

-- | The name of the Cloudwatch log group to read from. For example,
-- @\/aws-glue\/jobs\/output@.
athenaConnectorSource_schemaName :: Lens.Lens' AthenaConnectorSource Prelude.Text
athenaConnectorSource_schemaName = Lens.lens (\AthenaConnectorSource' {schemaName} -> schemaName) (\s@AthenaConnectorSource' {} a -> s {schemaName = a} :: AthenaConnectorSource)

instance Data.FromJSON AthenaConnectorSource where
  parseJSON =
    Data.withObject
      "AthenaConnectorSource"
      ( \x ->
          AthenaConnectorSource'
            Prelude.<$> (x Data..:? "ConnectionTable")
            Prelude.<*> (x Data..:? "OutputSchemas" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "ConnectionName")
            Prelude.<*> (x Data..: "ConnectorName")
            Prelude.<*> (x Data..: "ConnectionType")
            Prelude.<*> (x Data..: "SchemaName")
      )

instance Prelude.Hashable AthenaConnectorSource where
  hashWithSalt _salt AthenaConnectorSource' {..} =
    _salt `Prelude.hashWithSalt` connectionTable
      `Prelude.hashWithSalt` outputSchemas
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` connectorName
      `Prelude.hashWithSalt` connectionType
      `Prelude.hashWithSalt` schemaName

instance Prelude.NFData AthenaConnectorSource where
  rnf AthenaConnectorSource' {..} =
    Prelude.rnf connectionTable
      `Prelude.seq` Prelude.rnf outputSchemas
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf connectionName
      `Prelude.seq` Prelude.rnf connectorName
      `Prelude.seq` Prelude.rnf connectionType
      `Prelude.seq` Prelude.rnf schemaName

instance Data.ToJSON AthenaConnectorSource where
  toJSON AthenaConnectorSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConnectionTable" Data..=)
              Prelude.<$> connectionTable,
            ("OutputSchemas" Data..=) Prelude.<$> outputSchemas,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("ConnectionName" Data..= connectionName),
            Prelude.Just ("ConnectorName" Data..= connectorName),
            Prelude.Just
              ("ConnectionType" Data..= connectionType),
            Prelude.Just ("SchemaName" Data..= schemaName)
          ]
      )
