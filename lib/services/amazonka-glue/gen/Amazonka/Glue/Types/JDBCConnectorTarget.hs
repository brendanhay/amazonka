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
-- Module      : Amazonka.Glue.Types.JDBCConnectorTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.JDBCConnectorTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.GlueSchema
import qualified Amazonka.Prelude as Prelude

-- | Specifies a data target that writes to Amazon S3 in Apache Parquet
-- columnar storage.
--
-- /See:/ 'newJDBCConnectorTarget' smart constructor.
data JDBCConnectorTarget = JDBCConnectorTarget'
  { -- | Specifies the data schema for the JDBC target.
    outputSchemas :: Prelude.Maybe [GlueSchema],
    -- | Additional connection options for the connector.
    additionalOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the data target.
    name :: Prelude.Text,
    -- | The nodes that are inputs to the data target.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | The name of the connection that is associated with the connector.
    connectionName :: Prelude.Text,
    -- | The name of the table in the data target.
    connectionTable :: Prelude.Text,
    -- | The name of a connector that will be used.
    connectorName :: Prelude.Text,
    -- | The type of connection, such as marketplace.jdbc or custom.jdbc,
    -- designating a connection to a JDBC data target.
    connectionType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JDBCConnectorTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputSchemas', 'jDBCConnectorTarget_outputSchemas' - Specifies the data schema for the JDBC target.
--
-- 'additionalOptions', 'jDBCConnectorTarget_additionalOptions' - Additional connection options for the connector.
--
-- 'name', 'jDBCConnectorTarget_name' - The name of the data target.
--
-- 'inputs', 'jDBCConnectorTarget_inputs' - The nodes that are inputs to the data target.
--
-- 'connectionName', 'jDBCConnectorTarget_connectionName' - The name of the connection that is associated with the connector.
--
-- 'connectionTable', 'jDBCConnectorTarget_connectionTable' - The name of the table in the data target.
--
-- 'connectorName', 'jDBCConnectorTarget_connectorName' - The name of a connector that will be used.
--
-- 'connectionType', 'jDBCConnectorTarget_connectionType' - The type of connection, such as marketplace.jdbc or custom.jdbc,
-- designating a connection to a JDBC data target.
newJDBCConnectorTarget ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'connectionName'
  Prelude.Text ->
  -- | 'connectionTable'
  Prelude.Text ->
  -- | 'connectorName'
  Prelude.Text ->
  -- | 'connectionType'
  Prelude.Text ->
  JDBCConnectorTarget
newJDBCConnectorTarget
  pName_
  pInputs_
  pConnectionName_
  pConnectionTable_
  pConnectorName_
  pConnectionType_ =
    JDBCConnectorTarget'
      { outputSchemas =
          Prelude.Nothing,
        additionalOptions = Prelude.Nothing,
        name = pName_,
        inputs = Lens.coerced Lens.# pInputs_,
        connectionName = pConnectionName_,
        connectionTable = pConnectionTable_,
        connectorName = pConnectorName_,
        connectionType = pConnectionType_
      }

-- | Specifies the data schema for the JDBC target.
jDBCConnectorTarget_outputSchemas :: Lens.Lens' JDBCConnectorTarget (Prelude.Maybe [GlueSchema])
jDBCConnectorTarget_outputSchemas = Lens.lens (\JDBCConnectorTarget' {outputSchemas} -> outputSchemas) (\s@JDBCConnectorTarget' {} a -> s {outputSchemas = a} :: JDBCConnectorTarget) Prelude.. Lens.mapping Lens.coerced

-- | Additional connection options for the connector.
jDBCConnectorTarget_additionalOptions :: Lens.Lens' JDBCConnectorTarget (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jDBCConnectorTarget_additionalOptions = Lens.lens (\JDBCConnectorTarget' {additionalOptions} -> additionalOptions) (\s@JDBCConnectorTarget' {} a -> s {additionalOptions = a} :: JDBCConnectorTarget) Prelude.. Lens.mapping Lens.coerced

-- | The name of the data target.
jDBCConnectorTarget_name :: Lens.Lens' JDBCConnectorTarget Prelude.Text
jDBCConnectorTarget_name = Lens.lens (\JDBCConnectorTarget' {name} -> name) (\s@JDBCConnectorTarget' {} a -> s {name = a} :: JDBCConnectorTarget)

-- | The nodes that are inputs to the data target.
jDBCConnectorTarget_inputs :: Lens.Lens' JDBCConnectorTarget (Prelude.NonEmpty Prelude.Text)
jDBCConnectorTarget_inputs = Lens.lens (\JDBCConnectorTarget' {inputs} -> inputs) (\s@JDBCConnectorTarget' {} a -> s {inputs = a} :: JDBCConnectorTarget) Prelude.. Lens.coerced

-- | The name of the connection that is associated with the connector.
jDBCConnectorTarget_connectionName :: Lens.Lens' JDBCConnectorTarget Prelude.Text
jDBCConnectorTarget_connectionName = Lens.lens (\JDBCConnectorTarget' {connectionName} -> connectionName) (\s@JDBCConnectorTarget' {} a -> s {connectionName = a} :: JDBCConnectorTarget)

-- | The name of the table in the data target.
jDBCConnectorTarget_connectionTable :: Lens.Lens' JDBCConnectorTarget Prelude.Text
jDBCConnectorTarget_connectionTable = Lens.lens (\JDBCConnectorTarget' {connectionTable} -> connectionTable) (\s@JDBCConnectorTarget' {} a -> s {connectionTable = a} :: JDBCConnectorTarget)

-- | The name of a connector that will be used.
jDBCConnectorTarget_connectorName :: Lens.Lens' JDBCConnectorTarget Prelude.Text
jDBCConnectorTarget_connectorName = Lens.lens (\JDBCConnectorTarget' {connectorName} -> connectorName) (\s@JDBCConnectorTarget' {} a -> s {connectorName = a} :: JDBCConnectorTarget)

-- | The type of connection, such as marketplace.jdbc or custom.jdbc,
-- designating a connection to a JDBC data target.
jDBCConnectorTarget_connectionType :: Lens.Lens' JDBCConnectorTarget Prelude.Text
jDBCConnectorTarget_connectionType = Lens.lens (\JDBCConnectorTarget' {connectionType} -> connectionType) (\s@JDBCConnectorTarget' {} a -> s {connectionType = a} :: JDBCConnectorTarget)

instance Data.FromJSON JDBCConnectorTarget where
  parseJSON =
    Data.withObject
      "JDBCConnectorTarget"
      ( \x ->
          JDBCConnectorTarget'
            Prelude.<$> (x Data..:? "OutputSchemas" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "AdditionalOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "ConnectionName")
            Prelude.<*> (x Data..: "ConnectionTable")
            Prelude.<*> (x Data..: "ConnectorName")
            Prelude.<*> (x Data..: "ConnectionType")
      )

instance Prelude.Hashable JDBCConnectorTarget where
  hashWithSalt _salt JDBCConnectorTarget' {..} =
    _salt `Prelude.hashWithSalt` outputSchemas
      `Prelude.hashWithSalt` additionalOptions
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` connectionTable
      `Prelude.hashWithSalt` connectorName
      `Prelude.hashWithSalt` connectionType

instance Prelude.NFData JDBCConnectorTarget where
  rnf JDBCConnectorTarget' {..} =
    Prelude.rnf outputSchemas
      `Prelude.seq` Prelude.rnf additionalOptions
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf connectionName
      `Prelude.seq` Prelude.rnf connectionTable
      `Prelude.seq` Prelude.rnf connectorName
      `Prelude.seq` Prelude.rnf connectionType

instance Data.ToJSON JDBCConnectorTarget where
  toJSON JDBCConnectorTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OutputSchemas" Data..=) Prelude.<$> outputSchemas,
            ("AdditionalOptions" Data..=)
              Prelude.<$> additionalOptions,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just
              ("ConnectionName" Data..= connectionName),
            Prelude.Just
              ("ConnectionTable" Data..= connectionTable),
            Prelude.Just ("ConnectorName" Data..= connectorName),
            Prelude.Just
              ("ConnectionType" Data..= connectionType)
          ]
      )
