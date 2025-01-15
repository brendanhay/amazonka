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
-- Module      : Amazonka.Glue.Types.SparkConnectorSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SparkConnectorSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.GlueSchema
import qualified Amazonka.Prelude as Prelude

-- | Specifies a connector to an Apache Spark data source.
--
-- /See:/ 'newSparkConnectorSource' smart constructor.
data SparkConnectorSource = SparkConnectorSource'
  { -- | Additional connection options for the connector.
    additionalOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies data schema for the custom spark source.
    outputSchemas :: Prelude.Maybe [GlueSchema],
    -- | The name of the data source.
    name :: Prelude.Text,
    -- | The name of the connection that is associated with the connector.
    connectionName :: Prelude.Text,
    -- | The name of a connector that assists with accessing the data store in
    -- Glue Studio.
    connectorName :: Prelude.Text,
    -- | The type of connection, such as marketplace.spark or custom.spark,
    -- designating a connection to an Apache Spark data store.
    connectionType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SparkConnectorSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalOptions', 'sparkConnectorSource_additionalOptions' - Additional connection options for the connector.
--
-- 'outputSchemas', 'sparkConnectorSource_outputSchemas' - Specifies data schema for the custom spark source.
--
-- 'name', 'sparkConnectorSource_name' - The name of the data source.
--
-- 'connectionName', 'sparkConnectorSource_connectionName' - The name of the connection that is associated with the connector.
--
-- 'connectorName', 'sparkConnectorSource_connectorName' - The name of a connector that assists with accessing the data store in
-- Glue Studio.
--
-- 'connectionType', 'sparkConnectorSource_connectionType' - The type of connection, such as marketplace.spark or custom.spark,
-- designating a connection to an Apache Spark data store.
newSparkConnectorSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'connectionName'
  Prelude.Text ->
  -- | 'connectorName'
  Prelude.Text ->
  -- | 'connectionType'
  Prelude.Text ->
  SparkConnectorSource
newSparkConnectorSource
  pName_
  pConnectionName_
  pConnectorName_
  pConnectionType_ =
    SparkConnectorSource'
      { additionalOptions =
          Prelude.Nothing,
        outputSchemas = Prelude.Nothing,
        name = pName_,
        connectionName = pConnectionName_,
        connectorName = pConnectorName_,
        connectionType = pConnectionType_
      }

-- | Additional connection options for the connector.
sparkConnectorSource_additionalOptions :: Lens.Lens' SparkConnectorSource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
sparkConnectorSource_additionalOptions = Lens.lens (\SparkConnectorSource' {additionalOptions} -> additionalOptions) (\s@SparkConnectorSource' {} a -> s {additionalOptions = a} :: SparkConnectorSource) Prelude.. Lens.mapping Lens.coerced

-- | Specifies data schema for the custom spark source.
sparkConnectorSource_outputSchemas :: Lens.Lens' SparkConnectorSource (Prelude.Maybe [GlueSchema])
sparkConnectorSource_outputSchemas = Lens.lens (\SparkConnectorSource' {outputSchemas} -> outputSchemas) (\s@SparkConnectorSource' {} a -> s {outputSchemas = a} :: SparkConnectorSource) Prelude.. Lens.mapping Lens.coerced

-- | The name of the data source.
sparkConnectorSource_name :: Lens.Lens' SparkConnectorSource Prelude.Text
sparkConnectorSource_name = Lens.lens (\SparkConnectorSource' {name} -> name) (\s@SparkConnectorSource' {} a -> s {name = a} :: SparkConnectorSource)

-- | The name of the connection that is associated with the connector.
sparkConnectorSource_connectionName :: Lens.Lens' SparkConnectorSource Prelude.Text
sparkConnectorSource_connectionName = Lens.lens (\SparkConnectorSource' {connectionName} -> connectionName) (\s@SparkConnectorSource' {} a -> s {connectionName = a} :: SparkConnectorSource)

-- | The name of a connector that assists with accessing the data store in
-- Glue Studio.
sparkConnectorSource_connectorName :: Lens.Lens' SparkConnectorSource Prelude.Text
sparkConnectorSource_connectorName = Lens.lens (\SparkConnectorSource' {connectorName} -> connectorName) (\s@SparkConnectorSource' {} a -> s {connectorName = a} :: SparkConnectorSource)

-- | The type of connection, such as marketplace.spark or custom.spark,
-- designating a connection to an Apache Spark data store.
sparkConnectorSource_connectionType :: Lens.Lens' SparkConnectorSource Prelude.Text
sparkConnectorSource_connectionType = Lens.lens (\SparkConnectorSource' {connectionType} -> connectionType) (\s@SparkConnectorSource' {} a -> s {connectionType = a} :: SparkConnectorSource)

instance Data.FromJSON SparkConnectorSource where
  parseJSON =
    Data.withObject
      "SparkConnectorSource"
      ( \x ->
          SparkConnectorSource'
            Prelude.<$> ( x
                            Data..:? "AdditionalOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "OutputSchemas" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "ConnectionName")
            Prelude.<*> (x Data..: "ConnectorName")
            Prelude.<*> (x Data..: "ConnectionType")
      )

instance Prelude.Hashable SparkConnectorSource where
  hashWithSalt _salt SparkConnectorSource' {..} =
    _salt
      `Prelude.hashWithSalt` additionalOptions
      `Prelude.hashWithSalt` outputSchemas
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` connectorName
      `Prelude.hashWithSalt` connectionType

instance Prelude.NFData SparkConnectorSource where
  rnf SparkConnectorSource' {..} =
    Prelude.rnf additionalOptions `Prelude.seq`
      Prelude.rnf outputSchemas `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf connectionName `Prelude.seq`
            Prelude.rnf connectorName `Prelude.seq`
              Prelude.rnf connectionType

instance Data.ToJSON SparkConnectorSource where
  toJSON SparkConnectorSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalOptions" Data..=)
              Prelude.<$> additionalOptions,
            ("OutputSchemas" Data..=) Prelude.<$> outputSchemas,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("ConnectionName" Data..= connectionName),
            Prelude.Just ("ConnectorName" Data..= connectorName),
            Prelude.Just
              ("ConnectionType" Data..= connectionType)
          ]
      )
