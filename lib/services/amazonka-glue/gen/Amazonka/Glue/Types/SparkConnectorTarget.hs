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
-- Module      : Amazonka.Glue.Types.SparkConnectorTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SparkConnectorTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.GlueSchema
import qualified Amazonka.Prelude as Prelude

-- | Specifies a target that uses an Apache Spark connector.
--
-- /See:/ 'newSparkConnectorTarget' smart constructor.
data SparkConnectorTarget = SparkConnectorTarget'
  { -- | Additional connection options for the connector.
    additionalOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the data schema for the custom spark target.
    outputSchemas :: Prelude.Maybe [GlueSchema],
    -- | The name of the data target.
    name :: Prelude.Text,
    -- | The nodes that are inputs to the data target.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | The name of a connection for an Apache Spark connector.
    connectionName :: Prelude.Text,
    -- | The name of an Apache Spark connector.
    connectorName :: Prelude.Text,
    -- | The type of connection, such as marketplace.spark or custom.spark,
    -- designating a connection to an Apache Spark data store.
    connectionType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SparkConnectorTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalOptions', 'sparkConnectorTarget_additionalOptions' - Additional connection options for the connector.
--
-- 'outputSchemas', 'sparkConnectorTarget_outputSchemas' - Specifies the data schema for the custom spark target.
--
-- 'name', 'sparkConnectorTarget_name' - The name of the data target.
--
-- 'inputs', 'sparkConnectorTarget_inputs' - The nodes that are inputs to the data target.
--
-- 'connectionName', 'sparkConnectorTarget_connectionName' - The name of a connection for an Apache Spark connector.
--
-- 'connectorName', 'sparkConnectorTarget_connectorName' - The name of an Apache Spark connector.
--
-- 'connectionType', 'sparkConnectorTarget_connectionType' - The type of connection, such as marketplace.spark or custom.spark,
-- designating a connection to an Apache Spark data store.
newSparkConnectorTarget ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'connectionName'
  Prelude.Text ->
  -- | 'connectorName'
  Prelude.Text ->
  -- | 'connectionType'
  Prelude.Text ->
  SparkConnectorTarget
newSparkConnectorTarget
  pName_
  pInputs_
  pConnectionName_
  pConnectorName_
  pConnectionType_ =
    SparkConnectorTarget'
      { additionalOptions =
          Prelude.Nothing,
        outputSchemas = Prelude.Nothing,
        name = pName_,
        inputs = Lens.coerced Lens.# pInputs_,
        connectionName = pConnectionName_,
        connectorName = pConnectorName_,
        connectionType = pConnectionType_
      }

-- | Additional connection options for the connector.
sparkConnectorTarget_additionalOptions :: Lens.Lens' SparkConnectorTarget (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
sparkConnectorTarget_additionalOptions = Lens.lens (\SparkConnectorTarget' {additionalOptions} -> additionalOptions) (\s@SparkConnectorTarget' {} a -> s {additionalOptions = a} :: SparkConnectorTarget) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the data schema for the custom spark target.
sparkConnectorTarget_outputSchemas :: Lens.Lens' SparkConnectorTarget (Prelude.Maybe [GlueSchema])
sparkConnectorTarget_outputSchemas = Lens.lens (\SparkConnectorTarget' {outputSchemas} -> outputSchemas) (\s@SparkConnectorTarget' {} a -> s {outputSchemas = a} :: SparkConnectorTarget) Prelude.. Lens.mapping Lens.coerced

-- | The name of the data target.
sparkConnectorTarget_name :: Lens.Lens' SparkConnectorTarget Prelude.Text
sparkConnectorTarget_name = Lens.lens (\SparkConnectorTarget' {name} -> name) (\s@SparkConnectorTarget' {} a -> s {name = a} :: SparkConnectorTarget)

-- | The nodes that are inputs to the data target.
sparkConnectorTarget_inputs :: Lens.Lens' SparkConnectorTarget (Prelude.NonEmpty Prelude.Text)
sparkConnectorTarget_inputs = Lens.lens (\SparkConnectorTarget' {inputs} -> inputs) (\s@SparkConnectorTarget' {} a -> s {inputs = a} :: SparkConnectorTarget) Prelude.. Lens.coerced

-- | The name of a connection for an Apache Spark connector.
sparkConnectorTarget_connectionName :: Lens.Lens' SparkConnectorTarget Prelude.Text
sparkConnectorTarget_connectionName = Lens.lens (\SparkConnectorTarget' {connectionName} -> connectionName) (\s@SparkConnectorTarget' {} a -> s {connectionName = a} :: SparkConnectorTarget)

-- | The name of an Apache Spark connector.
sparkConnectorTarget_connectorName :: Lens.Lens' SparkConnectorTarget Prelude.Text
sparkConnectorTarget_connectorName = Lens.lens (\SparkConnectorTarget' {connectorName} -> connectorName) (\s@SparkConnectorTarget' {} a -> s {connectorName = a} :: SparkConnectorTarget)

-- | The type of connection, such as marketplace.spark or custom.spark,
-- designating a connection to an Apache Spark data store.
sparkConnectorTarget_connectionType :: Lens.Lens' SparkConnectorTarget Prelude.Text
sparkConnectorTarget_connectionType = Lens.lens (\SparkConnectorTarget' {connectionType} -> connectionType) (\s@SparkConnectorTarget' {} a -> s {connectionType = a} :: SparkConnectorTarget)

instance Data.FromJSON SparkConnectorTarget where
  parseJSON =
    Data.withObject
      "SparkConnectorTarget"
      ( \x ->
          SparkConnectorTarget'
            Prelude.<$> ( x Data..:? "AdditionalOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "OutputSchemas" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "ConnectionName")
            Prelude.<*> (x Data..: "ConnectorName")
            Prelude.<*> (x Data..: "ConnectionType")
      )

instance Prelude.Hashable SparkConnectorTarget where
  hashWithSalt _salt SparkConnectorTarget' {..} =
    _salt `Prelude.hashWithSalt` additionalOptions
      `Prelude.hashWithSalt` outputSchemas
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` connectorName
      `Prelude.hashWithSalt` connectionType

instance Prelude.NFData SparkConnectorTarget where
  rnf SparkConnectorTarget' {..} =
    Prelude.rnf additionalOptions
      `Prelude.seq` Prelude.rnf outputSchemas
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf connectionName
      `Prelude.seq` Prelude.rnf connectorName
      `Prelude.seq` Prelude.rnf connectionType

instance Data.ToJSON SparkConnectorTarget where
  toJSON SparkConnectorTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalOptions" Data..=)
              Prelude.<$> additionalOptions,
            ("OutputSchemas" Data..=) Prelude.<$> outputSchemas,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just
              ("ConnectionName" Data..= connectionName),
            Prelude.Just ("ConnectorName" Data..= connectorName),
            Prelude.Just
              ("ConnectionType" Data..= connectionType)
          ]
      )
