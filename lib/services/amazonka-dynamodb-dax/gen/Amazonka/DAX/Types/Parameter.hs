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
-- Module      : Amazonka.DAX.Types.Parameter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DAX.Types.Parameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DAX.Types.ChangeType
import Amazonka.DAX.Types.IsModifiable
import Amazonka.DAX.Types.NodeTypeSpecificValue
import Amazonka.DAX.Types.ParameterType
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an individual setting that controls some aspect of DAX
-- behavior.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | A range of values within which the parameter can be set.
    allowedValues :: Prelude.Maybe Prelude.Text,
    -- | The conditions under which changes to this parameter can be applied. For
    -- example, @requires-reboot@ indicates that a new value for this parameter
    -- will only take effect if a node is rebooted.
    changeType :: Prelude.Maybe ChangeType,
    -- | The data type of the parameter. For example, @integer@:
    dataType :: Prelude.Maybe Prelude.Text,
    -- | A description of the parameter
    description :: Prelude.Maybe Prelude.Text,
    -- | Whether the customer is allowed to modify the parameter.
    isModifiable :: Prelude.Maybe IsModifiable,
    -- | A list of node types, and specific parameter values for each node.
    nodeTypeSpecificValues :: Prelude.Maybe [NodeTypeSpecificValue],
    -- | The name of the parameter.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | Determines whether the parameter can be applied to any nodes, or only
    -- nodes of a particular type.
    parameterType :: Prelude.Maybe ParameterType,
    -- | The value for the parameter.
    parameterValue :: Prelude.Maybe Prelude.Text,
    -- | How the parameter is defined. For example, @system@ denotes a
    -- system-defined parameter.
    source :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Parameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedValues', 'parameter_allowedValues' - A range of values within which the parameter can be set.
--
-- 'changeType', 'parameter_changeType' - The conditions under which changes to this parameter can be applied. For
-- example, @requires-reboot@ indicates that a new value for this parameter
-- will only take effect if a node is rebooted.
--
-- 'dataType', 'parameter_dataType' - The data type of the parameter. For example, @integer@:
--
-- 'description', 'parameter_description' - A description of the parameter
--
-- 'isModifiable', 'parameter_isModifiable' - Whether the customer is allowed to modify the parameter.
--
-- 'nodeTypeSpecificValues', 'parameter_nodeTypeSpecificValues' - A list of node types, and specific parameter values for each node.
--
-- 'parameterName', 'parameter_parameterName' - The name of the parameter.
--
-- 'parameterType', 'parameter_parameterType' - Determines whether the parameter can be applied to any nodes, or only
-- nodes of a particular type.
--
-- 'parameterValue', 'parameter_parameterValue' - The value for the parameter.
--
-- 'source', 'parameter_source' - How the parameter is defined. For example, @system@ denotes a
-- system-defined parameter.
newParameter ::
  Parameter
newParameter =
  Parameter'
    { allowedValues = Prelude.Nothing,
      changeType = Prelude.Nothing,
      dataType = Prelude.Nothing,
      description = Prelude.Nothing,
      isModifiable = Prelude.Nothing,
      nodeTypeSpecificValues = Prelude.Nothing,
      parameterName = Prelude.Nothing,
      parameterType = Prelude.Nothing,
      parameterValue = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | A range of values within which the parameter can be set.
parameter_allowedValues :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_allowedValues = Lens.lens (\Parameter' {allowedValues} -> allowedValues) (\s@Parameter' {} a -> s {allowedValues = a} :: Parameter)

-- | The conditions under which changes to this parameter can be applied. For
-- example, @requires-reboot@ indicates that a new value for this parameter
-- will only take effect if a node is rebooted.
parameter_changeType :: Lens.Lens' Parameter (Prelude.Maybe ChangeType)
parameter_changeType = Lens.lens (\Parameter' {changeType} -> changeType) (\s@Parameter' {} a -> s {changeType = a} :: Parameter)

-- | The data type of the parameter. For example, @integer@:
parameter_dataType :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_dataType = Lens.lens (\Parameter' {dataType} -> dataType) (\s@Parameter' {} a -> s {dataType = a} :: Parameter)

-- | A description of the parameter
parameter_description :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_description = Lens.lens (\Parameter' {description} -> description) (\s@Parameter' {} a -> s {description = a} :: Parameter)

-- | Whether the customer is allowed to modify the parameter.
parameter_isModifiable :: Lens.Lens' Parameter (Prelude.Maybe IsModifiable)
parameter_isModifiable = Lens.lens (\Parameter' {isModifiable} -> isModifiable) (\s@Parameter' {} a -> s {isModifiable = a} :: Parameter)

-- | A list of node types, and specific parameter values for each node.
parameter_nodeTypeSpecificValues :: Lens.Lens' Parameter (Prelude.Maybe [NodeTypeSpecificValue])
parameter_nodeTypeSpecificValues = Lens.lens (\Parameter' {nodeTypeSpecificValues} -> nodeTypeSpecificValues) (\s@Parameter' {} a -> s {nodeTypeSpecificValues = a} :: Parameter) Prelude.. Lens.mapping Lens.coerced

-- | The name of the parameter.
parameter_parameterName :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_parameterName = Lens.lens (\Parameter' {parameterName} -> parameterName) (\s@Parameter' {} a -> s {parameterName = a} :: Parameter)

-- | Determines whether the parameter can be applied to any nodes, or only
-- nodes of a particular type.
parameter_parameterType :: Lens.Lens' Parameter (Prelude.Maybe ParameterType)
parameter_parameterType = Lens.lens (\Parameter' {parameterType} -> parameterType) (\s@Parameter' {} a -> s {parameterType = a} :: Parameter)

-- | The value for the parameter.
parameter_parameterValue :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_parameterValue = Lens.lens (\Parameter' {parameterValue} -> parameterValue) (\s@Parameter' {} a -> s {parameterValue = a} :: Parameter)

-- | How the parameter is defined. For example, @system@ denotes a
-- system-defined parameter.
parameter_source :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_source = Lens.lens (\Parameter' {source} -> source) (\s@Parameter' {} a -> s {source = a} :: Parameter)

instance Data.FromJSON Parameter where
  parseJSON =
    Data.withObject
      "Parameter"
      ( \x ->
          Parameter'
            Prelude.<$> (x Data..:? "AllowedValues")
            Prelude.<*> (x Data..:? "ChangeType")
            Prelude.<*> (x Data..:? "DataType")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "IsModifiable")
            Prelude.<*> ( x Data..:? "NodeTypeSpecificValues"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ParameterName")
            Prelude.<*> (x Data..:? "ParameterType")
            Prelude.<*> (x Data..:? "ParameterValue")
            Prelude.<*> (x Data..:? "Source")
      )

instance Prelude.Hashable Parameter where
  hashWithSalt _salt Parameter' {..} =
    _salt `Prelude.hashWithSalt` allowedValues
      `Prelude.hashWithSalt` changeType
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` isModifiable
      `Prelude.hashWithSalt` nodeTypeSpecificValues
      `Prelude.hashWithSalt` parameterName
      `Prelude.hashWithSalt` parameterType
      `Prelude.hashWithSalt` parameterValue
      `Prelude.hashWithSalt` source

instance Prelude.NFData Parameter where
  rnf Parameter' {..} =
    Prelude.rnf allowedValues
      `Prelude.seq` Prelude.rnf changeType
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf isModifiable
      `Prelude.seq` Prelude.rnf nodeTypeSpecificValues
      `Prelude.seq` Prelude.rnf parameterName
      `Prelude.seq` Prelude.rnf parameterType
      `Prelude.seq` Prelude.rnf parameterValue
      `Prelude.seq` Prelude.rnf source
