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
  { -- | The conditions under which changes to this parameter can be applied. For
    -- example, @requires-reboot@ indicates that a new value for this parameter
    -- will only take effect if a node is rebooted.
    changeType :: Prelude.Maybe ChangeType,
    -- | The value for the parameter.
    parameterValue :: Prelude.Maybe Prelude.Text,
    -- | Whether the customer is allowed to modify the parameter.
    isModifiable :: Prelude.Maybe IsModifiable,
    -- | A description of the parameter
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | A list of node types, and specific parameter values for each node.
    nodeTypeSpecificValues :: Prelude.Maybe [NodeTypeSpecificValue],
    -- | How the parameter is defined. For example, @system@ denotes a
    -- system-defined parameter.
    source :: Prelude.Maybe Prelude.Text,
    -- | Determines whether the parameter can be applied to any nodes, or only
    -- nodes of a particular type.
    parameterType :: Prelude.Maybe ParameterType,
    -- | A range of values within which the parameter can be set.
    allowedValues :: Prelude.Maybe Prelude.Text,
    -- | The data type of the parameter. For example, @integer@:
    dataType :: Prelude.Maybe Prelude.Text
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
-- 'changeType', 'parameter_changeType' - The conditions under which changes to this parameter can be applied. For
-- example, @requires-reboot@ indicates that a new value for this parameter
-- will only take effect if a node is rebooted.
--
-- 'parameterValue', 'parameter_parameterValue' - The value for the parameter.
--
-- 'isModifiable', 'parameter_isModifiable' - Whether the customer is allowed to modify the parameter.
--
-- 'description', 'parameter_description' - A description of the parameter
--
-- 'parameterName', 'parameter_parameterName' - The name of the parameter.
--
-- 'nodeTypeSpecificValues', 'parameter_nodeTypeSpecificValues' - A list of node types, and specific parameter values for each node.
--
-- 'source', 'parameter_source' - How the parameter is defined. For example, @system@ denotes a
-- system-defined parameter.
--
-- 'parameterType', 'parameter_parameterType' - Determines whether the parameter can be applied to any nodes, or only
-- nodes of a particular type.
--
-- 'allowedValues', 'parameter_allowedValues' - A range of values within which the parameter can be set.
--
-- 'dataType', 'parameter_dataType' - The data type of the parameter. For example, @integer@:
newParameter ::
  Parameter
newParameter =
  Parameter'
    { changeType = Prelude.Nothing,
      parameterValue = Prelude.Nothing,
      isModifiable = Prelude.Nothing,
      description = Prelude.Nothing,
      parameterName = Prelude.Nothing,
      nodeTypeSpecificValues = Prelude.Nothing,
      source = Prelude.Nothing,
      parameterType = Prelude.Nothing,
      allowedValues = Prelude.Nothing,
      dataType = Prelude.Nothing
    }

-- | The conditions under which changes to this parameter can be applied. For
-- example, @requires-reboot@ indicates that a new value for this parameter
-- will only take effect if a node is rebooted.
parameter_changeType :: Lens.Lens' Parameter (Prelude.Maybe ChangeType)
parameter_changeType = Lens.lens (\Parameter' {changeType} -> changeType) (\s@Parameter' {} a -> s {changeType = a} :: Parameter)

-- | The value for the parameter.
parameter_parameterValue :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_parameterValue = Lens.lens (\Parameter' {parameterValue} -> parameterValue) (\s@Parameter' {} a -> s {parameterValue = a} :: Parameter)

-- | Whether the customer is allowed to modify the parameter.
parameter_isModifiable :: Lens.Lens' Parameter (Prelude.Maybe IsModifiable)
parameter_isModifiable = Lens.lens (\Parameter' {isModifiable} -> isModifiable) (\s@Parameter' {} a -> s {isModifiable = a} :: Parameter)

-- | A description of the parameter
parameter_description :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_description = Lens.lens (\Parameter' {description} -> description) (\s@Parameter' {} a -> s {description = a} :: Parameter)

-- | The name of the parameter.
parameter_parameterName :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_parameterName = Lens.lens (\Parameter' {parameterName} -> parameterName) (\s@Parameter' {} a -> s {parameterName = a} :: Parameter)

-- | A list of node types, and specific parameter values for each node.
parameter_nodeTypeSpecificValues :: Lens.Lens' Parameter (Prelude.Maybe [NodeTypeSpecificValue])
parameter_nodeTypeSpecificValues = Lens.lens (\Parameter' {nodeTypeSpecificValues} -> nodeTypeSpecificValues) (\s@Parameter' {} a -> s {nodeTypeSpecificValues = a} :: Parameter) Prelude.. Lens.mapping Lens.coerced

-- | How the parameter is defined. For example, @system@ denotes a
-- system-defined parameter.
parameter_source :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_source = Lens.lens (\Parameter' {source} -> source) (\s@Parameter' {} a -> s {source = a} :: Parameter)

-- | Determines whether the parameter can be applied to any nodes, or only
-- nodes of a particular type.
parameter_parameterType :: Lens.Lens' Parameter (Prelude.Maybe ParameterType)
parameter_parameterType = Lens.lens (\Parameter' {parameterType} -> parameterType) (\s@Parameter' {} a -> s {parameterType = a} :: Parameter)

-- | A range of values within which the parameter can be set.
parameter_allowedValues :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_allowedValues = Lens.lens (\Parameter' {allowedValues} -> allowedValues) (\s@Parameter' {} a -> s {allowedValues = a} :: Parameter)

-- | The data type of the parameter. For example, @integer@:
parameter_dataType :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_dataType = Lens.lens (\Parameter' {dataType} -> dataType) (\s@Parameter' {} a -> s {dataType = a} :: Parameter)

instance Data.FromJSON Parameter where
  parseJSON =
    Data.withObject
      "Parameter"
      ( \x ->
          Parameter'
            Prelude.<$> (x Data..:? "ChangeType")
            Prelude.<*> (x Data..:? "ParameterValue")
            Prelude.<*> (x Data..:? "IsModifiable")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "ParameterName")
            Prelude.<*> ( x Data..:? "NodeTypeSpecificValues"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Source")
            Prelude.<*> (x Data..:? "ParameterType")
            Prelude.<*> (x Data..:? "AllowedValues")
            Prelude.<*> (x Data..:? "DataType")
      )

instance Prelude.Hashable Parameter where
  hashWithSalt _salt Parameter' {..} =
    _salt `Prelude.hashWithSalt` changeType
      `Prelude.hashWithSalt` parameterValue
      `Prelude.hashWithSalt` isModifiable
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` parameterName
      `Prelude.hashWithSalt` nodeTypeSpecificValues
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` parameterType
      `Prelude.hashWithSalt` allowedValues
      `Prelude.hashWithSalt` dataType

instance Prelude.NFData Parameter where
  rnf Parameter' {..} =
    Prelude.rnf changeType
      `Prelude.seq` Prelude.rnf parameterValue
      `Prelude.seq` Prelude.rnf isModifiable
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf parameterName
      `Prelude.seq` Prelude.rnf nodeTypeSpecificValues
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf parameterType
      `Prelude.seq` Prelude.rnf allowedValues
      `Prelude.seq` Prelude.rnf dataType
