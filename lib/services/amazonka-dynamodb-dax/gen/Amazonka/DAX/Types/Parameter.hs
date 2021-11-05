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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DAX.Types.Parameter where

import qualified Amazonka.Core as Core
import Amazonka.DAX.Types.ChangeType
import Amazonka.DAX.Types.IsModifiable
import Amazonka.DAX.Types.NodeTypeSpecificValue
import Amazonka.DAX.Types.ParameterType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an individual setting that controls some aspect of DAX
-- behavior.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | The value for the parameter.
    parameterValue :: Prelude.Maybe Prelude.Text,
    -- | Determines whether the parameter can be applied to any nodes, or only
    -- nodes of a particular type.
    parameterType :: Prelude.Maybe ParameterType,
    -- | How the parameter is defined. For example, @system@ denotes a
    -- system-defined parameter.
    source :: Prelude.Maybe Prelude.Text,
    -- | Whether the customer is allowed to modify the parameter.
    isModifiable :: Prelude.Maybe IsModifiable,
    -- | The data type of the parameter. For example, @integer@:
    dataType :: Prelude.Maybe Prelude.Text,
    -- | A list of node types, and specific parameter values for each node.
    nodeTypeSpecificValues :: Prelude.Maybe [NodeTypeSpecificValue],
    -- | A range of values within which the parameter can be set.
    allowedValues :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | A description of the parameter
    description :: Prelude.Maybe Prelude.Text,
    -- | The conditions under which changes to this parameter can be applied. For
    -- example, @requires-reboot@ indicates that a new value for this parameter
    -- will only take effect if a node is rebooted.
    changeType :: Prelude.Maybe ChangeType
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
-- 'parameterValue', 'parameter_parameterValue' - The value for the parameter.
--
-- 'parameterType', 'parameter_parameterType' - Determines whether the parameter can be applied to any nodes, or only
-- nodes of a particular type.
--
-- 'source', 'parameter_source' - How the parameter is defined. For example, @system@ denotes a
-- system-defined parameter.
--
-- 'isModifiable', 'parameter_isModifiable' - Whether the customer is allowed to modify the parameter.
--
-- 'dataType', 'parameter_dataType' - The data type of the parameter. For example, @integer@:
--
-- 'nodeTypeSpecificValues', 'parameter_nodeTypeSpecificValues' - A list of node types, and specific parameter values for each node.
--
-- 'allowedValues', 'parameter_allowedValues' - A range of values within which the parameter can be set.
--
-- 'parameterName', 'parameter_parameterName' - The name of the parameter.
--
-- 'description', 'parameter_description' - A description of the parameter
--
-- 'changeType', 'parameter_changeType' - The conditions under which changes to this parameter can be applied. For
-- example, @requires-reboot@ indicates that a new value for this parameter
-- will only take effect if a node is rebooted.
newParameter ::
  Parameter
newParameter =
  Parameter'
    { parameterValue = Prelude.Nothing,
      parameterType = Prelude.Nothing,
      source = Prelude.Nothing,
      isModifiable = Prelude.Nothing,
      dataType = Prelude.Nothing,
      nodeTypeSpecificValues = Prelude.Nothing,
      allowedValues = Prelude.Nothing,
      parameterName = Prelude.Nothing,
      description = Prelude.Nothing,
      changeType = Prelude.Nothing
    }

-- | The value for the parameter.
parameter_parameterValue :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_parameterValue = Lens.lens (\Parameter' {parameterValue} -> parameterValue) (\s@Parameter' {} a -> s {parameterValue = a} :: Parameter)

-- | Determines whether the parameter can be applied to any nodes, or only
-- nodes of a particular type.
parameter_parameterType :: Lens.Lens' Parameter (Prelude.Maybe ParameterType)
parameter_parameterType = Lens.lens (\Parameter' {parameterType} -> parameterType) (\s@Parameter' {} a -> s {parameterType = a} :: Parameter)

-- | How the parameter is defined. For example, @system@ denotes a
-- system-defined parameter.
parameter_source :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_source = Lens.lens (\Parameter' {source} -> source) (\s@Parameter' {} a -> s {source = a} :: Parameter)

-- | Whether the customer is allowed to modify the parameter.
parameter_isModifiable :: Lens.Lens' Parameter (Prelude.Maybe IsModifiable)
parameter_isModifiable = Lens.lens (\Parameter' {isModifiable} -> isModifiable) (\s@Parameter' {} a -> s {isModifiable = a} :: Parameter)

-- | The data type of the parameter. For example, @integer@:
parameter_dataType :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_dataType = Lens.lens (\Parameter' {dataType} -> dataType) (\s@Parameter' {} a -> s {dataType = a} :: Parameter)

-- | A list of node types, and specific parameter values for each node.
parameter_nodeTypeSpecificValues :: Lens.Lens' Parameter (Prelude.Maybe [NodeTypeSpecificValue])
parameter_nodeTypeSpecificValues = Lens.lens (\Parameter' {nodeTypeSpecificValues} -> nodeTypeSpecificValues) (\s@Parameter' {} a -> s {nodeTypeSpecificValues = a} :: Parameter) Prelude.. Lens.mapping Lens.coerced

-- | A range of values within which the parameter can be set.
parameter_allowedValues :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_allowedValues = Lens.lens (\Parameter' {allowedValues} -> allowedValues) (\s@Parameter' {} a -> s {allowedValues = a} :: Parameter)

-- | The name of the parameter.
parameter_parameterName :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_parameterName = Lens.lens (\Parameter' {parameterName} -> parameterName) (\s@Parameter' {} a -> s {parameterName = a} :: Parameter)

-- | A description of the parameter
parameter_description :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_description = Lens.lens (\Parameter' {description} -> description) (\s@Parameter' {} a -> s {description = a} :: Parameter)

-- | The conditions under which changes to this parameter can be applied. For
-- example, @requires-reboot@ indicates that a new value for this parameter
-- will only take effect if a node is rebooted.
parameter_changeType :: Lens.Lens' Parameter (Prelude.Maybe ChangeType)
parameter_changeType = Lens.lens (\Parameter' {changeType} -> changeType) (\s@Parameter' {} a -> s {changeType = a} :: Parameter)

instance Core.FromJSON Parameter where
  parseJSON =
    Core.withObject
      "Parameter"
      ( \x ->
          Parameter'
            Prelude.<$> (x Core..:? "ParameterValue")
            Prelude.<*> (x Core..:? "ParameterType")
            Prelude.<*> (x Core..:? "Source")
            Prelude.<*> (x Core..:? "IsModifiable")
            Prelude.<*> (x Core..:? "DataType")
            Prelude.<*> ( x Core..:? "NodeTypeSpecificValues"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "AllowedValues")
            Prelude.<*> (x Core..:? "ParameterName")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "ChangeType")
      )

instance Prelude.Hashable Parameter

instance Prelude.NFData Parameter
