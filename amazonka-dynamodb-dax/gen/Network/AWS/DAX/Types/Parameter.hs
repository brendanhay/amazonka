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
-- Module      : Network.AWS.DAX.Types.Parameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.Parameter where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types.ChangeType
import Network.AWS.DAX.Types.IsModifiable
import Network.AWS.DAX.Types.NodeTypeSpecificValue
import Network.AWS.DAX.Types.ParameterType
import qualified Network.AWS.Lens as Lens

-- | Describes an individual setting that controls some aspect of DAX
-- behavior.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | The conditions under which changes to this parameter can be applied. For
    -- example, @requires-reboot@ indicates that a new value for this parameter
    -- will only take effect if a node is rebooted.
    changeType :: Core.Maybe ChangeType,
    -- | A range of values within which the parameter can be set.
    allowedValues :: Core.Maybe Core.Text,
    -- | How the parameter is defined. For example, @system@ denotes a
    -- system-defined parameter.
    source :: Core.Maybe Core.Text,
    -- | The value for the parameter.
    parameterValue :: Core.Maybe Core.Text,
    -- | Determines whether the parameter can be applied to any nodes, or only
    -- nodes of a particular type.
    parameterType :: Core.Maybe ParameterType,
    -- | The name of the parameter.
    parameterName :: Core.Maybe Core.Text,
    -- | A description of the parameter
    description :: Core.Maybe Core.Text,
    -- | The data type of the parameter. For example, @integer@:
    dataType :: Core.Maybe Core.Text,
    -- | Whether the customer is allowed to modify the parameter.
    isModifiable :: Core.Maybe IsModifiable,
    -- | A list of node types, and specific parameter values for each node.
    nodeTypeSpecificValues :: Core.Maybe [NodeTypeSpecificValue]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'allowedValues', 'parameter_allowedValues' - A range of values within which the parameter can be set.
--
-- 'source', 'parameter_source' - How the parameter is defined. For example, @system@ denotes a
-- system-defined parameter.
--
-- 'parameterValue', 'parameter_parameterValue' - The value for the parameter.
--
-- 'parameterType', 'parameter_parameterType' - Determines whether the parameter can be applied to any nodes, or only
-- nodes of a particular type.
--
-- 'parameterName', 'parameter_parameterName' - The name of the parameter.
--
-- 'description', 'parameter_description' - A description of the parameter
--
-- 'dataType', 'parameter_dataType' - The data type of the parameter. For example, @integer@:
--
-- 'isModifiable', 'parameter_isModifiable' - Whether the customer is allowed to modify the parameter.
--
-- 'nodeTypeSpecificValues', 'parameter_nodeTypeSpecificValues' - A list of node types, and specific parameter values for each node.
newParameter ::
  Parameter
newParameter =
  Parameter'
    { changeType = Core.Nothing,
      allowedValues = Core.Nothing,
      source = Core.Nothing,
      parameterValue = Core.Nothing,
      parameterType = Core.Nothing,
      parameterName = Core.Nothing,
      description = Core.Nothing,
      dataType = Core.Nothing,
      isModifiable = Core.Nothing,
      nodeTypeSpecificValues = Core.Nothing
    }

-- | The conditions under which changes to this parameter can be applied. For
-- example, @requires-reboot@ indicates that a new value for this parameter
-- will only take effect if a node is rebooted.
parameter_changeType :: Lens.Lens' Parameter (Core.Maybe ChangeType)
parameter_changeType = Lens.lens (\Parameter' {changeType} -> changeType) (\s@Parameter' {} a -> s {changeType = a} :: Parameter)

-- | A range of values within which the parameter can be set.
parameter_allowedValues :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_allowedValues = Lens.lens (\Parameter' {allowedValues} -> allowedValues) (\s@Parameter' {} a -> s {allowedValues = a} :: Parameter)

-- | How the parameter is defined. For example, @system@ denotes a
-- system-defined parameter.
parameter_source :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_source = Lens.lens (\Parameter' {source} -> source) (\s@Parameter' {} a -> s {source = a} :: Parameter)

-- | The value for the parameter.
parameter_parameterValue :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_parameterValue = Lens.lens (\Parameter' {parameterValue} -> parameterValue) (\s@Parameter' {} a -> s {parameterValue = a} :: Parameter)

-- | Determines whether the parameter can be applied to any nodes, or only
-- nodes of a particular type.
parameter_parameterType :: Lens.Lens' Parameter (Core.Maybe ParameterType)
parameter_parameterType = Lens.lens (\Parameter' {parameterType} -> parameterType) (\s@Parameter' {} a -> s {parameterType = a} :: Parameter)

-- | The name of the parameter.
parameter_parameterName :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_parameterName = Lens.lens (\Parameter' {parameterName} -> parameterName) (\s@Parameter' {} a -> s {parameterName = a} :: Parameter)

-- | A description of the parameter
parameter_description :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_description = Lens.lens (\Parameter' {description} -> description) (\s@Parameter' {} a -> s {description = a} :: Parameter)

-- | The data type of the parameter. For example, @integer@:
parameter_dataType :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_dataType = Lens.lens (\Parameter' {dataType} -> dataType) (\s@Parameter' {} a -> s {dataType = a} :: Parameter)

-- | Whether the customer is allowed to modify the parameter.
parameter_isModifiable :: Lens.Lens' Parameter (Core.Maybe IsModifiable)
parameter_isModifiable = Lens.lens (\Parameter' {isModifiable} -> isModifiable) (\s@Parameter' {} a -> s {isModifiable = a} :: Parameter)

-- | A list of node types, and specific parameter values for each node.
parameter_nodeTypeSpecificValues :: Lens.Lens' Parameter (Core.Maybe [NodeTypeSpecificValue])
parameter_nodeTypeSpecificValues = Lens.lens (\Parameter' {nodeTypeSpecificValues} -> nodeTypeSpecificValues) (\s@Parameter' {} a -> s {nodeTypeSpecificValues = a} :: Parameter) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Parameter where
  parseJSON =
    Core.withObject
      "Parameter"
      ( \x ->
          Parameter'
            Core.<$> (x Core..:? "ChangeType")
            Core.<*> (x Core..:? "AllowedValues")
            Core.<*> (x Core..:? "Source")
            Core.<*> (x Core..:? "ParameterValue")
            Core.<*> (x Core..:? "ParameterType")
            Core.<*> (x Core..:? "ParameterName")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "DataType")
            Core.<*> (x Core..:? "IsModifiable")
            Core.<*> ( x Core..:? "NodeTypeSpecificValues"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable Parameter

instance Core.NFData Parameter
