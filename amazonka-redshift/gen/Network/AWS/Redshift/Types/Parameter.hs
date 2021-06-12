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
-- Module      : Network.AWS.Redshift.Types.Parameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Parameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ParameterApplyType

-- | Describes a parameter in a cluster parameter group.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | The valid range of values for the parameter.
    allowedValues :: Core.Maybe Core.Text,
    -- | The source of the parameter value, such as \"engine-default\" or
    -- \"user\".
    source :: Core.Maybe Core.Text,
    -- | The value of the parameter.
    parameterValue :: Core.Maybe Core.Text,
    -- | Specifies how to apply the WLM configuration parameter. Some properties
    -- can be applied dynamically, while other properties require that any
    -- associated clusters be rebooted for the configuration changes to be
    -- applied. For more information about parameters and parameter groups, go
    -- to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
    -- in the /Amazon Redshift Cluster Management Guide/.
    applyType :: Core.Maybe ParameterApplyType,
    -- | The name of the parameter.
    parameterName :: Core.Maybe Core.Text,
    -- | A description of the parameter.
    description :: Core.Maybe Core.Text,
    -- | The data type of the parameter.
    dataType :: Core.Maybe Core.Text,
    -- | If @true@, the parameter can be modified. Some parameters have security
    -- or operational implications that prevent them from being changed.
    isModifiable :: Core.Maybe Core.Bool,
    -- | The earliest engine version to which the parameter can apply.
    minimumEngineVersion :: Core.Maybe Core.Text
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
-- 'allowedValues', 'parameter_allowedValues' - The valid range of values for the parameter.
--
-- 'source', 'parameter_source' - The source of the parameter value, such as \"engine-default\" or
-- \"user\".
--
-- 'parameterValue', 'parameter_parameterValue' - The value of the parameter.
--
-- 'applyType', 'parameter_applyType' - Specifies how to apply the WLM configuration parameter. Some properties
-- can be applied dynamically, while other properties require that any
-- associated clusters be rebooted for the configuration changes to be
-- applied. For more information about parameters and parameter groups, go
-- to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- 'parameterName', 'parameter_parameterName' - The name of the parameter.
--
-- 'description', 'parameter_description' - A description of the parameter.
--
-- 'dataType', 'parameter_dataType' - The data type of the parameter.
--
-- 'isModifiable', 'parameter_isModifiable' - If @true@, the parameter can be modified. Some parameters have security
-- or operational implications that prevent them from being changed.
--
-- 'minimumEngineVersion', 'parameter_minimumEngineVersion' - The earliest engine version to which the parameter can apply.
newParameter ::
  Parameter
newParameter =
  Parameter'
    { allowedValues = Core.Nothing,
      source = Core.Nothing,
      parameterValue = Core.Nothing,
      applyType = Core.Nothing,
      parameterName = Core.Nothing,
      description = Core.Nothing,
      dataType = Core.Nothing,
      isModifiable = Core.Nothing,
      minimumEngineVersion = Core.Nothing
    }

-- | The valid range of values for the parameter.
parameter_allowedValues :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_allowedValues = Lens.lens (\Parameter' {allowedValues} -> allowedValues) (\s@Parameter' {} a -> s {allowedValues = a} :: Parameter)

-- | The source of the parameter value, such as \"engine-default\" or
-- \"user\".
parameter_source :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_source = Lens.lens (\Parameter' {source} -> source) (\s@Parameter' {} a -> s {source = a} :: Parameter)

-- | The value of the parameter.
parameter_parameterValue :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_parameterValue = Lens.lens (\Parameter' {parameterValue} -> parameterValue) (\s@Parameter' {} a -> s {parameterValue = a} :: Parameter)

-- | Specifies how to apply the WLM configuration parameter. Some properties
-- can be applied dynamically, while other properties require that any
-- associated clusters be rebooted for the configuration changes to be
-- applied. For more information about parameters and parameter groups, go
-- to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
parameter_applyType :: Lens.Lens' Parameter (Core.Maybe ParameterApplyType)
parameter_applyType = Lens.lens (\Parameter' {applyType} -> applyType) (\s@Parameter' {} a -> s {applyType = a} :: Parameter)

-- | The name of the parameter.
parameter_parameterName :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_parameterName = Lens.lens (\Parameter' {parameterName} -> parameterName) (\s@Parameter' {} a -> s {parameterName = a} :: Parameter)

-- | A description of the parameter.
parameter_description :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_description = Lens.lens (\Parameter' {description} -> description) (\s@Parameter' {} a -> s {description = a} :: Parameter)

-- | The data type of the parameter.
parameter_dataType :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_dataType = Lens.lens (\Parameter' {dataType} -> dataType) (\s@Parameter' {} a -> s {dataType = a} :: Parameter)

-- | If @true@, the parameter can be modified. Some parameters have security
-- or operational implications that prevent them from being changed.
parameter_isModifiable :: Lens.Lens' Parameter (Core.Maybe Core.Bool)
parameter_isModifiable = Lens.lens (\Parameter' {isModifiable} -> isModifiable) (\s@Parameter' {} a -> s {isModifiable = a} :: Parameter)

-- | The earliest engine version to which the parameter can apply.
parameter_minimumEngineVersion :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_minimumEngineVersion = Lens.lens (\Parameter' {minimumEngineVersion} -> minimumEngineVersion) (\s@Parameter' {} a -> s {minimumEngineVersion = a} :: Parameter)

instance Core.FromXML Parameter where
  parseXML x =
    Parameter'
      Core.<$> (x Core..@? "AllowedValues")
      Core.<*> (x Core..@? "Source")
      Core.<*> (x Core..@? "ParameterValue")
      Core.<*> (x Core..@? "ApplyType")
      Core.<*> (x Core..@? "ParameterName")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "DataType")
      Core.<*> (x Core..@? "IsModifiable")
      Core.<*> (x Core..@? "MinimumEngineVersion")

instance Core.Hashable Parameter

instance Core.NFData Parameter

instance Core.ToQuery Parameter where
  toQuery Parameter' {..} =
    Core.mconcat
      [ "AllowedValues" Core.=: allowedValues,
        "Source" Core.=: source,
        "ParameterValue" Core.=: parameterValue,
        "ApplyType" Core.=: applyType,
        "ParameterName" Core.=: parameterName,
        "Description" Core.=: description,
        "DataType" Core.=: dataType,
        "IsModifiable" Core.=: isModifiable,
        "MinimumEngineVersion" Core.=: minimumEngineVersion
      ]
