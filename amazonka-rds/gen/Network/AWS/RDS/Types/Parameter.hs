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
-- Module      : Network.AWS.RDS.Types.Parameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Parameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.ApplyMethod

-- | This data type is used as a request parameter in the
-- @ModifyDBParameterGroup@ and @ResetDBParameterGroup@ actions.
--
-- This data type is used as a response element in the
-- @DescribeEngineDefaultParameters@ and @DescribeDBParameters@ actions.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | Specifies the valid range of values for the parameter.
    allowedValues :: Core.Maybe Core.Text,
    -- | The valid DB engine modes.
    supportedEngineModes :: Core.Maybe [Core.Text],
    -- | Indicates the source of the parameter value.
    source :: Core.Maybe Core.Text,
    -- | Specifies the value of the parameter.
    parameterValue :: Core.Maybe Core.Text,
    -- | Specifies the engine specific parameters type.
    applyType :: Core.Maybe Core.Text,
    -- | Specifies the name of the parameter.
    parameterName :: Core.Maybe Core.Text,
    -- | Provides a description of the parameter.
    description :: Core.Maybe Core.Text,
    -- | Indicates when to apply parameter updates.
    applyMethod :: Core.Maybe ApplyMethod,
    -- | Specifies the valid data type for the parameter.
    dataType :: Core.Maybe Core.Text,
    -- | Indicates whether (@true@) or not (@false@) the parameter can be
    -- modified. Some parameters have security or operational implications that
    -- prevent them from being changed.
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
-- 'allowedValues', 'parameter_allowedValues' - Specifies the valid range of values for the parameter.
--
-- 'supportedEngineModes', 'parameter_supportedEngineModes' - The valid DB engine modes.
--
-- 'source', 'parameter_source' - Indicates the source of the parameter value.
--
-- 'parameterValue', 'parameter_parameterValue' - Specifies the value of the parameter.
--
-- 'applyType', 'parameter_applyType' - Specifies the engine specific parameters type.
--
-- 'parameterName', 'parameter_parameterName' - Specifies the name of the parameter.
--
-- 'description', 'parameter_description' - Provides a description of the parameter.
--
-- 'applyMethod', 'parameter_applyMethod' - Indicates when to apply parameter updates.
--
-- 'dataType', 'parameter_dataType' - Specifies the valid data type for the parameter.
--
-- 'isModifiable', 'parameter_isModifiable' - Indicates whether (@true@) or not (@false@) the parameter can be
-- modified. Some parameters have security or operational implications that
-- prevent them from being changed.
--
-- 'minimumEngineVersion', 'parameter_minimumEngineVersion' - The earliest engine version to which the parameter can apply.
newParameter ::
  Parameter
newParameter =
  Parameter'
    { allowedValues = Core.Nothing,
      supportedEngineModes = Core.Nothing,
      source = Core.Nothing,
      parameterValue = Core.Nothing,
      applyType = Core.Nothing,
      parameterName = Core.Nothing,
      description = Core.Nothing,
      applyMethod = Core.Nothing,
      dataType = Core.Nothing,
      isModifiable = Core.Nothing,
      minimumEngineVersion = Core.Nothing
    }

-- | Specifies the valid range of values for the parameter.
parameter_allowedValues :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_allowedValues = Lens.lens (\Parameter' {allowedValues} -> allowedValues) (\s@Parameter' {} a -> s {allowedValues = a} :: Parameter)

-- | The valid DB engine modes.
parameter_supportedEngineModes :: Lens.Lens' Parameter (Core.Maybe [Core.Text])
parameter_supportedEngineModes = Lens.lens (\Parameter' {supportedEngineModes} -> supportedEngineModes) (\s@Parameter' {} a -> s {supportedEngineModes = a} :: Parameter) Core.. Lens.mapping Lens._Coerce

-- | Indicates the source of the parameter value.
parameter_source :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_source = Lens.lens (\Parameter' {source} -> source) (\s@Parameter' {} a -> s {source = a} :: Parameter)

-- | Specifies the value of the parameter.
parameter_parameterValue :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_parameterValue = Lens.lens (\Parameter' {parameterValue} -> parameterValue) (\s@Parameter' {} a -> s {parameterValue = a} :: Parameter)

-- | Specifies the engine specific parameters type.
parameter_applyType :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_applyType = Lens.lens (\Parameter' {applyType} -> applyType) (\s@Parameter' {} a -> s {applyType = a} :: Parameter)

-- | Specifies the name of the parameter.
parameter_parameterName :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_parameterName = Lens.lens (\Parameter' {parameterName} -> parameterName) (\s@Parameter' {} a -> s {parameterName = a} :: Parameter)

-- | Provides a description of the parameter.
parameter_description :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_description = Lens.lens (\Parameter' {description} -> description) (\s@Parameter' {} a -> s {description = a} :: Parameter)

-- | Indicates when to apply parameter updates.
parameter_applyMethod :: Lens.Lens' Parameter (Core.Maybe ApplyMethod)
parameter_applyMethod = Lens.lens (\Parameter' {applyMethod} -> applyMethod) (\s@Parameter' {} a -> s {applyMethod = a} :: Parameter)

-- | Specifies the valid data type for the parameter.
parameter_dataType :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_dataType = Lens.lens (\Parameter' {dataType} -> dataType) (\s@Parameter' {} a -> s {dataType = a} :: Parameter)

-- | Indicates whether (@true@) or not (@false@) the parameter can be
-- modified. Some parameters have security or operational implications that
-- prevent them from being changed.
parameter_isModifiable :: Lens.Lens' Parameter (Core.Maybe Core.Bool)
parameter_isModifiable = Lens.lens (\Parameter' {isModifiable} -> isModifiable) (\s@Parameter' {} a -> s {isModifiable = a} :: Parameter)

-- | The earliest engine version to which the parameter can apply.
parameter_minimumEngineVersion :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_minimumEngineVersion = Lens.lens (\Parameter' {minimumEngineVersion} -> minimumEngineVersion) (\s@Parameter' {} a -> s {minimumEngineVersion = a} :: Parameter)

instance Core.FromXML Parameter where
  parseXML x =
    Parameter'
      Core.<$> (x Core..@? "AllowedValues")
      Core.<*> ( x Core..@? "SupportedEngineModes"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "Source")
      Core.<*> (x Core..@? "ParameterValue")
      Core.<*> (x Core..@? "ApplyType")
      Core.<*> (x Core..@? "ParameterName")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "ApplyMethod")
      Core.<*> (x Core..@? "DataType")
      Core.<*> (x Core..@? "IsModifiable")
      Core.<*> (x Core..@? "MinimumEngineVersion")

instance Core.Hashable Parameter

instance Core.NFData Parameter

instance Core.ToQuery Parameter where
  toQuery Parameter' {..} =
    Core.mconcat
      [ "AllowedValues" Core.=: allowedValues,
        "SupportedEngineModes"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> supportedEngineModes
            ),
        "Source" Core.=: source,
        "ParameterValue" Core.=: parameterValue,
        "ApplyType" Core.=: applyType,
        "ParameterName" Core.=: parameterName,
        "Description" Core.=: description,
        "ApplyMethod" Core.=: applyMethod,
        "DataType" Core.=: dataType,
        "IsModifiable" Core.=: isModifiable,
        "MinimumEngineVersion" Core.=: minimumEngineVersion
      ]
