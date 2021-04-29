{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    allowedValues :: Prelude.Maybe Prelude.Text,
    -- | The valid DB engine modes.
    supportedEngineModes :: Prelude.Maybe [Prelude.Text],
    -- | Indicates the source of the parameter value.
    source :: Prelude.Maybe Prelude.Text,
    -- | Specifies the value of the parameter.
    parameterValue :: Prelude.Maybe Prelude.Text,
    -- | Specifies the engine specific parameters type.
    applyType :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the parameter.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | Provides a description of the parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates when to apply parameter updates.
    applyMethod :: Prelude.Maybe ApplyMethod,
    -- | Specifies the valid data type for the parameter.
    dataType :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether (@true@) or not (@false@) the parameter can be
    -- modified. Some parameters have security or operational implications that
    -- prevent them from being changed.
    isModifiable :: Prelude.Maybe Prelude.Bool,
    -- | The earliest engine version to which the parameter can apply.
    minimumEngineVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { allowedValues = Prelude.Nothing,
      supportedEngineModes = Prelude.Nothing,
      source = Prelude.Nothing,
      parameterValue = Prelude.Nothing,
      applyType = Prelude.Nothing,
      parameterName = Prelude.Nothing,
      description = Prelude.Nothing,
      applyMethod = Prelude.Nothing,
      dataType = Prelude.Nothing,
      isModifiable = Prelude.Nothing,
      minimumEngineVersion = Prelude.Nothing
    }

-- | Specifies the valid range of values for the parameter.
parameter_allowedValues :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_allowedValues = Lens.lens (\Parameter' {allowedValues} -> allowedValues) (\s@Parameter' {} a -> s {allowedValues = a} :: Parameter)

-- | The valid DB engine modes.
parameter_supportedEngineModes :: Lens.Lens' Parameter (Prelude.Maybe [Prelude.Text])
parameter_supportedEngineModes = Lens.lens (\Parameter' {supportedEngineModes} -> supportedEngineModes) (\s@Parameter' {} a -> s {supportedEngineModes = a} :: Parameter) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates the source of the parameter value.
parameter_source :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_source = Lens.lens (\Parameter' {source} -> source) (\s@Parameter' {} a -> s {source = a} :: Parameter)

-- | Specifies the value of the parameter.
parameter_parameterValue :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_parameterValue = Lens.lens (\Parameter' {parameterValue} -> parameterValue) (\s@Parameter' {} a -> s {parameterValue = a} :: Parameter)

-- | Specifies the engine specific parameters type.
parameter_applyType :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_applyType = Lens.lens (\Parameter' {applyType} -> applyType) (\s@Parameter' {} a -> s {applyType = a} :: Parameter)

-- | Specifies the name of the parameter.
parameter_parameterName :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_parameterName = Lens.lens (\Parameter' {parameterName} -> parameterName) (\s@Parameter' {} a -> s {parameterName = a} :: Parameter)

-- | Provides a description of the parameter.
parameter_description :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_description = Lens.lens (\Parameter' {description} -> description) (\s@Parameter' {} a -> s {description = a} :: Parameter)

-- | Indicates when to apply parameter updates.
parameter_applyMethod :: Lens.Lens' Parameter (Prelude.Maybe ApplyMethod)
parameter_applyMethod = Lens.lens (\Parameter' {applyMethod} -> applyMethod) (\s@Parameter' {} a -> s {applyMethod = a} :: Parameter)

-- | Specifies the valid data type for the parameter.
parameter_dataType :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_dataType = Lens.lens (\Parameter' {dataType} -> dataType) (\s@Parameter' {} a -> s {dataType = a} :: Parameter)

-- | Indicates whether (@true@) or not (@false@) the parameter can be
-- modified. Some parameters have security or operational implications that
-- prevent them from being changed.
parameter_isModifiable :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Bool)
parameter_isModifiable = Lens.lens (\Parameter' {isModifiable} -> isModifiable) (\s@Parameter' {} a -> s {isModifiable = a} :: Parameter)

-- | The earliest engine version to which the parameter can apply.
parameter_minimumEngineVersion :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_minimumEngineVersion = Lens.lens (\Parameter' {minimumEngineVersion} -> minimumEngineVersion) (\s@Parameter' {} a -> s {minimumEngineVersion = a} :: Parameter)

instance Prelude.FromXML Parameter where
  parseXML x =
    Parameter'
      Prelude.<$> (x Prelude..@? "AllowedValues")
      Prelude.<*> ( x Prelude..@? "SupportedEngineModes"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "Source")
      Prelude.<*> (x Prelude..@? "ParameterValue")
      Prelude.<*> (x Prelude..@? "ApplyType")
      Prelude.<*> (x Prelude..@? "ParameterName")
      Prelude.<*> (x Prelude..@? "Description")
      Prelude.<*> (x Prelude..@? "ApplyMethod")
      Prelude.<*> (x Prelude..@? "DataType")
      Prelude.<*> (x Prelude..@? "IsModifiable")
      Prelude.<*> (x Prelude..@? "MinimumEngineVersion")

instance Prelude.Hashable Parameter

instance Prelude.NFData Parameter

instance Prelude.ToQuery Parameter where
  toQuery Parameter' {..} =
    Prelude.mconcat
      [ "AllowedValues" Prelude.=: allowedValues,
        "SupportedEngineModes"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> supportedEngineModes
            ),
        "Source" Prelude.=: source,
        "ParameterValue" Prelude.=: parameterValue,
        "ApplyType" Prelude.=: applyType,
        "ParameterName" Prelude.=: parameterName,
        "Description" Prelude.=: description,
        "ApplyMethod" Prelude.=: applyMethod,
        "DataType" Prelude.=: dataType,
        "IsModifiable" Prelude.=: isModifiable,
        "MinimumEngineVersion"
          Prelude.=: minimumEngineVersion
      ]
