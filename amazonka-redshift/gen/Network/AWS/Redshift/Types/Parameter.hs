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
-- Module      : Network.AWS.Redshift.Types.Parameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Parameter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ParameterApplyType

-- | Describes a parameter in a cluster parameter group.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | The valid range of values for the parameter.
    allowedValues :: Prelude.Maybe Prelude.Text,
    -- | The source of the parameter value, such as \"engine-default\" or
    -- \"user\".
    source :: Prelude.Maybe Prelude.Text,
    -- | The value of the parameter.
    parameterValue :: Prelude.Maybe Prelude.Text,
    -- | Specifies how to apply the WLM configuration parameter. Some properties
    -- can be applied dynamically, while other properties require that any
    -- associated clusters be rebooted for the configuration changes to be
    -- applied. For more information about parameters and parameter groups, go
    -- to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
    -- in the /Amazon Redshift Cluster Management Guide/.
    applyType :: Prelude.Maybe ParameterApplyType,
    -- | The name of the parameter.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | A description of the parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | The data type of the parameter.
    dataType :: Prelude.Maybe Prelude.Text,
    -- | If @true@, the parameter can be modified. Some parameters have security
    -- or operational implications that prevent them from being changed.
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
    { allowedValues = Prelude.Nothing,
      source = Prelude.Nothing,
      parameterValue = Prelude.Nothing,
      applyType = Prelude.Nothing,
      parameterName = Prelude.Nothing,
      description = Prelude.Nothing,
      dataType = Prelude.Nothing,
      isModifiable = Prelude.Nothing,
      minimumEngineVersion = Prelude.Nothing
    }

-- | The valid range of values for the parameter.
parameter_allowedValues :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_allowedValues = Lens.lens (\Parameter' {allowedValues} -> allowedValues) (\s@Parameter' {} a -> s {allowedValues = a} :: Parameter)

-- | The source of the parameter value, such as \"engine-default\" or
-- \"user\".
parameter_source :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_source = Lens.lens (\Parameter' {source} -> source) (\s@Parameter' {} a -> s {source = a} :: Parameter)

-- | The value of the parameter.
parameter_parameterValue :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_parameterValue = Lens.lens (\Parameter' {parameterValue} -> parameterValue) (\s@Parameter' {} a -> s {parameterValue = a} :: Parameter)

-- | Specifies how to apply the WLM configuration parameter. Some properties
-- can be applied dynamically, while other properties require that any
-- associated clusters be rebooted for the configuration changes to be
-- applied. For more information about parameters and parameter groups, go
-- to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
parameter_applyType :: Lens.Lens' Parameter (Prelude.Maybe ParameterApplyType)
parameter_applyType = Lens.lens (\Parameter' {applyType} -> applyType) (\s@Parameter' {} a -> s {applyType = a} :: Parameter)

-- | The name of the parameter.
parameter_parameterName :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_parameterName = Lens.lens (\Parameter' {parameterName} -> parameterName) (\s@Parameter' {} a -> s {parameterName = a} :: Parameter)

-- | A description of the parameter.
parameter_description :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_description = Lens.lens (\Parameter' {description} -> description) (\s@Parameter' {} a -> s {description = a} :: Parameter)

-- | The data type of the parameter.
parameter_dataType :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_dataType = Lens.lens (\Parameter' {dataType} -> dataType) (\s@Parameter' {} a -> s {dataType = a} :: Parameter)

-- | If @true@, the parameter can be modified. Some parameters have security
-- or operational implications that prevent them from being changed.
parameter_isModifiable :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Bool)
parameter_isModifiable = Lens.lens (\Parameter' {isModifiable} -> isModifiable) (\s@Parameter' {} a -> s {isModifiable = a} :: Parameter)

-- | The earliest engine version to which the parameter can apply.
parameter_minimumEngineVersion :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_minimumEngineVersion = Lens.lens (\Parameter' {minimumEngineVersion} -> minimumEngineVersion) (\s@Parameter' {} a -> s {minimumEngineVersion = a} :: Parameter)

instance Prelude.FromXML Parameter where
  parseXML x =
    Parameter'
      Prelude.<$> (x Prelude..@? "AllowedValues")
      Prelude.<*> (x Prelude..@? "Source")
      Prelude.<*> (x Prelude..@? "ParameterValue")
      Prelude.<*> (x Prelude..@? "ApplyType")
      Prelude.<*> (x Prelude..@? "ParameterName")
      Prelude.<*> (x Prelude..@? "Description")
      Prelude.<*> (x Prelude..@? "DataType")
      Prelude.<*> (x Prelude..@? "IsModifiable")
      Prelude.<*> (x Prelude..@? "MinimumEngineVersion")

instance Prelude.Hashable Parameter

instance Prelude.NFData Parameter

instance Prelude.ToQuery Parameter where
  toQuery Parameter' {..} =
    Prelude.mconcat
      [ "AllowedValues" Prelude.=: allowedValues,
        "Source" Prelude.=: source,
        "ParameterValue" Prelude.=: parameterValue,
        "ApplyType" Prelude.=: applyType,
        "ParameterName" Prelude.=: parameterName,
        "Description" Prelude.=: description,
        "DataType" Prelude.=: dataType,
        "IsModifiable" Prelude.=: isModifiable,
        "MinimumEngineVersion"
          Prelude.=: minimumEngineVersion
      ]
