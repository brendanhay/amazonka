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
-- Module      : Amazonka.Neptune.Types.Parameter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.Parameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types.ApplyMethod
import qualified Amazonka.Prelude as Prelude

-- | Specifies a parameter.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | Specifies the value of the parameter.
    parameterValue :: Prelude.Maybe Prelude.Text,
    -- | Indicates when to apply parameter updates.
    applyMethod :: Prelude.Maybe ApplyMethod,
    -- | Specifies the engine specific parameters type.
    applyType :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether (@true@) or not (@false@) the parameter can be
    -- modified. Some parameters have security or operational implications that
    -- prevent them from being changed.
    isModifiable :: Prelude.Maybe Prelude.Bool,
    -- | Provides a description of the parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the parameter.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | The earliest engine version to which the parameter can apply.
    minimumEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | Indicates the source of the parameter value.
    source :: Prelude.Maybe Prelude.Text,
    -- | Specifies the valid range of values for the parameter.
    allowedValues :: Prelude.Maybe Prelude.Text,
    -- | Specifies the valid data type for the parameter.
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
-- 'parameterValue', 'parameter_parameterValue' - Specifies the value of the parameter.
--
-- 'applyMethod', 'parameter_applyMethod' - Indicates when to apply parameter updates.
--
-- 'applyType', 'parameter_applyType' - Specifies the engine specific parameters type.
--
-- 'isModifiable', 'parameter_isModifiable' - Indicates whether (@true@) or not (@false@) the parameter can be
-- modified. Some parameters have security or operational implications that
-- prevent them from being changed.
--
-- 'description', 'parameter_description' - Provides a description of the parameter.
--
-- 'parameterName', 'parameter_parameterName' - Specifies the name of the parameter.
--
-- 'minimumEngineVersion', 'parameter_minimumEngineVersion' - The earliest engine version to which the parameter can apply.
--
-- 'source', 'parameter_source' - Indicates the source of the parameter value.
--
-- 'allowedValues', 'parameter_allowedValues' - Specifies the valid range of values for the parameter.
--
-- 'dataType', 'parameter_dataType' - Specifies the valid data type for the parameter.
newParameter ::
  Parameter
newParameter =
  Parameter'
    { parameterValue = Prelude.Nothing,
      applyMethod = Prelude.Nothing,
      applyType = Prelude.Nothing,
      isModifiable = Prelude.Nothing,
      description = Prelude.Nothing,
      parameterName = Prelude.Nothing,
      minimumEngineVersion = Prelude.Nothing,
      source = Prelude.Nothing,
      allowedValues = Prelude.Nothing,
      dataType = Prelude.Nothing
    }

-- | Specifies the value of the parameter.
parameter_parameterValue :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_parameterValue = Lens.lens (\Parameter' {parameterValue} -> parameterValue) (\s@Parameter' {} a -> s {parameterValue = a} :: Parameter)

-- | Indicates when to apply parameter updates.
parameter_applyMethod :: Lens.Lens' Parameter (Prelude.Maybe ApplyMethod)
parameter_applyMethod = Lens.lens (\Parameter' {applyMethod} -> applyMethod) (\s@Parameter' {} a -> s {applyMethod = a} :: Parameter)

-- | Specifies the engine specific parameters type.
parameter_applyType :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_applyType = Lens.lens (\Parameter' {applyType} -> applyType) (\s@Parameter' {} a -> s {applyType = a} :: Parameter)

-- | Indicates whether (@true@) or not (@false@) the parameter can be
-- modified. Some parameters have security or operational implications that
-- prevent them from being changed.
parameter_isModifiable :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Bool)
parameter_isModifiable = Lens.lens (\Parameter' {isModifiable} -> isModifiable) (\s@Parameter' {} a -> s {isModifiable = a} :: Parameter)

-- | Provides a description of the parameter.
parameter_description :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_description = Lens.lens (\Parameter' {description} -> description) (\s@Parameter' {} a -> s {description = a} :: Parameter)

-- | Specifies the name of the parameter.
parameter_parameterName :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_parameterName = Lens.lens (\Parameter' {parameterName} -> parameterName) (\s@Parameter' {} a -> s {parameterName = a} :: Parameter)

-- | The earliest engine version to which the parameter can apply.
parameter_minimumEngineVersion :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_minimumEngineVersion = Lens.lens (\Parameter' {minimumEngineVersion} -> minimumEngineVersion) (\s@Parameter' {} a -> s {minimumEngineVersion = a} :: Parameter)

-- | Indicates the source of the parameter value.
parameter_source :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_source = Lens.lens (\Parameter' {source} -> source) (\s@Parameter' {} a -> s {source = a} :: Parameter)

-- | Specifies the valid range of values for the parameter.
parameter_allowedValues :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_allowedValues = Lens.lens (\Parameter' {allowedValues} -> allowedValues) (\s@Parameter' {} a -> s {allowedValues = a} :: Parameter)

-- | Specifies the valid data type for the parameter.
parameter_dataType :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_dataType = Lens.lens (\Parameter' {dataType} -> dataType) (\s@Parameter' {} a -> s {dataType = a} :: Parameter)

instance Data.FromXML Parameter where
  parseXML x =
    Parameter'
      Prelude.<$> (x Data..@? "ParameterValue")
      Prelude.<*> (x Data..@? "ApplyMethod")
      Prelude.<*> (x Data..@? "ApplyType")
      Prelude.<*> (x Data..@? "IsModifiable")
      Prelude.<*> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "ParameterName")
      Prelude.<*> (x Data..@? "MinimumEngineVersion")
      Prelude.<*> (x Data..@? "Source")
      Prelude.<*> (x Data..@? "AllowedValues")
      Prelude.<*> (x Data..@? "DataType")

instance Prelude.Hashable Parameter where
  hashWithSalt _salt Parameter' {..} =
    _salt `Prelude.hashWithSalt` parameterValue
      `Prelude.hashWithSalt` applyMethod
      `Prelude.hashWithSalt` applyType
      `Prelude.hashWithSalt` isModifiable
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` parameterName
      `Prelude.hashWithSalt` minimumEngineVersion
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` allowedValues
      `Prelude.hashWithSalt` dataType

instance Prelude.NFData Parameter where
  rnf Parameter' {..} =
    Prelude.rnf parameterValue
      `Prelude.seq` Prelude.rnf applyMethod
      `Prelude.seq` Prelude.rnf applyType
      `Prelude.seq` Prelude.rnf isModifiable
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf parameterName
      `Prelude.seq` Prelude.rnf minimumEngineVersion
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf allowedValues
      `Prelude.seq` Prelude.rnf dataType

instance Data.ToQuery Parameter where
  toQuery Parameter' {..} =
    Prelude.mconcat
      [ "ParameterValue" Data.=: parameterValue,
        "ApplyMethod" Data.=: applyMethod,
        "ApplyType" Data.=: applyType,
        "IsModifiable" Data.=: isModifiable,
        "Description" Data.=: description,
        "ParameterName" Data.=: parameterName,
        "MinimumEngineVersion" Data.=: minimumEngineVersion,
        "Source" Data.=: source,
        "AllowedValues" Data.=: allowedValues,
        "DataType" Data.=: dataType
      ]
