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
-- Module      : Amazonka.QuickSight.Types.DecimalParameterDeclaration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DecimalParameterDeclaration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DecimalDefaultValues
import Amazonka.QuickSight.Types.DecimalValueWhenUnsetConfiguration
import Amazonka.QuickSight.Types.ParameterValueType

-- | A parameter declaration for the @Decimal@ data type.
--
-- /See:/ 'newDecimalParameterDeclaration' smart constructor.
data DecimalParameterDeclaration = DecimalParameterDeclaration'
  { -- | The default values of a parameter. If the parameter is a single-value
    -- parameter, a maximum of one default value can be provided.
    defaultValues :: Prelude.Maybe DecimalDefaultValues,
    -- | The configuration that defines the default value of a @Decimal@
    -- parameter when a value has not been set.
    valueWhenUnset :: Prelude.Maybe DecimalValueWhenUnsetConfiguration,
    -- | The value type determines whether the parameter is a single-value or
    -- multi-value parameter.
    parameterValueType :: ParameterValueType,
    -- | The name of the parameter that is being declared.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecimalParameterDeclaration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValues', 'decimalParameterDeclaration_defaultValues' - The default values of a parameter. If the parameter is a single-value
-- parameter, a maximum of one default value can be provided.
--
-- 'valueWhenUnset', 'decimalParameterDeclaration_valueWhenUnset' - The configuration that defines the default value of a @Decimal@
-- parameter when a value has not been set.
--
-- 'parameterValueType', 'decimalParameterDeclaration_parameterValueType' - The value type determines whether the parameter is a single-value or
-- multi-value parameter.
--
-- 'name', 'decimalParameterDeclaration_name' - The name of the parameter that is being declared.
newDecimalParameterDeclaration ::
  -- | 'parameterValueType'
  ParameterValueType ->
  -- | 'name'
  Prelude.Text ->
  DecimalParameterDeclaration
newDecimalParameterDeclaration
  pParameterValueType_
  pName_ =
    DecimalParameterDeclaration'
      { defaultValues =
          Prelude.Nothing,
        valueWhenUnset = Prelude.Nothing,
        parameterValueType = pParameterValueType_,
        name = pName_
      }

-- | The default values of a parameter. If the parameter is a single-value
-- parameter, a maximum of one default value can be provided.
decimalParameterDeclaration_defaultValues :: Lens.Lens' DecimalParameterDeclaration (Prelude.Maybe DecimalDefaultValues)
decimalParameterDeclaration_defaultValues = Lens.lens (\DecimalParameterDeclaration' {defaultValues} -> defaultValues) (\s@DecimalParameterDeclaration' {} a -> s {defaultValues = a} :: DecimalParameterDeclaration)

-- | The configuration that defines the default value of a @Decimal@
-- parameter when a value has not been set.
decimalParameterDeclaration_valueWhenUnset :: Lens.Lens' DecimalParameterDeclaration (Prelude.Maybe DecimalValueWhenUnsetConfiguration)
decimalParameterDeclaration_valueWhenUnset = Lens.lens (\DecimalParameterDeclaration' {valueWhenUnset} -> valueWhenUnset) (\s@DecimalParameterDeclaration' {} a -> s {valueWhenUnset = a} :: DecimalParameterDeclaration)

-- | The value type determines whether the parameter is a single-value or
-- multi-value parameter.
decimalParameterDeclaration_parameterValueType :: Lens.Lens' DecimalParameterDeclaration ParameterValueType
decimalParameterDeclaration_parameterValueType = Lens.lens (\DecimalParameterDeclaration' {parameterValueType} -> parameterValueType) (\s@DecimalParameterDeclaration' {} a -> s {parameterValueType = a} :: DecimalParameterDeclaration)

-- | The name of the parameter that is being declared.
decimalParameterDeclaration_name :: Lens.Lens' DecimalParameterDeclaration Prelude.Text
decimalParameterDeclaration_name = Lens.lens (\DecimalParameterDeclaration' {name} -> name) (\s@DecimalParameterDeclaration' {} a -> s {name = a} :: DecimalParameterDeclaration)

instance Data.FromJSON DecimalParameterDeclaration where
  parseJSON =
    Data.withObject
      "DecimalParameterDeclaration"
      ( \x ->
          DecimalParameterDeclaration'
            Prelude.<$> (x Data..:? "DefaultValues")
            Prelude.<*> (x Data..:? "ValueWhenUnset")
            Prelude.<*> (x Data..: "ParameterValueType")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable DecimalParameterDeclaration where
  hashWithSalt _salt DecimalParameterDeclaration' {..} =
    _salt
      `Prelude.hashWithSalt` defaultValues
      `Prelude.hashWithSalt` valueWhenUnset
      `Prelude.hashWithSalt` parameterValueType
      `Prelude.hashWithSalt` name

instance Prelude.NFData DecimalParameterDeclaration where
  rnf DecimalParameterDeclaration' {..} =
    Prelude.rnf defaultValues `Prelude.seq`
      Prelude.rnf valueWhenUnset `Prelude.seq`
        Prelude.rnf parameterValueType `Prelude.seq`
          Prelude.rnf name

instance Data.ToJSON DecimalParameterDeclaration where
  toJSON DecimalParameterDeclaration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultValues" Data..=) Prelude.<$> defaultValues,
            ("ValueWhenUnset" Data..=)
              Prelude.<$> valueWhenUnset,
            Prelude.Just
              ("ParameterValueType" Data..= parameterValueType),
            Prelude.Just ("Name" Data..= name)
          ]
      )
