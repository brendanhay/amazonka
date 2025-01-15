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
-- Module      : Amazonka.QuickSight.Types.StringParameterDeclaration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.StringParameterDeclaration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ParameterValueType
import Amazonka.QuickSight.Types.StringDefaultValues
import Amazonka.QuickSight.Types.StringValueWhenUnsetConfiguration

-- | A parameter declaration for the @String@ data type.
--
-- /See:/ 'newStringParameterDeclaration' smart constructor.
data StringParameterDeclaration = StringParameterDeclaration'
  { -- | The default values of a parameter. If the parameter is a single-value
    -- parameter, a maximum of one default value can be provided.
    defaultValues :: Prelude.Maybe StringDefaultValues,
    -- | The configuration that defines the default value of a @String@ parameter
    -- when a value has not been set.
    valueWhenUnset :: Prelude.Maybe StringValueWhenUnsetConfiguration,
    -- | The value type determines whether the parameter is a single-value or
    -- multi-value parameter.
    parameterValueType :: ParameterValueType,
    -- | The name of the parameter that is being declared.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StringParameterDeclaration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValues', 'stringParameterDeclaration_defaultValues' - The default values of a parameter. If the parameter is a single-value
-- parameter, a maximum of one default value can be provided.
--
-- 'valueWhenUnset', 'stringParameterDeclaration_valueWhenUnset' - The configuration that defines the default value of a @String@ parameter
-- when a value has not been set.
--
-- 'parameterValueType', 'stringParameterDeclaration_parameterValueType' - The value type determines whether the parameter is a single-value or
-- multi-value parameter.
--
-- 'name', 'stringParameterDeclaration_name' - The name of the parameter that is being declared.
newStringParameterDeclaration ::
  -- | 'parameterValueType'
  ParameterValueType ->
  -- | 'name'
  Prelude.Text ->
  StringParameterDeclaration
newStringParameterDeclaration
  pParameterValueType_
  pName_ =
    StringParameterDeclaration'
      { defaultValues =
          Prelude.Nothing,
        valueWhenUnset = Prelude.Nothing,
        parameterValueType = pParameterValueType_,
        name = pName_
      }

-- | The default values of a parameter. If the parameter is a single-value
-- parameter, a maximum of one default value can be provided.
stringParameterDeclaration_defaultValues :: Lens.Lens' StringParameterDeclaration (Prelude.Maybe StringDefaultValues)
stringParameterDeclaration_defaultValues = Lens.lens (\StringParameterDeclaration' {defaultValues} -> defaultValues) (\s@StringParameterDeclaration' {} a -> s {defaultValues = a} :: StringParameterDeclaration)

-- | The configuration that defines the default value of a @String@ parameter
-- when a value has not been set.
stringParameterDeclaration_valueWhenUnset :: Lens.Lens' StringParameterDeclaration (Prelude.Maybe StringValueWhenUnsetConfiguration)
stringParameterDeclaration_valueWhenUnset = Lens.lens (\StringParameterDeclaration' {valueWhenUnset} -> valueWhenUnset) (\s@StringParameterDeclaration' {} a -> s {valueWhenUnset = a} :: StringParameterDeclaration)

-- | The value type determines whether the parameter is a single-value or
-- multi-value parameter.
stringParameterDeclaration_parameterValueType :: Lens.Lens' StringParameterDeclaration ParameterValueType
stringParameterDeclaration_parameterValueType = Lens.lens (\StringParameterDeclaration' {parameterValueType} -> parameterValueType) (\s@StringParameterDeclaration' {} a -> s {parameterValueType = a} :: StringParameterDeclaration)

-- | The name of the parameter that is being declared.
stringParameterDeclaration_name :: Lens.Lens' StringParameterDeclaration Prelude.Text
stringParameterDeclaration_name = Lens.lens (\StringParameterDeclaration' {name} -> name) (\s@StringParameterDeclaration' {} a -> s {name = a} :: StringParameterDeclaration)

instance Data.FromJSON StringParameterDeclaration where
  parseJSON =
    Data.withObject
      "StringParameterDeclaration"
      ( \x ->
          StringParameterDeclaration'
            Prelude.<$> (x Data..:? "DefaultValues")
            Prelude.<*> (x Data..:? "ValueWhenUnset")
            Prelude.<*> (x Data..: "ParameterValueType")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable StringParameterDeclaration where
  hashWithSalt _salt StringParameterDeclaration' {..} =
    _salt
      `Prelude.hashWithSalt` defaultValues
      `Prelude.hashWithSalt` valueWhenUnset
      `Prelude.hashWithSalt` parameterValueType
      `Prelude.hashWithSalt` name

instance Prelude.NFData StringParameterDeclaration where
  rnf StringParameterDeclaration' {..} =
    Prelude.rnf defaultValues `Prelude.seq`
      Prelude.rnf valueWhenUnset `Prelude.seq`
        Prelude.rnf parameterValueType `Prelude.seq`
          Prelude.rnf name

instance Data.ToJSON StringParameterDeclaration where
  toJSON StringParameterDeclaration' {..} =
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
