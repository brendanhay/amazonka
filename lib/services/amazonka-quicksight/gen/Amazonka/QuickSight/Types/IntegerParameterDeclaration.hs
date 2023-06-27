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
-- Module      : Amazonka.QuickSight.Types.IntegerParameterDeclaration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.IntegerParameterDeclaration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.IntegerDefaultValues
import Amazonka.QuickSight.Types.IntegerValueWhenUnsetConfiguration
import Amazonka.QuickSight.Types.MappedDataSetParameter
import Amazonka.QuickSight.Types.ParameterValueType

-- | A parameter declaration for the @Integer@ data type.
--
-- /See:/ 'newIntegerParameterDeclaration' smart constructor.
data IntegerParameterDeclaration = IntegerParameterDeclaration'
  { -- | The default values of a parameter. If the parameter is a single-value
    -- parameter, a maximum of one default value can be provided.
    defaultValues :: Prelude.Maybe IntegerDefaultValues,
    mappedDataSetParameters :: Prelude.Maybe [MappedDataSetParameter],
    -- | A parameter declaration for the @Integer@ data type.
    valueWhenUnset :: Prelude.Maybe IntegerValueWhenUnsetConfiguration,
    -- | The value type determines whether the parameter is a single-value or
    -- multi-value parameter.
    parameterValueType :: ParameterValueType,
    -- | The name of the parameter that is being declared.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntegerParameterDeclaration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValues', 'integerParameterDeclaration_defaultValues' - The default values of a parameter. If the parameter is a single-value
-- parameter, a maximum of one default value can be provided.
--
-- 'mappedDataSetParameters', 'integerParameterDeclaration_mappedDataSetParameters' - Undocumented member.
--
-- 'valueWhenUnset', 'integerParameterDeclaration_valueWhenUnset' - A parameter declaration for the @Integer@ data type.
--
-- 'parameterValueType', 'integerParameterDeclaration_parameterValueType' - The value type determines whether the parameter is a single-value or
-- multi-value parameter.
--
-- 'name', 'integerParameterDeclaration_name' - The name of the parameter that is being declared.
newIntegerParameterDeclaration ::
  -- | 'parameterValueType'
  ParameterValueType ->
  -- | 'name'
  Prelude.Text ->
  IntegerParameterDeclaration
newIntegerParameterDeclaration
  pParameterValueType_
  pName_ =
    IntegerParameterDeclaration'
      { defaultValues =
          Prelude.Nothing,
        mappedDataSetParameters = Prelude.Nothing,
        valueWhenUnset = Prelude.Nothing,
        parameterValueType = pParameterValueType_,
        name = pName_
      }

-- | The default values of a parameter. If the parameter is a single-value
-- parameter, a maximum of one default value can be provided.
integerParameterDeclaration_defaultValues :: Lens.Lens' IntegerParameterDeclaration (Prelude.Maybe IntegerDefaultValues)
integerParameterDeclaration_defaultValues = Lens.lens (\IntegerParameterDeclaration' {defaultValues} -> defaultValues) (\s@IntegerParameterDeclaration' {} a -> s {defaultValues = a} :: IntegerParameterDeclaration)

-- | Undocumented member.
integerParameterDeclaration_mappedDataSetParameters :: Lens.Lens' IntegerParameterDeclaration (Prelude.Maybe [MappedDataSetParameter])
integerParameterDeclaration_mappedDataSetParameters = Lens.lens (\IntegerParameterDeclaration' {mappedDataSetParameters} -> mappedDataSetParameters) (\s@IntegerParameterDeclaration' {} a -> s {mappedDataSetParameters = a} :: IntegerParameterDeclaration) Prelude.. Lens.mapping Lens.coerced

-- | A parameter declaration for the @Integer@ data type.
integerParameterDeclaration_valueWhenUnset :: Lens.Lens' IntegerParameterDeclaration (Prelude.Maybe IntegerValueWhenUnsetConfiguration)
integerParameterDeclaration_valueWhenUnset = Lens.lens (\IntegerParameterDeclaration' {valueWhenUnset} -> valueWhenUnset) (\s@IntegerParameterDeclaration' {} a -> s {valueWhenUnset = a} :: IntegerParameterDeclaration)

-- | The value type determines whether the parameter is a single-value or
-- multi-value parameter.
integerParameterDeclaration_parameterValueType :: Lens.Lens' IntegerParameterDeclaration ParameterValueType
integerParameterDeclaration_parameterValueType = Lens.lens (\IntegerParameterDeclaration' {parameterValueType} -> parameterValueType) (\s@IntegerParameterDeclaration' {} a -> s {parameterValueType = a} :: IntegerParameterDeclaration)

-- | The name of the parameter that is being declared.
integerParameterDeclaration_name :: Lens.Lens' IntegerParameterDeclaration Prelude.Text
integerParameterDeclaration_name = Lens.lens (\IntegerParameterDeclaration' {name} -> name) (\s@IntegerParameterDeclaration' {} a -> s {name = a} :: IntegerParameterDeclaration)

instance Data.FromJSON IntegerParameterDeclaration where
  parseJSON =
    Data.withObject
      "IntegerParameterDeclaration"
      ( \x ->
          IntegerParameterDeclaration'
            Prelude.<$> (x Data..:? "DefaultValues")
            Prelude.<*> ( x
                            Data..:? "MappedDataSetParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ValueWhenUnset")
            Prelude.<*> (x Data..: "ParameterValueType")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable IntegerParameterDeclaration where
  hashWithSalt _salt IntegerParameterDeclaration' {..} =
    _salt
      `Prelude.hashWithSalt` defaultValues
      `Prelude.hashWithSalt` mappedDataSetParameters
      `Prelude.hashWithSalt` valueWhenUnset
      `Prelude.hashWithSalt` parameterValueType
      `Prelude.hashWithSalt` name

instance Prelude.NFData IntegerParameterDeclaration where
  rnf IntegerParameterDeclaration' {..} =
    Prelude.rnf defaultValues
      `Prelude.seq` Prelude.rnf mappedDataSetParameters
      `Prelude.seq` Prelude.rnf valueWhenUnset
      `Prelude.seq` Prelude.rnf parameterValueType
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON IntegerParameterDeclaration where
  toJSON IntegerParameterDeclaration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultValues" Data..=) Prelude.<$> defaultValues,
            ("MappedDataSetParameters" Data..=)
              Prelude.<$> mappedDataSetParameters,
            ("ValueWhenUnset" Data..=)
              Prelude.<$> valueWhenUnset,
            Prelude.Just
              ("ParameterValueType" Data..= parameterValueType),
            Prelude.Just ("Name" Data..= name)
          ]
      )
