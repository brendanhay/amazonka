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
-- Module      : Amazonka.RDSData.Types.ArrayValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDSData.Types.ArrayValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains an array.
--
-- /See:/ 'newArrayValue' smart constructor.
data ArrayValue = ArrayValue'
  { -- | An array of arrays.
    arrayValues :: Prelude.Maybe [ArrayValue],
    -- | An array of Boolean values.
    booleanValues :: Prelude.Maybe [Prelude.Bool],
    -- | An array of floating-point numbers.
    doubleValues :: Prelude.Maybe [Prelude.Double],
    -- | An array of integers.
    longValues :: Prelude.Maybe [Prelude.Integer],
    -- | An array of strings.
    stringValues :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArrayValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arrayValues', 'arrayValue_arrayValues' - An array of arrays.
--
-- 'booleanValues', 'arrayValue_booleanValues' - An array of Boolean values.
--
-- 'doubleValues', 'arrayValue_doubleValues' - An array of floating-point numbers.
--
-- 'longValues', 'arrayValue_longValues' - An array of integers.
--
-- 'stringValues', 'arrayValue_stringValues' - An array of strings.
newArrayValue ::
  ArrayValue
newArrayValue =
  ArrayValue'
    { arrayValues = Prelude.Nothing,
      booleanValues = Prelude.Nothing,
      doubleValues = Prelude.Nothing,
      longValues = Prelude.Nothing,
      stringValues = Prelude.Nothing
    }

-- | An array of arrays.
arrayValue_arrayValues :: Lens.Lens' ArrayValue (Prelude.Maybe [ArrayValue])
arrayValue_arrayValues = Lens.lens (\ArrayValue' {arrayValues} -> arrayValues) (\s@ArrayValue' {} a -> s {arrayValues = a} :: ArrayValue) Prelude.. Lens.mapping Lens.coerced

-- | An array of Boolean values.
arrayValue_booleanValues :: Lens.Lens' ArrayValue (Prelude.Maybe [Prelude.Bool])
arrayValue_booleanValues = Lens.lens (\ArrayValue' {booleanValues} -> booleanValues) (\s@ArrayValue' {} a -> s {booleanValues = a} :: ArrayValue) Prelude.. Lens.mapping Lens.coerced

-- | An array of floating-point numbers.
arrayValue_doubleValues :: Lens.Lens' ArrayValue (Prelude.Maybe [Prelude.Double])
arrayValue_doubleValues = Lens.lens (\ArrayValue' {doubleValues} -> doubleValues) (\s@ArrayValue' {} a -> s {doubleValues = a} :: ArrayValue) Prelude.. Lens.mapping Lens.coerced

-- | An array of integers.
arrayValue_longValues :: Lens.Lens' ArrayValue (Prelude.Maybe [Prelude.Integer])
arrayValue_longValues = Lens.lens (\ArrayValue' {longValues} -> longValues) (\s@ArrayValue' {} a -> s {longValues = a} :: ArrayValue) Prelude.. Lens.mapping Lens.coerced

-- | An array of strings.
arrayValue_stringValues :: Lens.Lens' ArrayValue (Prelude.Maybe [Prelude.Text])
arrayValue_stringValues = Lens.lens (\ArrayValue' {stringValues} -> stringValues) (\s@ArrayValue' {} a -> s {stringValues = a} :: ArrayValue) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ArrayValue where
  parseJSON =
    Data.withObject
      "ArrayValue"
      ( \x ->
          ArrayValue'
            Prelude.<$> (x Data..:? "arrayValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "booleanValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "doubleValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "longValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "stringValues" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ArrayValue where
  hashWithSalt _salt ArrayValue' {..} =
    _salt
      `Prelude.hashWithSalt` arrayValues
      `Prelude.hashWithSalt` booleanValues
      `Prelude.hashWithSalt` doubleValues
      `Prelude.hashWithSalt` longValues
      `Prelude.hashWithSalt` stringValues

instance Prelude.NFData ArrayValue where
  rnf ArrayValue' {..} =
    Prelude.rnf arrayValues
      `Prelude.seq` Prelude.rnf booleanValues
      `Prelude.seq` Prelude.rnf doubleValues
      `Prelude.seq` Prelude.rnf longValues
      `Prelude.seq` Prelude.rnf stringValues

instance Data.ToJSON ArrayValue where
  toJSON ArrayValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("arrayValues" Data..=) Prelude.<$> arrayValues,
            ("booleanValues" Data..=) Prelude.<$> booleanValues,
            ("doubleValues" Data..=) Prelude.<$> doubleValues,
            ("longValues" Data..=) Prelude.<$> longValues,
            ("stringValues" Data..=) Prelude.<$> stringValues
          ]
      )
