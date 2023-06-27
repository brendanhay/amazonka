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
-- Module      : Amazonka.Evidently.Types.VariableValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.VariableValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The value assigned to a feature variation. This structure must contain
-- exactly one field. It can be @boolValue@, @doubleValue@, @longValue@, or
-- @stringValue@.
--
-- /See:/ 'newVariableValue' smart constructor.
data VariableValue = VariableValue'
  { -- | If this feature uses the Boolean variation type, this field contains the
    -- Boolean value of this variation.
    boolValue :: Prelude.Maybe Prelude.Bool,
    -- | If this feature uses the double integer variation type, this field
    -- contains the double integer value of this variation.
    doubleValue :: Prelude.Maybe Prelude.Double,
    -- | If this feature uses the long variation type, this field contains the
    -- long value of this variation.
    longValue :: Prelude.Maybe Prelude.Integer,
    -- | If this feature uses the string variation type, this field contains the
    -- string value of this variation.
    stringValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VariableValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'boolValue', 'variableValue_boolValue' - If this feature uses the Boolean variation type, this field contains the
-- Boolean value of this variation.
--
-- 'doubleValue', 'variableValue_doubleValue' - If this feature uses the double integer variation type, this field
-- contains the double integer value of this variation.
--
-- 'longValue', 'variableValue_longValue' - If this feature uses the long variation type, this field contains the
-- long value of this variation.
--
-- 'stringValue', 'variableValue_stringValue' - If this feature uses the string variation type, this field contains the
-- string value of this variation.
newVariableValue ::
  VariableValue
newVariableValue =
  VariableValue'
    { boolValue = Prelude.Nothing,
      doubleValue = Prelude.Nothing,
      longValue = Prelude.Nothing,
      stringValue = Prelude.Nothing
    }

-- | If this feature uses the Boolean variation type, this field contains the
-- Boolean value of this variation.
variableValue_boolValue :: Lens.Lens' VariableValue (Prelude.Maybe Prelude.Bool)
variableValue_boolValue = Lens.lens (\VariableValue' {boolValue} -> boolValue) (\s@VariableValue' {} a -> s {boolValue = a} :: VariableValue)

-- | If this feature uses the double integer variation type, this field
-- contains the double integer value of this variation.
variableValue_doubleValue :: Lens.Lens' VariableValue (Prelude.Maybe Prelude.Double)
variableValue_doubleValue = Lens.lens (\VariableValue' {doubleValue} -> doubleValue) (\s@VariableValue' {} a -> s {doubleValue = a} :: VariableValue)

-- | If this feature uses the long variation type, this field contains the
-- long value of this variation.
variableValue_longValue :: Lens.Lens' VariableValue (Prelude.Maybe Prelude.Integer)
variableValue_longValue = Lens.lens (\VariableValue' {longValue} -> longValue) (\s@VariableValue' {} a -> s {longValue = a} :: VariableValue)

-- | If this feature uses the string variation type, this field contains the
-- string value of this variation.
variableValue_stringValue :: Lens.Lens' VariableValue (Prelude.Maybe Prelude.Text)
variableValue_stringValue = Lens.lens (\VariableValue' {stringValue} -> stringValue) (\s@VariableValue' {} a -> s {stringValue = a} :: VariableValue)

instance Data.FromJSON VariableValue where
  parseJSON =
    Data.withObject
      "VariableValue"
      ( \x ->
          VariableValue'
            Prelude.<$> (x Data..:? "boolValue")
            Prelude.<*> (x Data..:? "doubleValue")
            Prelude.<*> (x Data..:? "longValue")
            Prelude.<*> (x Data..:? "stringValue")
      )

instance Prelude.Hashable VariableValue where
  hashWithSalt _salt VariableValue' {..} =
    _salt
      `Prelude.hashWithSalt` boolValue
      `Prelude.hashWithSalt` doubleValue
      `Prelude.hashWithSalt` longValue
      `Prelude.hashWithSalt` stringValue

instance Prelude.NFData VariableValue where
  rnf VariableValue' {..} =
    Prelude.rnf boolValue
      `Prelude.seq` Prelude.rnf doubleValue
      `Prelude.seq` Prelude.rnf longValue
      `Prelude.seq` Prelude.rnf stringValue

instance Data.ToJSON VariableValue where
  toJSON VariableValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("boolValue" Data..=) Prelude.<$> boolValue,
            ("doubleValue" Data..=) Prelude.<$> doubleValue,
            ("longValue" Data..=) Prelude.<$> longValue,
            ("stringValue" Data..=) Prelude.<$> stringValue
          ]
      )
