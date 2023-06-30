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
-- Module      : Amazonka.ConnectCases.Types.FieldValueUnion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.FieldValueUnion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object to store union of Field values.
--
-- /See:/ 'newFieldValueUnion' smart constructor.
data FieldValueUnion = FieldValueUnion'
  { -- | Can be either null, or have a Boolean value type. Only one value can be
    -- provided.
    booleanValue :: Prelude.Maybe Prelude.Bool,
    -- | Can be either null, or have a Double number value type. Only one value
    -- can be provided.
    doubleValue :: Prelude.Maybe Prelude.Double,
    -- | String value type.
    stringValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldValueUnion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'booleanValue', 'fieldValueUnion_booleanValue' - Can be either null, or have a Boolean value type. Only one value can be
-- provided.
--
-- 'doubleValue', 'fieldValueUnion_doubleValue' - Can be either null, or have a Double number value type. Only one value
-- can be provided.
--
-- 'stringValue', 'fieldValueUnion_stringValue' - String value type.
newFieldValueUnion ::
  FieldValueUnion
newFieldValueUnion =
  FieldValueUnion'
    { booleanValue = Prelude.Nothing,
      doubleValue = Prelude.Nothing,
      stringValue = Prelude.Nothing
    }

-- | Can be either null, or have a Boolean value type. Only one value can be
-- provided.
fieldValueUnion_booleanValue :: Lens.Lens' FieldValueUnion (Prelude.Maybe Prelude.Bool)
fieldValueUnion_booleanValue = Lens.lens (\FieldValueUnion' {booleanValue} -> booleanValue) (\s@FieldValueUnion' {} a -> s {booleanValue = a} :: FieldValueUnion)

-- | Can be either null, or have a Double number value type. Only one value
-- can be provided.
fieldValueUnion_doubleValue :: Lens.Lens' FieldValueUnion (Prelude.Maybe Prelude.Double)
fieldValueUnion_doubleValue = Lens.lens (\FieldValueUnion' {doubleValue} -> doubleValue) (\s@FieldValueUnion' {} a -> s {doubleValue = a} :: FieldValueUnion)

-- | String value type.
fieldValueUnion_stringValue :: Lens.Lens' FieldValueUnion (Prelude.Maybe Prelude.Text)
fieldValueUnion_stringValue = Lens.lens (\FieldValueUnion' {stringValue} -> stringValue) (\s@FieldValueUnion' {} a -> s {stringValue = a} :: FieldValueUnion)

instance Data.FromJSON FieldValueUnion where
  parseJSON =
    Data.withObject
      "FieldValueUnion"
      ( \x ->
          FieldValueUnion'
            Prelude.<$> (x Data..:? "booleanValue")
            Prelude.<*> (x Data..:? "doubleValue")
            Prelude.<*> (x Data..:? "stringValue")
      )

instance Prelude.Hashable FieldValueUnion where
  hashWithSalt _salt FieldValueUnion' {..} =
    _salt
      `Prelude.hashWithSalt` booleanValue
      `Prelude.hashWithSalt` doubleValue
      `Prelude.hashWithSalt` stringValue

instance Prelude.NFData FieldValueUnion where
  rnf FieldValueUnion' {..} =
    Prelude.rnf booleanValue
      `Prelude.seq` Prelude.rnf doubleValue
      `Prelude.seq` Prelude.rnf stringValue

instance Data.ToJSON FieldValueUnion where
  toJSON FieldValueUnion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("booleanValue" Data..=) Prelude.<$> booleanValue,
            ("doubleValue" Data..=) Prelude.<$> doubleValue,
            ("stringValue" Data..=) Prelude.<$> stringValue
          ]
      )
