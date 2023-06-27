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
-- Module      : Amazonka.RDSData.Types.Field
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDSData.Types.Field where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDSData.Types.ArrayValue

-- | Contains a value.
--
-- /See:/ 'newField' smart constructor.
data Field = Field'
  { -- | An array of values.
    arrayValue :: Prelude.Maybe ArrayValue,
    -- | A value of BLOB data type.
    blobValue :: Prelude.Maybe Data.Base64,
    -- | A value of Boolean data type.
    booleanValue :: Prelude.Maybe Prelude.Bool,
    -- | A value of double data type.
    doubleValue :: Prelude.Maybe Prelude.Double,
    -- | A NULL value.
    isNull :: Prelude.Maybe Prelude.Bool,
    -- | A value of long data type.
    longValue :: Prelude.Maybe Prelude.Integer,
    -- | A value of string data type.
    stringValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Field' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arrayValue', 'field_arrayValue' - An array of values.
--
-- 'blobValue', 'field_blobValue' - A value of BLOB data type.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'booleanValue', 'field_booleanValue' - A value of Boolean data type.
--
-- 'doubleValue', 'field_doubleValue' - A value of double data type.
--
-- 'isNull', 'field_isNull' - A NULL value.
--
-- 'longValue', 'field_longValue' - A value of long data type.
--
-- 'stringValue', 'field_stringValue' - A value of string data type.
newField ::
  Field
newField =
  Field'
    { arrayValue = Prelude.Nothing,
      blobValue = Prelude.Nothing,
      booleanValue = Prelude.Nothing,
      doubleValue = Prelude.Nothing,
      isNull = Prelude.Nothing,
      longValue = Prelude.Nothing,
      stringValue = Prelude.Nothing
    }

-- | An array of values.
field_arrayValue :: Lens.Lens' Field (Prelude.Maybe ArrayValue)
field_arrayValue = Lens.lens (\Field' {arrayValue} -> arrayValue) (\s@Field' {} a -> s {arrayValue = a} :: Field)

-- | A value of BLOB data type.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
field_blobValue :: Lens.Lens' Field (Prelude.Maybe Prelude.ByteString)
field_blobValue = Lens.lens (\Field' {blobValue} -> blobValue) (\s@Field' {} a -> s {blobValue = a} :: Field) Prelude.. Lens.mapping Data._Base64

-- | A value of Boolean data type.
field_booleanValue :: Lens.Lens' Field (Prelude.Maybe Prelude.Bool)
field_booleanValue = Lens.lens (\Field' {booleanValue} -> booleanValue) (\s@Field' {} a -> s {booleanValue = a} :: Field)

-- | A value of double data type.
field_doubleValue :: Lens.Lens' Field (Prelude.Maybe Prelude.Double)
field_doubleValue = Lens.lens (\Field' {doubleValue} -> doubleValue) (\s@Field' {} a -> s {doubleValue = a} :: Field)

-- | A NULL value.
field_isNull :: Lens.Lens' Field (Prelude.Maybe Prelude.Bool)
field_isNull = Lens.lens (\Field' {isNull} -> isNull) (\s@Field' {} a -> s {isNull = a} :: Field)

-- | A value of long data type.
field_longValue :: Lens.Lens' Field (Prelude.Maybe Prelude.Integer)
field_longValue = Lens.lens (\Field' {longValue} -> longValue) (\s@Field' {} a -> s {longValue = a} :: Field)

-- | A value of string data type.
field_stringValue :: Lens.Lens' Field (Prelude.Maybe Prelude.Text)
field_stringValue = Lens.lens (\Field' {stringValue} -> stringValue) (\s@Field' {} a -> s {stringValue = a} :: Field)

instance Data.FromJSON Field where
  parseJSON =
    Data.withObject
      "Field"
      ( \x ->
          Field'
            Prelude.<$> (x Data..:? "arrayValue")
            Prelude.<*> (x Data..:? "blobValue")
            Prelude.<*> (x Data..:? "booleanValue")
            Prelude.<*> (x Data..:? "doubleValue")
            Prelude.<*> (x Data..:? "isNull")
            Prelude.<*> (x Data..:? "longValue")
            Prelude.<*> (x Data..:? "stringValue")
      )

instance Prelude.Hashable Field where
  hashWithSalt _salt Field' {..} =
    _salt
      `Prelude.hashWithSalt` arrayValue
      `Prelude.hashWithSalt` blobValue
      `Prelude.hashWithSalt` booleanValue
      `Prelude.hashWithSalt` doubleValue
      `Prelude.hashWithSalt` isNull
      `Prelude.hashWithSalt` longValue
      `Prelude.hashWithSalt` stringValue

instance Prelude.NFData Field where
  rnf Field' {..} =
    Prelude.rnf arrayValue
      `Prelude.seq` Prelude.rnf blobValue
      `Prelude.seq` Prelude.rnf booleanValue
      `Prelude.seq` Prelude.rnf doubleValue
      `Prelude.seq` Prelude.rnf isNull
      `Prelude.seq` Prelude.rnf longValue
      `Prelude.seq` Prelude.rnf stringValue

instance Data.ToJSON Field where
  toJSON Field' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("arrayValue" Data..=) Prelude.<$> arrayValue,
            ("blobValue" Data..=) Prelude.<$> blobValue,
            ("booleanValue" Data..=) Prelude.<$> booleanValue,
            ("doubleValue" Data..=) Prelude.<$> doubleValue,
            ("isNull" Data..=) Prelude.<$> isNull,
            ("longValue" Data..=) Prelude.<$> longValue,
            ("stringValue" Data..=) Prelude.<$> stringValue
          ]
      )
