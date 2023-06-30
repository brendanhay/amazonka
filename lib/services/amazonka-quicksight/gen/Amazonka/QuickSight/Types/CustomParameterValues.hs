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
-- Module      : Amazonka.QuickSight.Types.CustomParameterValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CustomParameterValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The customized parameter values.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newCustomParameterValues' smart constructor.
data CustomParameterValues = CustomParameterValues'
  { -- | A list of datetime-type parameter values.
    dateTimeValues :: Prelude.Maybe [Data.Sensitive Data.POSIX],
    -- | A list of decimal-type parameter values.
    decimalValues :: Prelude.Maybe [Data.Sensitive Prelude.Double],
    -- | A list of integer-type parameter values.
    integerValues :: Prelude.Maybe [Data.Sensitive Prelude.Integer],
    -- | A list of string-type parameter values.
    stringValues :: Prelude.Maybe [Data.Sensitive Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomParameterValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateTimeValues', 'customParameterValues_dateTimeValues' - A list of datetime-type parameter values.
--
-- 'decimalValues', 'customParameterValues_decimalValues' - A list of decimal-type parameter values.
--
-- 'integerValues', 'customParameterValues_integerValues' - A list of integer-type parameter values.
--
-- 'stringValues', 'customParameterValues_stringValues' - A list of string-type parameter values.
newCustomParameterValues ::
  CustomParameterValues
newCustomParameterValues =
  CustomParameterValues'
    { dateTimeValues =
        Prelude.Nothing,
      decimalValues = Prelude.Nothing,
      integerValues = Prelude.Nothing,
      stringValues = Prelude.Nothing
    }

-- | A list of datetime-type parameter values.
customParameterValues_dateTimeValues :: Lens.Lens' CustomParameterValues (Prelude.Maybe [Prelude.UTCTime])
customParameterValues_dateTimeValues = Lens.lens (\CustomParameterValues' {dateTimeValues} -> dateTimeValues) (\s@CustomParameterValues' {} a -> s {dateTimeValues = a} :: CustomParameterValues) Prelude.. Lens.mapping Lens.coerced

-- | A list of decimal-type parameter values.
customParameterValues_decimalValues :: Lens.Lens' CustomParameterValues (Prelude.Maybe [Prelude.Double])
customParameterValues_decimalValues = Lens.lens (\CustomParameterValues' {decimalValues} -> decimalValues) (\s@CustomParameterValues' {} a -> s {decimalValues = a} :: CustomParameterValues) Prelude.. Lens.mapping Lens.coerced

-- | A list of integer-type parameter values.
customParameterValues_integerValues :: Lens.Lens' CustomParameterValues (Prelude.Maybe [Prelude.Integer])
customParameterValues_integerValues = Lens.lens (\CustomParameterValues' {integerValues} -> integerValues) (\s@CustomParameterValues' {} a -> s {integerValues = a} :: CustomParameterValues) Prelude.. Lens.mapping Lens.coerced

-- | A list of string-type parameter values.
customParameterValues_stringValues :: Lens.Lens' CustomParameterValues (Prelude.Maybe [Prelude.Text])
customParameterValues_stringValues = Lens.lens (\CustomParameterValues' {stringValues} -> stringValues) (\s@CustomParameterValues' {} a -> s {stringValues = a} :: CustomParameterValues) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CustomParameterValues where
  parseJSON =
    Data.withObject
      "CustomParameterValues"
      ( \x ->
          CustomParameterValues'
            Prelude.<$> (x Data..:? "DateTimeValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "DecimalValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "IntegerValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "StringValues" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CustomParameterValues where
  hashWithSalt _salt CustomParameterValues' {..} =
    _salt
      `Prelude.hashWithSalt` dateTimeValues
      `Prelude.hashWithSalt` decimalValues
      `Prelude.hashWithSalt` integerValues
      `Prelude.hashWithSalt` stringValues

instance Prelude.NFData CustomParameterValues where
  rnf CustomParameterValues' {..} =
    Prelude.rnf dateTimeValues
      `Prelude.seq` Prelude.rnf decimalValues
      `Prelude.seq` Prelude.rnf integerValues
      `Prelude.seq` Prelude.rnf stringValues

instance Data.ToJSON CustomParameterValues where
  toJSON CustomParameterValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateTimeValues" Data..=)
              Prelude.<$> dateTimeValues,
            ("DecimalValues" Data..=) Prelude.<$> decimalValues,
            ("IntegerValues" Data..=) Prelude.<$> integerValues,
            ("StringValues" Data..=) Prelude.<$> stringValues
          ]
      )
