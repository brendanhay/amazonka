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
-- Module      : Amazonka.QuickSight.Types.NewDefaultValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NewDefaultValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration that overrides the existing default values for a
-- dataset parameter that is inherited from another dataset.
--
-- /See:/ 'newNewDefaultValues' smart constructor.
data NewDefaultValues = NewDefaultValues'
  { -- | A list of static default values for a given date time parameter.
    dateTimeStaticValues :: Prelude.Maybe (Prelude.NonEmpty Data.POSIX),
    -- | A list of static default values for a given decimal parameter.
    decimalStaticValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Double),
    -- | A list of static default values for a given integer parameter.
    integerStaticValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Integer),
    -- | A list of static default values for a given string parameter.
    stringStaticValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NewDefaultValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateTimeStaticValues', 'newDefaultValues_dateTimeStaticValues' - A list of static default values for a given date time parameter.
--
-- 'decimalStaticValues', 'newDefaultValues_decimalStaticValues' - A list of static default values for a given decimal parameter.
--
-- 'integerStaticValues', 'newDefaultValues_integerStaticValues' - A list of static default values for a given integer parameter.
--
-- 'stringStaticValues', 'newDefaultValues_stringStaticValues' - A list of static default values for a given string parameter.
newNewDefaultValues ::
  NewDefaultValues
newNewDefaultValues =
  NewDefaultValues'
    { dateTimeStaticValues =
        Prelude.Nothing,
      decimalStaticValues = Prelude.Nothing,
      integerStaticValues = Prelude.Nothing,
      stringStaticValues = Prelude.Nothing
    }

-- | A list of static default values for a given date time parameter.
newDefaultValues_dateTimeStaticValues :: Lens.Lens' NewDefaultValues (Prelude.Maybe (Prelude.NonEmpty Prelude.UTCTime))
newDefaultValues_dateTimeStaticValues = Lens.lens (\NewDefaultValues' {dateTimeStaticValues} -> dateTimeStaticValues) (\s@NewDefaultValues' {} a -> s {dateTimeStaticValues = a} :: NewDefaultValues) Prelude.. Lens.mapping Lens.coerced

-- | A list of static default values for a given decimal parameter.
newDefaultValues_decimalStaticValues :: Lens.Lens' NewDefaultValues (Prelude.Maybe (Prelude.NonEmpty Prelude.Double))
newDefaultValues_decimalStaticValues = Lens.lens (\NewDefaultValues' {decimalStaticValues} -> decimalStaticValues) (\s@NewDefaultValues' {} a -> s {decimalStaticValues = a} :: NewDefaultValues) Prelude.. Lens.mapping Lens.coerced

-- | A list of static default values for a given integer parameter.
newDefaultValues_integerStaticValues :: Lens.Lens' NewDefaultValues (Prelude.Maybe (Prelude.NonEmpty Prelude.Integer))
newDefaultValues_integerStaticValues = Lens.lens (\NewDefaultValues' {integerStaticValues} -> integerStaticValues) (\s@NewDefaultValues' {} a -> s {integerStaticValues = a} :: NewDefaultValues) Prelude.. Lens.mapping Lens.coerced

-- | A list of static default values for a given string parameter.
newDefaultValues_stringStaticValues :: Lens.Lens' NewDefaultValues (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
newDefaultValues_stringStaticValues = Lens.lens (\NewDefaultValues' {stringStaticValues} -> stringStaticValues) (\s@NewDefaultValues' {} a -> s {stringStaticValues = a} :: NewDefaultValues) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON NewDefaultValues where
  parseJSON =
    Data.withObject
      "NewDefaultValues"
      ( \x ->
          NewDefaultValues'
            Prelude.<$> (x Data..:? "DateTimeStaticValues")
            Prelude.<*> (x Data..:? "DecimalStaticValues")
            Prelude.<*> (x Data..:? "IntegerStaticValues")
            Prelude.<*> (x Data..:? "StringStaticValues")
      )

instance Prelude.Hashable NewDefaultValues where
  hashWithSalt _salt NewDefaultValues' {..} =
    _salt
      `Prelude.hashWithSalt` dateTimeStaticValues
      `Prelude.hashWithSalt` decimalStaticValues
      `Prelude.hashWithSalt` integerStaticValues
      `Prelude.hashWithSalt` stringStaticValues

instance Prelude.NFData NewDefaultValues where
  rnf NewDefaultValues' {..} =
    Prelude.rnf dateTimeStaticValues
      `Prelude.seq` Prelude.rnf decimalStaticValues
      `Prelude.seq` Prelude.rnf integerStaticValues
      `Prelude.seq` Prelude.rnf stringStaticValues

instance Data.ToJSON NewDefaultValues where
  toJSON NewDefaultValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateTimeStaticValues" Data..=)
              Prelude.<$> dateTimeStaticValues,
            ("DecimalStaticValues" Data..=)
              Prelude.<$> decimalStaticValues,
            ("IntegerStaticValues" Data..=)
              Prelude.<$> integerStaticValues,
            ("StringStaticValues" Data..=)
              Prelude.<$> stringStaticValues
          ]
      )
