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
-- Module      : Amazonka.QuickSight.Types.DisplayFormatOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DisplayFormatOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NegativeFormat
import Amazonka.QuickSight.Types.NumberScale
import Amazonka.QuickSight.Types.TopicNumericSeparatorSymbol

-- | A structure that represents additional options for display formatting.
--
-- /See:/ 'newDisplayFormatOptions' smart constructor.
data DisplayFormatOptions = DisplayFormatOptions'
  { -- | Determines the blank cell format.
    blankCellFormat :: Prelude.Maybe Prelude.Text,
    -- | The currency symbol, such as @USD@.
    currencySymbol :: Prelude.Maybe Prelude.Text,
    -- | Determines the @DateTime@ format.
    dateFormat :: Prelude.Maybe Prelude.Text,
    -- | Determines the decimal separator.
    decimalSeparator :: Prelude.Maybe TopicNumericSeparatorSymbol,
    -- | Determines the number of fraction digits.
    fractionDigits :: Prelude.Maybe Prelude.Int,
    -- | Determines the grouping separator.
    groupingSeparator :: Prelude.Maybe Prelude.Text,
    -- | The negative format.
    negativeFormat :: Prelude.Maybe NegativeFormat,
    -- | The prefix value for a display format.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The suffix value for a display format.
    suffix :: Prelude.Maybe Prelude.Text,
    -- | The unit scaler. Valid values for this structure are: @NONE@, @AUTO@,
    -- @THOUSANDS@, @MILLIONS@, @BILLIONS@, and @TRILLIONS@.
    unitScaler :: Prelude.Maybe NumberScale,
    -- | A Boolean value that indicates whether to use blank cell format.
    useBlankCellFormat :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean value that indicates whether to use grouping.
    useGrouping :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisplayFormatOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blankCellFormat', 'displayFormatOptions_blankCellFormat' - Determines the blank cell format.
--
-- 'currencySymbol', 'displayFormatOptions_currencySymbol' - The currency symbol, such as @USD@.
--
-- 'dateFormat', 'displayFormatOptions_dateFormat' - Determines the @DateTime@ format.
--
-- 'decimalSeparator', 'displayFormatOptions_decimalSeparator' - Determines the decimal separator.
--
-- 'fractionDigits', 'displayFormatOptions_fractionDigits' - Determines the number of fraction digits.
--
-- 'groupingSeparator', 'displayFormatOptions_groupingSeparator' - Determines the grouping separator.
--
-- 'negativeFormat', 'displayFormatOptions_negativeFormat' - The negative format.
--
-- 'prefix', 'displayFormatOptions_prefix' - The prefix value for a display format.
--
-- 'suffix', 'displayFormatOptions_suffix' - The suffix value for a display format.
--
-- 'unitScaler', 'displayFormatOptions_unitScaler' - The unit scaler. Valid values for this structure are: @NONE@, @AUTO@,
-- @THOUSANDS@, @MILLIONS@, @BILLIONS@, and @TRILLIONS@.
--
-- 'useBlankCellFormat', 'displayFormatOptions_useBlankCellFormat' - A Boolean value that indicates whether to use blank cell format.
--
-- 'useGrouping', 'displayFormatOptions_useGrouping' - A Boolean value that indicates whether to use grouping.
newDisplayFormatOptions ::
  DisplayFormatOptions
newDisplayFormatOptions =
  DisplayFormatOptions'
    { blankCellFormat =
        Prelude.Nothing,
      currencySymbol = Prelude.Nothing,
      dateFormat = Prelude.Nothing,
      decimalSeparator = Prelude.Nothing,
      fractionDigits = Prelude.Nothing,
      groupingSeparator = Prelude.Nothing,
      negativeFormat = Prelude.Nothing,
      prefix = Prelude.Nothing,
      suffix = Prelude.Nothing,
      unitScaler = Prelude.Nothing,
      useBlankCellFormat = Prelude.Nothing,
      useGrouping = Prelude.Nothing
    }

-- | Determines the blank cell format.
displayFormatOptions_blankCellFormat :: Lens.Lens' DisplayFormatOptions (Prelude.Maybe Prelude.Text)
displayFormatOptions_blankCellFormat = Lens.lens (\DisplayFormatOptions' {blankCellFormat} -> blankCellFormat) (\s@DisplayFormatOptions' {} a -> s {blankCellFormat = a} :: DisplayFormatOptions)

-- | The currency symbol, such as @USD@.
displayFormatOptions_currencySymbol :: Lens.Lens' DisplayFormatOptions (Prelude.Maybe Prelude.Text)
displayFormatOptions_currencySymbol = Lens.lens (\DisplayFormatOptions' {currencySymbol} -> currencySymbol) (\s@DisplayFormatOptions' {} a -> s {currencySymbol = a} :: DisplayFormatOptions)

-- | Determines the @DateTime@ format.
displayFormatOptions_dateFormat :: Lens.Lens' DisplayFormatOptions (Prelude.Maybe Prelude.Text)
displayFormatOptions_dateFormat = Lens.lens (\DisplayFormatOptions' {dateFormat} -> dateFormat) (\s@DisplayFormatOptions' {} a -> s {dateFormat = a} :: DisplayFormatOptions)

-- | Determines the decimal separator.
displayFormatOptions_decimalSeparator :: Lens.Lens' DisplayFormatOptions (Prelude.Maybe TopicNumericSeparatorSymbol)
displayFormatOptions_decimalSeparator = Lens.lens (\DisplayFormatOptions' {decimalSeparator} -> decimalSeparator) (\s@DisplayFormatOptions' {} a -> s {decimalSeparator = a} :: DisplayFormatOptions)

-- | Determines the number of fraction digits.
displayFormatOptions_fractionDigits :: Lens.Lens' DisplayFormatOptions (Prelude.Maybe Prelude.Int)
displayFormatOptions_fractionDigits = Lens.lens (\DisplayFormatOptions' {fractionDigits} -> fractionDigits) (\s@DisplayFormatOptions' {} a -> s {fractionDigits = a} :: DisplayFormatOptions)

-- | Determines the grouping separator.
displayFormatOptions_groupingSeparator :: Lens.Lens' DisplayFormatOptions (Prelude.Maybe Prelude.Text)
displayFormatOptions_groupingSeparator = Lens.lens (\DisplayFormatOptions' {groupingSeparator} -> groupingSeparator) (\s@DisplayFormatOptions' {} a -> s {groupingSeparator = a} :: DisplayFormatOptions)

-- | The negative format.
displayFormatOptions_negativeFormat :: Lens.Lens' DisplayFormatOptions (Prelude.Maybe NegativeFormat)
displayFormatOptions_negativeFormat = Lens.lens (\DisplayFormatOptions' {negativeFormat} -> negativeFormat) (\s@DisplayFormatOptions' {} a -> s {negativeFormat = a} :: DisplayFormatOptions)

-- | The prefix value for a display format.
displayFormatOptions_prefix :: Lens.Lens' DisplayFormatOptions (Prelude.Maybe Prelude.Text)
displayFormatOptions_prefix = Lens.lens (\DisplayFormatOptions' {prefix} -> prefix) (\s@DisplayFormatOptions' {} a -> s {prefix = a} :: DisplayFormatOptions)

-- | The suffix value for a display format.
displayFormatOptions_suffix :: Lens.Lens' DisplayFormatOptions (Prelude.Maybe Prelude.Text)
displayFormatOptions_suffix = Lens.lens (\DisplayFormatOptions' {suffix} -> suffix) (\s@DisplayFormatOptions' {} a -> s {suffix = a} :: DisplayFormatOptions)

-- | The unit scaler. Valid values for this structure are: @NONE@, @AUTO@,
-- @THOUSANDS@, @MILLIONS@, @BILLIONS@, and @TRILLIONS@.
displayFormatOptions_unitScaler :: Lens.Lens' DisplayFormatOptions (Prelude.Maybe NumberScale)
displayFormatOptions_unitScaler = Lens.lens (\DisplayFormatOptions' {unitScaler} -> unitScaler) (\s@DisplayFormatOptions' {} a -> s {unitScaler = a} :: DisplayFormatOptions)

-- | A Boolean value that indicates whether to use blank cell format.
displayFormatOptions_useBlankCellFormat :: Lens.Lens' DisplayFormatOptions (Prelude.Maybe Prelude.Bool)
displayFormatOptions_useBlankCellFormat = Lens.lens (\DisplayFormatOptions' {useBlankCellFormat} -> useBlankCellFormat) (\s@DisplayFormatOptions' {} a -> s {useBlankCellFormat = a} :: DisplayFormatOptions)

-- | A Boolean value that indicates whether to use grouping.
displayFormatOptions_useGrouping :: Lens.Lens' DisplayFormatOptions (Prelude.Maybe Prelude.Bool)
displayFormatOptions_useGrouping = Lens.lens (\DisplayFormatOptions' {useGrouping} -> useGrouping) (\s@DisplayFormatOptions' {} a -> s {useGrouping = a} :: DisplayFormatOptions)

instance Data.FromJSON DisplayFormatOptions where
  parseJSON =
    Data.withObject
      "DisplayFormatOptions"
      ( \x ->
          DisplayFormatOptions'
            Prelude.<$> (x Data..:? "BlankCellFormat")
            Prelude.<*> (x Data..:? "CurrencySymbol")
            Prelude.<*> (x Data..:? "DateFormat")
            Prelude.<*> (x Data..:? "DecimalSeparator")
            Prelude.<*> (x Data..:? "FractionDigits")
            Prelude.<*> (x Data..:? "GroupingSeparator")
            Prelude.<*> (x Data..:? "NegativeFormat")
            Prelude.<*> (x Data..:? "Prefix")
            Prelude.<*> (x Data..:? "Suffix")
            Prelude.<*> (x Data..:? "UnitScaler")
            Prelude.<*> (x Data..:? "UseBlankCellFormat")
            Prelude.<*> (x Data..:? "UseGrouping")
      )

instance Prelude.Hashable DisplayFormatOptions where
  hashWithSalt _salt DisplayFormatOptions' {..} =
    _salt
      `Prelude.hashWithSalt` blankCellFormat
      `Prelude.hashWithSalt` currencySymbol
      `Prelude.hashWithSalt` dateFormat
      `Prelude.hashWithSalt` decimalSeparator
      `Prelude.hashWithSalt` fractionDigits
      `Prelude.hashWithSalt` groupingSeparator
      `Prelude.hashWithSalt` negativeFormat
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` suffix
      `Prelude.hashWithSalt` unitScaler
      `Prelude.hashWithSalt` useBlankCellFormat
      `Prelude.hashWithSalt` useGrouping

instance Prelude.NFData DisplayFormatOptions where
  rnf DisplayFormatOptions' {..} =
    Prelude.rnf blankCellFormat
      `Prelude.seq` Prelude.rnf currencySymbol
      `Prelude.seq` Prelude.rnf dateFormat
      `Prelude.seq` Prelude.rnf decimalSeparator
      `Prelude.seq` Prelude.rnf fractionDigits
      `Prelude.seq` Prelude.rnf groupingSeparator
      `Prelude.seq` Prelude.rnf negativeFormat
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf suffix
      `Prelude.seq` Prelude.rnf unitScaler
      `Prelude.seq` Prelude.rnf useBlankCellFormat
      `Prelude.seq` Prelude.rnf useGrouping

instance Data.ToJSON DisplayFormatOptions where
  toJSON DisplayFormatOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BlankCellFormat" Data..=)
              Prelude.<$> blankCellFormat,
            ("CurrencySymbol" Data..=)
              Prelude.<$> currencySymbol,
            ("DateFormat" Data..=) Prelude.<$> dateFormat,
            ("DecimalSeparator" Data..=)
              Prelude.<$> decimalSeparator,
            ("FractionDigits" Data..=)
              Prelude.<$> fractionDigits,
            ("GroupingSeparator" Data..=)
              Prelude.<$> groupingSeparator,
            ("NegativeFormat" Data..=)
              Prelude.<$> negativeFormat,
            ("Prefix" Data..=) Prelude.<$> prefix,
            ("Suffix" Data..=) Prelude.<$> suffix,
            ("UnitScaler" Data..=) Prelude.<$> unitScaler,
            ("UseBlankCellFormat" Data..=)
              Prelude.<$> useBlankCellFormat,
            ("UseGrouping" Data..=) Prelude.<$> useGrouping
          ]
      )
