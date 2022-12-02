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
-- Module      : Amazonka.HoneyCode.Types.Cell
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.Cell where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types.Format
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a single cell in a table.
--
-- /See:/ 'newCell' smart constructor.
data Cell = Cell'
  { -- | The format of the cell. If this field is empty, then the format is
    -- either not specified in the workbook or the format is set to AUTO.
    format :: Prelude.Maybe Format,
    -- | The formatted value of the cell. This is the value that you see
    -- displayed in the cell in the UI.
    --
    -- Note that the formatted value of a cell is always represented as a
    -- string irrespective of the data that is stored in the cell. For example,
    -- if a cell contains a date, the formatted value of the cell is the string
    -- representation of the formatted date being shown in the cell in the UI.
    -- See details in the rawValue field below for how cells of different
    -- formats will have different raw and formatted values.
    formattedValue :: Prelude.Maybe Prelude.Text,
    -- | The formula contained in the cell. This field is empty if a cell does
    -- not have a formula.
    formula :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A list of formatted values of the cell. This field is only returned when
    -- the cell is ROWSET format (aka multi-select or multi-record picklist).
    -- Values in the list are always represented as strings. The formattedValue
    -- field will be empty if this field is returned.
    formattedValues :: Prelude.Maybe [Prelude.Text],
    -- | The raw value of the data contained in the cell. The raw value depends
    -- on the format of the data in the cell. However the attribute in the API
    -- return value is always a string containing the raw value.
    --
    -- Cells with format DATE, DATE_TIME or TIME have the raw value as a
    -- floating point number where the whole number represents the number of
    -- days since 1\/1\/1900 and the fractional part represents the fraction of
    -- the day since midnight. For example, a cell with date 11\/3\/2020 has
    -- the raw value \"44138\". A cell with the time 9:00 AM has the raw value
    -- \"0.375\" and a cell with date\/time value of 11\/3\/2020 9:00 AM has
    -- the raw value \"44138.375\". Notice that even though the raw value is a
    -- number in all three cases, it is still represented as a string.
    --
    -- Cells with format NUMBER, CURRENCY, PERCENTAGE and ACCOUNTING have the
    -- raw value of the data as the number representing the data being
    -- displayed. For example, the number 1.325 with two decimal places in the
    -- format will have it\'s raw value as \"1.325\" and formatted value as
    -- \"1.33\". A currency value for $10 will have the raw value as \"10\" and
    -- formatted value as \"$10.00\". A value representing 20% with two decimal
    -- places in the format will have its raw value as \"0.2\" and the
    -- formatted value as \"20.00%\". An accounting value of -$25 will have
    -- \"-25\" as the raw value and \"$ (25.00)\" as the formatted value.
    --
    -- Cells with format TEXT will have the raw text as the raw value. For
    -- example, a cell with text \"John Smith\" will have \"John Smith\" as
    -- both the raw value and the formatted value.
    --
    -- Cells with format CONTACT will have the name of the contact as a
    -- formatted value and the email address of the contact as the raw value.
    -- For example, a contact for John Smith will have \"John Smith\" as the
    -- formatted value and \"john.smith\@example.com\" as the raw value.
    --
    -- Cells with format ROWLINK (aka picklist) will have the first column of
    -- the linked row as the formatted value and the row id of the linked row
    -- as the raw value. For example, a cell containing a picklist to a table
    -- that displays task status might have \"Completed\" as the formatted
    -- value and
    -- \"row:dfcefaee-5b37-4355-8f28-40c3e4ff5dd4\/ca432b2f-b8eb-431d-9fb5-cbe0342f9f03\"
    -- as the raw value.
    --
    -- Cells with format ROWSET (aka multi-select or multi-record picklist)
    -- will by default have the first column of each of the linked rows as the
    -- formatted value in the list, and the rowset id of the linked rows as the
    -- raw value. For example, a cell containing a multi-select picklist to a
    -- table that contains items might have \"Item A\", \"Item B\" in the
    -- formatted value list and \"rows:b742c1f4-6cb0-4650-a845-35eb86fcc2bb\/
    -- [fdea123b-8f68-474a-aa8a-5ff87aa333af,6daf41f0-a138-4eee-89da-123086d36ecf]\"
    -- as the raw value.
    --
    -- Cells with format ATTACHMENT will have the name of the attachment as the
    -- formatted value and the attachment id as the raw value. For example, a
    -- cell containing an attachment named \"image.jpeg\" will have
    -- \"image.jpeg\" as the formatted value and
    -- \"attachment:ca432b2f-b8eb-431d-9fb5-cbe0342f9f03\" as the raw value.
    --
    -- Cells with format AUTO or cells without any format that are
    -- auto-detected as one of the formats above will contain the raw and
    -- formatted values as mentioned above, based on the auto-detected formats.
    -- If there is no auto-detected format, the raw and formatted values will
    -- be the same as the data in the cell.
    rawValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Cell' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'cell_format' - The format of the cell. If this field is empty, then the format is
-- either not specified in the workbook or the format is set to AUTO.
--
-- 'formattedValue', 'cell_formattedValue' - The formatted value of the cell. This is the value that you see
-- displayed in the cell in the UI.
--
-- Note that the formatted value of a cell is always represented as a
-- string irrespective of the data that is stored in the cell. For example,
-- if a cell contains a date, the formatted value of the cell is the string
-- representation of the formatted date being shown in the cell in the UI.
-- See details in the rawValue field below for how cells of different
-- formats will have different raw and formatted values.
--
-- 'formula', 'cell_formula' - The formula contained in the cell. This field is empty if a cell does
-- not have a formula.
--
-- 'formattedValues', 'cell_formattedValues' - A list of formatted values of the cell. This field is only returned when
-- the cell is ROWSET format (aka multi-select or multi-record picklist).
-- Values in the list are always represented as strings. The formattedValue
-- field will be empty if this field is returned.
--
-- 'rawValue', 'cell_rawValue' - The raw value of the data contained in the cell. The raw value depends
-- on the format of the data in the cell. However the attribute in the API
-- return value is always a string containing the raw value.
--
-- Cells with format DATE, DATE_TIME or TIME have the raw value as a
-- floating point number where the whole number represents the number of
-- days since 1\/1\/1900 and the fractional part represents the fraction of
-- the day since midnight. For example, a cell with date 11\/3\/2020 has
-- the raw value \"44138\". A cell with the time 9:00 AM has the raw value
-- \"0.375\" and a cell with date\/time value of 11\/3\/2020 9:00 AM has
-- the raw value \"44138.375\". Notice that even though the raw value is a
-- number in all three cases, it is still represented as a string.
--
-- Cells with format NUMBER, CURRENCY, PERCENTAGE and ACCOUNTING have the
-- raw value of the data as the number representing the data being
-- displayed. For example, the number 1.325 with two decimal places in the
-- format will have it\'s raw value as \"1.325\" and formatted value as
-- \"1.33\". A currency value for $10 will have the raw value as \"10\" and
-- formatted value as \"$10.00\". A value representing 20% with two decimal
-- places in the format will have its raw value as \"0.2\" and the
-- formatted value as \"20.00%\". An accounting value of -$25 will have
-- \"-25\" as the raw value and \"$ (25.00)\" as the formatted value.
--
-- Cells with format TEXT will have the raw text as the raw value. For
-- example, a cell with text \"John Smith\" will have \"John Smith\" as
-- both the raw value and the formatted value.
--
-- Cells with format CONTACT will have the name of the contact as a
-- formatted value and the email address of the contact as the raw value.
-- For example, a contact for John Smith will have \"John Smith\" as the
-- formatted value and \"john.smith\@example.com\" as the raw value.
--
-- Cells with format ROWLINK (aka picklist) will have the first column of
-- the linked row as the formatted value and the row id of the linked row
-- as the raw value. For example, a cell containing a picklist to a table
-- that displays task status might have \"Completed\" as the formatted
-- value and
-- \"row:dfcefaee-5b37-4355-8f28-40c3e4ff5dd4\/ca432b2f-b8eb-431d-9fb5-cbe0342f9f03\"
-- as the raw value.
--
-- Cells with format ROWSET (aka multi-select or multi-record picklist)
-- will by default have the first column of each of the linked rows as the
-- formatted value in the list, and the rowset id of the linked rows as the
-- raw value. For example, a cell containing a multi-select picklist to a
-- table that contains items might have \"Item A\", \"Item B\" in the
-- formatted value list and \"rows:b742c1f4-6cb0-4650-a845-35eb86fcc2bb\/
-- [fdea123b-8f68-474a-aa8a-5ff87aa333af,6daf41f0-a138-4eee-89da-123086d36ecf]\"
-- as the raw value.
--
-- Cells with format ATTACHMENT will have the name of the attachment as the
-- formatted value and the attachment id as the raw value. For example, a
-- cell containing an attachment named \"image.jpeg\" will have
-- \"image.jpeg\" as the formatted value and
-- \"attachment:ca432b2f-b8eb-431d-9fb5-cbe0342f9f03\" as the raw value.
--
-- Cells with format AUTO or cells without any format that are
-- auto-detected as one of the formats above will contain the raw and
-- formatted values as mentioned above, based on the auto-detected formats.
-- If there is no auto-detected format, the raw and formatted values will
-- be the same as the data in the cell.
newCell ::
  Cell
newCell =
  Cell'
    { format = Prelude.Nothing,
      formattedValue = Prelude.Nothing,
      formula = Prelude.Nothing,
      formattedValues = Prelude.Nothing,
      rawValue = Prelude.Nothing
    }

-- | The format of the cell. If this field is empty, then the format is
-- either not specified in the workbook or the format is set to AUTO.
cell_format :: Lens.Lens' Cell (Prelude.Maybe Format)
cell_format = Lens.lens (\Cell' {format} -> format) (\s@Cell' {} a -> s {format = a} :: Cell)

-- | The formatted value of the cell. This is the value that you see
-- displayed in the cell in the UI.
--
-- Note that the formatted value of a cell is always represented as a
-- string irrespective of the data that is stored in the cell. For example,
-- if a cell contains a date, the formatted value of the cell is the string
-- representation of the formatted date being shown in the cell in the UI.
-- See details in the rawValue field below for how cells of different
-- formats will have different raw and formatted values.
cell_formattedValue :: Lens.Lens' Cell (Prelude.Maybe Prelude.Text)
cell_formattedValue = Lens.lens (\Cell' {formattedValue} -> formattedValue) (\s@Cell' {} a -> s {formattedValue = a} :: Cell)

-- | The formula contained in the cell. This field is empty if a cell does
-- not have a formula.
cell_formula :: Lens.Lens' Cell (Prelude.Maybe Prelude.Text)
cell_formula = Lens.lens (\Cell' {formula} -> formula) (\s@Cell' {} a -> s {formula = a} :: Cell) Prelude.. Lens.mapping Data._Sensitive

-- | A list of formatted values of the cell. This field is only returned when
-- the cell is ROWSET format (aka multi-select or multi-record picklist).
-- Values in the list are always represented as strings. The formattedValue
-- field will be empty if this field is returned.
cell_formattedValues :: Lens.Lens' Cell (Prelude.Maybe [Prelude.Text])
cell_formattedValues = Lens.lens (\Cell' {formattedValues} -> formattedValues) (\s@Cell' {} a -> s {formattedValues = a} :: Cell) Prelude.. Lens.mapping Lens.coerced

-- | The raw value of the data contained in the cell. The raw value depends
-- on the format of the data in the cell. However the attribute in the API
-- return value is always a string containing the raw value.
--
-- Cells with format DATE, DATE_TIME or TIME have the raw value as a
-- floating point number where the whole number represents the number of
-- days since 1\/1\/1900 and the fractional part represents the fraction of
-- the day since midnight. For example, a cell with date 11\/3\/2020 has
-- the raw value \"44138\". A cell with the time 9:00 AM has the raw value
-- \"0.375\" and a cell with date\/time value of 11\/3\/2020 9:00 AM has
-- the raw value \"44138.375\". Notice that even though the raw value is a
-- number in all three cases, it is still represented as a string.
--
-- Cells with format NUMBER, CURRENCY, PERCENTAGE and ACCOUNTING have the
-- raw value of the data as the number representing the data being
-- displayed. For example, the number 1.325 with two decimal places in the
-- format will have it\'s raw value as \"1.325\" and formatted value as
-- \"1.33\". A currency value for $10 will have the raw value as \"10\" and
-- formatted value as \"$10.00\". A value representing 20% with two decimal
-- places in the format will have its raw value as \"0.2\" and the
-- formatted value as \"20.00%\". An accounting value of -$25 will have
-- \"-25\" as the raw value and \"$ (25.00)\" as the formatted value.
--
-- Cells with format TEXT will have the raw text as the raw value. For
-- example, a cell with text \"John Smith\" will have \"John Smith\" as
-- both the raw value and the formatted value.
--
-- Cells with format CONTACT will have the name of the contact as a
-- formatted value and the email address of the contact as the raw value.
-- For example, a contact for John Smith will have \"John Smith\" as the
-- formatted value and \"john.smith\@example.com\" as the raw value.
--
-- Cells with format ROWLINK (aka picklist) will have the first column of
-- the linked row as the formatted value and the row id of the linked row
-- as the raw value. For example, a cell containing a picklist to a table
-- that displays task status might have \"Completed\" as the formatted
-- value and
-- \"row:dfcefaee-5b37-4355-8f28-40c3e4ff5dd4\/ca432b2f-b8eb-431d-9fb5-cbe0342f9f03\"
-- as the raw value.
--
-- Cells with format ROWSET (aka multi-select or multi-record picklist)
-- will by default have the first column of each of the linked rows as the
-- formatted value in the list, and the rowset id of the linked rows as the
-- raw value. For example, a cell containing a multi-select picklist to a
-- table that contains items might have \"Item A\", \"Item B\" in the
-- formatted value list and \"rows:b742c1f4-6cb0-4650-a845-35eb86fcc2bb\/
-- [fdea123b-8f68-474a-aa8a-5ff87aa333af,6daf41f0-a138-4eee-89da-123086d36ecf]\"
-- as the raw value.
--
-- Cells with format ATTACHMENT will have the name of the attachment as the
-- formatted value and the attachment id as the raw value. For example, a
-- cell containing an attachment named \"image.jpeg\" will have
-- \"image.jpeg\" as the formatted value and
-- \"attachment:ca432b2f-b8eb-431d-9fb5-cbe0342f9f03\" as the raw value.
--
-- Cells with format AUTO or cells without any format that are
-- auto-detected as one of the formats above will contain the raw and
-- formatted values as mentioned above, based on the auto-detected formats.
-- If there is no auto-detected format, the raw and formatted values will
-- be the same as the data in the cell.
cell_rawValue :: Lens.Lens' Cell (Prelude.Maybe Prelude.Text)
cell_rawValue = Lens.lens (\Cell' {rawValue} -> rawValue) (\s@Cell' {} a -> s {rawValue = a} :: Cell)

instance Data.FromJSON Cell where
  parseJSON =
    Data.withObject
      "Cell"
      ( \x ->
          Cell'
            Prelude.<$> (x Data..:? "format")
            Prelude.<*> (x Data..:? "formattedValue")
            Prelude.<*> (x Data..:? "formula")
            Prelude.<*> ( x Data..:? "formattedValues"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "rawValue")
      )

instance Prelude.Hashable Cell where
  hashWithSalt _salt Cell' {..} =
    _salt `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` formattedValue
      `Prelude.hashWithSalt` formula
      `Prelude.hashWithSalt` formattedValues
      `Prelude.hashWithSalt` rawValue

instance Prelude.NFData Cell where
  rnf Cell' {..} =
    Prelude.rnf format
      `Prelude.seq` Prelude.rnf formattedValue
      `Prelude.seq` Prelude.rnf formula
      `Prelude.seq` Prelude.rnf formattedValues
      `Prelude.seq` Prelude.rnf rawValue
