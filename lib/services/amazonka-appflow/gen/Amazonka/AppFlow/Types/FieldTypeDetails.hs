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
-- Module      : Amazonka.AppFlow.Types.FieldTypeDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.FieldTypeDetails where

import Amazonka.AppFlow.Types.Operator
import Amazonka.AppFlow.Types.Range
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details regarding the supported field type and the operators
-- that can be applied for filtering.
--
-- /See:/ 'newFieldTypeDetails' smart constructor.
data FieldTypeDetails = FieldTypeDetails'
  { -- | This is the allowable length range for this field\'s value.
    fieldLengthRange :: Prelude.Maybe Range,
    -- | The range of values this field can hold.
    fieldValueRange :: Prelude.Maybe Range,
    -- | The date format that the field supports.
    supportedDateFormat :: Prelude.Maybe Prelude.Text,
    -- | The list of values that a field can contain. For example, a Boolean
    -- @fieldType@ can have two values: \"true\" and \"false\".
    supportedValues :: Prelude.Maybe [Prelude.Text],
    -- | The regular expression pattern for the field name.
    valueRegexPattern :: Prelude.Maybe Prelude.Text,
    -- | The type of field, such as string, integer, date, and so on.
    fieldType :: Prelude.Text,
    -- | The list of operators supported by a field.
    filterOperators :: [Operator]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldTypeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldLengthRange', 'fieldTypeDetails_fieldLengthRange' - This is the allowable length range for this field\'s value.
--
-- 'fieldValueRange', 'fieldTypeDetails_fieldValueRange' - The range of values this field can hold.
--
-- 'supportedDateFormat', 'fieldTypeDetails_supportedDateFormat' - The date format that the field supports.
--
-- 'supportedValues', 'fieldTypeDetails_supportedValues' - The list of values that a field can contain. For example, a Boolean
-- @fieldType@ can have two values: \"true\" and \"false\".
--
-- 'valueRegexPattern', 'fieldTypeDetails_valueRegexPattern' - The regular expression pattern for the field name.
--
-- 'fieldType', 'fieldTypeDetails_fieldType' - The type of field, such as string, integer, date, and so on.
--
-- 'filterOperators', 'fieldTypeDetails_filterOperators' - The list of operators supported by a field.
newFieldTypeDetails ::
  -- | 'fieldType'
  Prelude.Text ->
  FieldTypeDetails
newFieldTypeDetails pFieldType_ =
  FieldTypeDetails'
    { fieldLengthRange =
        Prelude.Nothing,
      fieldValueRange = Prelude.Nothing,
      supportedDateFormat = Prelude.Nothing,
      supportedValues = Prelude.Nothing,
      valueRegexPattern = Prelude.Nothing,
      fieldType = pFieldType_,
      filterOperators = Prelude.mempty
    }

-- | This is the allowable length range for this field\'s value.
fieldTypeDetails_fieldLengthRange :: Lens.Lens' FieldTypeDetails (Prelude.Maybe Range)
fieldTypeDetails_fieldLengthRange = Lens.lens (\FieldTypeDetails' {fieldLengthRange} -> fieldLengthRange) (\s@FieldTypeDetails' {} a -> s {fieldLengthRange = a} :: FieldTypeDetails)

-- | The range of values this field can hold.
fieldTypeDetails_fieldValueRange :: Lens.Lens' FieldTypeDetails (Prelude.Maybe Range)
fieldTypeDetails_fieldValueRange = Lens.lens (\FieldTypeDetails' {fieldValueRange} -> fieldValueRange) (\s@FieldTypeDetails' {} a -> s {fieldValueRange = a} :: FieldTypeDetails)

-- | The date format that the field supports.
fieldTypeDetails_supportedDateFormat :: Lens.Lens' FieldTypeDetails (Prelude.Maybe Prelude.Text)
fieldTypeDetails_supportedDateFormat = Lens.lens (\FieldTypeDetails' {supportedDateFormat} -> supportedDateFormat) (\s@FieldTypeDetails' {} a -> s {supportedDateFormat = a} :: FieldTypeDetails)

-- | The list of values that a field can contain. For example, a Boolean
-- @fieldType@ can have two values: \"true\" and \"false\".
fieldTypeDetails_supportedValues :: Lens.Lens' FieldTypeDetails (Prelude.Maybe [Prelude.Text])
fieldTypeDetails_supportedValues = Lens.lens (\FieldTypeDetails' {supportedValues} -> supportedValues) (\s@FieldTypeDetails' {} a -> s {supportedValues = a} :: FieldTypeDetails) Prelude.. Lens.mapping Lens.coerced

-- | The regular expression pattern for the field name.
fieldTypeDetails_valueRegexPattern :: Lens.Lens' FieldTypeDetails (Prelude.Maybe Prelude.Text)
fieldTypeDetails_valueRegexPattern = Lens.lens (\FieldTypeDetails' {valueRegexPattern} -> valueRegexPattern) (\s@FieldTypeDetails' {} a -> s {valueRegexPattern = a} :: FieldTypeDetails)

-- | The type of field, such as string, integer, date, and so on.
fieldTypeDetails_fieldType :: Lens.Lens' FieldTypeDetails Prelude.Text
fieldTypeDetails_fieldType = Lens.lens (\FieldTypeDetails' {fieldType} -> fieldType) (\s@FieldTypeDetails' {} a -> s {fieldType = a} :: FieldTypeDetails)

-- | The list of operators supported by a field.
fieldTypeDetails_filterOperators :: Lens.Lens' FieldTypeDetails [Operator]
fieldTypeDetails_filterOperators = Lens.lens (\FieldTypeDetails' {filterOperators} -> filterOperators) (\s@FieldTypeDetails' {} a -> s {filterOperators = a} :: FieldTypeDetails) Prelude.. Lens.coerced

instance Data.FromJSON FieldTypeDetails where
  parseJSON =
    Data.withObject
      "FieldTypeDetails"
      ( \x ->
          FieldTypeDetails'
            Prelude.<$> (x Data..:? "fieldLengthRange")
            Prelude.<*> (x Data..:? "fieldValueRange")
            Prelude.<*> (x Data..:? "supportedDateFormat")
            Prelude.<*> ( x
                            Data..:? "supportedValues"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "valueRegexPattern")
            Prelude.<*> (x Data..: "fieldType")
            Prelude.<*> ( x
                            Data..:? "filterOperators"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable FieldTypeDetails where
  hashWithSalt _salt FieldTypeDetails' {..} =
    _salt
      `Prelude.hashWithSalt` fieldLengthRange
      `Prelude.hashWithSalt` fieldValueRange
      `Prelude.hashWithSalt` supportedDateFormat
      `Prelude.hashWithSalt` supportedValues
      `Prelude.hashWithSalt` valueRegexPattern
      `Prelude.hashWithSalt` fieldType
      `Prelude.hashWithSalt` filterOperators

instance Prelude.NFData FieldTypeDetails where
  rnf FieldTypeDetails' {..} =
    Prelude.rnf fieldLengthRange
      `Prelude.seq` Prelude.rnf fieldValueRange
      `Prelude.seq` Prelude.rnf supportedDateFormat
      `Prelude.seq` Prelude.rnf supportedValues
      `Prelude.seq` Prelude.rnf valueRegexPattern
      `Prelude.seq` Prelude.rnf fieldType
      `Prelude.seq` Prelude.rnf filterOperators
