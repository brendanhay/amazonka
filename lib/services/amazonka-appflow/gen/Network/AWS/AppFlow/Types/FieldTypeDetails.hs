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
-- Module      : Network.AWS.AppFlow.Types.FieldTypeDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppFlow.Types.FieldTypeDetails where

import Network.AWS.AppFlow.Types.Operator
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains details regarding the supported field type and the operators
-- that can be applied for filtering.
--
-- /See:/ 'newFieldTypeDetails' smart constructor.
data FieldTypeDetails = FieldTypeDetails'
  { -- | The list of values that a field can contain. For example, a Boolean
    -- @fieldType@ can have two values: \"true\" and \"false\".
    supportedValues :: Prelude.Maybe [Prelude.Text],
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
-- 'supportedValues', 'fieldTypeDetails_supportedValues' - The list of values that a field can contain. For example, a Boolean
-- @fieldType@ can have two values: \"true\" and \"false\".
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
    { supportedValues =
        Prelude.Nothing,
      fieldType = pFieldType_,
      filterOperators = Prelude.mempty
    }

-- | The list of values that a field can contain. For example, a Boolean
-- @fieldType@ can have two values: \"true\" and \"false\".
fieldTypeDetails_supportedValues :: Lens.Lens' FieldTypeDetails (Prelude.Maybe [Prelude.Text])
fieldTypeDetails_supportedValues = Lens.lens (\FieldTypeDetails' {supportedValues} -> supportedValues) (\s@FieldTypeDetails' {} a -> s {supportedValues = a} :: FieldTypeDetails) Prelude.. Lens.mapping Lens.coerced

-- | The type of field, such as string, integer, date, and so on.
fieldTypeDetails_fieldType :: Lens.Lens' FieldTypeDetails Prelude.Text
fieldTypeDetails_fieldType = Lens.lens (\FieldTypeDetails' {fieldType} -> fieldType) (\s@FieldTypeDetails' {} a -> s {fieldType = a} :: FieldTypeDetails)

-- | The list of operators supported by a field.
fieldTypeDetails_filterOperators :: Lens.Lens' FieldTypeDetails [Operator]
fieldTypeDetails_filterOperators = Lens.lens (\FieldTypeDetails' {filterOperators} -> filterOperators) (\s@FieldTypeDetails' {} a -> s {filterOperators = a} :: FieldTypeDetails) Prelude.. Lens.coerced

instance Core.FromJSON FieldTypeDetails where
  parseJSON =
    Core.withObject
      "FieldTypeDetails"
      ( \x ->
          FieldTypeDetails'
            Prelude.<$> ( x Core..:? "supportedValues"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "fieldType")
            Prelude.<*> ( x Core..:? "filterOperators"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable FieldTypeDetails

instance Prelude.NFData FieldTypeDetails
