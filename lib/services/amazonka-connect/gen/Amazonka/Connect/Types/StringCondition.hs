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
-- Module      : Amazonka.Connect.Types.StringCondition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.StringCondition where

import Amazonka.Connect.Types.StringComparisonType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A leaf node condition which can be used to specify a string condition.
--
-- The currently supported value for @FieldName@: @name@
--
-- /See:/ 'newStringCondition' smart constructor.
data StringCondition = StringCondition'
  { -- | The name of the field in the string condition.
    fieldName :: Prelude.Maybe Prelude.Text,
    -- | The type of comparison to be made when evaluating the string condition.
    comparisonType :: Prelude.Maybe StringComparisonType,
    -- | The value of the string.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StringCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldName', 'stringCondition_fieldName' - The name of the field in the string condition.
--
-- 'comparisonType', 'stringCondition_comparisonType' - The type of comparison to be made when evaluating the string condition.
--
-- 'value', 'stringCondition_value' - The value of the string.
newStringCondition ::
  StringCondition
newStringCondition =
  StringCondition'
    { fieldName = Prelude.Nothing,
      comparisonType = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the field in the string condition.
stringCondition_fieldName :: Lens.Lens' StringCondition (Prelude.Maybe Prelude.Text)
stringCondition_fieldName = Lens.lens (\StringCondition' {fieldName} -> fieldName) (\s@StringCondition' {} a -> s {fieldName = a} :: StringCondition)

-- | The type of comparison to be made when evaluating the string condition.
stringCondition_comparisonType :: Lens.Lens' StringCondition (Prelude.Maybe StringComparisonType)
stringCondition_comparisonType = Lens.lens (\StringCondition' {comparisonType} -> comparisonType) (\s@StringCondition' {} a -> s {comparisonType = a} :: StringCondition)

-- | The value of the string.
stringCondition_value :: Lens.Lens' StringCondition (Prelude.Maybe Prelude.Text)
stringCondition_value = Lens.lens (\StringCondition' {value} -> value) (\s@StringCondition' {} a -> s {value = a} :: StringCondition)

instance Prelude.Hashable StringCondition where
  hashWithSalt _salt StringCondition' {..} =
    _salt `Prelude.hashWithSalt` fieldName
      `Prelude.hashWithSalt` comparisonType
      `Prelude.hashWithSalt` value

instance Prelude.NFData StringCondition where
  rnf StringCondition' {..} =
    Prelude.rnf fieldName
      `Prelude.seq` Prelude.rnf comparisonType
      `Prelude.seq` Prelude.rnf value

instance Core.ToJSON StringCondition where
  toJSON StringCondition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FieldName" Core..=) Prelude.<$> fieldName,
            ("ComparisonType" Core..=)
              Prelude.<$> comparisonType,
            ("Value" Core..=) Prelude.<$> value
          ]
      )
