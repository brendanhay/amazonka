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
-- Module      : Amazonka.LakeFormation.Types.FilterCondition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.FilterCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types.ComparisonOperator
import Amazonka.LakeFormation.Types.FieldNameString
import qualified Amazonka.Prelude as Prelude

-- | This structure describes the filtering of columns in a table based on a
-- filter condition.
--
-- /See:/ 'newFilterCondition' smart constructor.
data FilterCondition = FilterCondition'
  { -- | The comparison operator used in the filter condition.
    comparisonOperator :: Prelude.Maybe ComparisonOperator,
    -- | The field to filter in the filter condition.
    field :: Prelude.Maybe FieldNameString,
    -- | A string with values used in evaluating the filter condition.
    stringValueList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparisonOperator', 'filterCondition_comparisonOperator' - The comparison operator used in the filter condition.
--
-- 'field', 'filterCondition_field' - The field to filter in the filter condition.
--
-- 'stringValueList', 'filterCondition_stringValueList' - A string with values used in evaluating the filter condition.
newFilterCondition ::
  FilterCondition
newFilterCondition =
  FilterCondition'
    { comparisonOperator =
        Prelude.Nothing,
      field = Prelude.Nothing,
      stringValueList = Prelude.Nothing
    }

-- | The comparison operator used in the filter condition.
filterCondition_comparisonOperator :: Lens.Lens' FilterCondition (Prelude.Maybe ComparisonOperator)
filterCondition_comparisonOperator = Lens.lens (\FilterCondition' {comparisonOperator} -> comparisonOperator) (\s@FilterCondition' {} a -> s {comparisonOperator = a} :: FilterCondition)

-- | The field to filter in the filter condition.
filterCondition_field :: Lens.Lens' FilterCondition (Prelude.Maybe FieldNameString)
filterCondition_field = Lens.lens (\FilterCondition' {field} -> field) (\s@FilterCondition' {} a -> s {field = a} :: FilterCondition)

-- | A string with values used in evaluating the filter condition.
filterCondition_stringValueList :: Lens.Lens' FilterCondition (Prelude.Maybe [Prelude.Text])
filterCondition_stringValueList = Lens.lens (\FilterCondition' {stringValueList} -> stringValueList) (\s@FilterCondition' {} a -> s {stringValueList = a} :: FilterCondition) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable FilterCondition where
  hashWithSalt _salt FilterCondition' {..} =
    _salt `Prelude.hashWithSalt` comparisonOperator
      `Prelude.hashWithSalt` field
      `Prelude.hashWithSalt` stringValueList

instance Prelude.NFData FilterCondition where
  rnf FilterCondition' {..} =
    Prelude.rnf comparisonOperator
      `Prelude.seq` Prelude.rnf field
      `Prelude.seq` Prelude.rnf stringValueList

instance Data.ToJSON FilterCondition where
  toJSON FilterCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ComparisonOperator" Data..=)
              Prelude.<$> comparisonOperator,
            ("Field" Data..=) Prelude.<$> field,
            ("StringValueList" Data..=)
              Prelude.<$> stringValueList
          ]
      )
