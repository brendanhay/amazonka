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
-- Module      : Amazonka.QuickSight.Types.TopicFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FilterClass
import Amazonka.QuickSight.Types.NamedFilterType
import Amazonka.QuickSight.Types.TopicCategoryFilter
import Amazonka.QuickSight.Types.TopicDateRangeFilter
import Amazonka.QuickSight.Types.TopicNumericEqualityFilter
import Amazonka.QuickSight.Types.TopicNumericRangeFilter
import Amazonka.QuickSight.Types.TopicRelativeDateFilter

-- | A structure that represents a filter used to select items for a topic.
--
-- /See:/ 'newTopicFilter' smart constructor.
data TopicFilter = TopicFilter'
  { -- | The category filter that is associated with this filter.
    categoryFilter :: Prelude.Maybe TopicCategoryFilter,
    -- | The date range filter.
    dateRangeFilter :: Prelude.Maybe TopicDateRangeFilter,
    -- | The class of the filter. Valid values for this structure are
    -- @ENFORCED_VALUE_FILTER@, @CONDITIONAL_VALUE_FILTER@, and
    -- @NAMED_VALUE_FILTER@.
    filterClass :: Prelude.Maybe FilterClass,
    -- | A description of the filter used to select items for a topic.
    filterDescription :: Prelude.Maybe Prelude.Text,
    -- | The other names or aliases for the filter.
    filterSynonyms :: Prelude.Maybe [Prelude.Text],
    -- | The type of the filter. Valid values for this structure are
    -- @CATEGORY_FILTER@, @NUMERIC_EQUALITY_FILTER@, @NUMERIC_RANGE_FILTER@,
    -- @DATE_RANGE_FILTER@, and @RELATIVE_DATE_FILTER@.
    filterType :: Prelude.Maybe NamedFilterType,
    -- | The numeric equality filter.
    numericEqualityFilter :: Prelude.Maybe TopicNumericEqualityFilter,
    -- | The numeric range filter.
    numericRangeFilter :: Prelude.Maybe TopicNumericRangeFilter,
    -- | The relative date filter.
    relativeDateFilter :: Prelude.Maybe TopicRelativeDateFilter,
    -- | The name of the filter.
    filterName :: Prelude.Text,
    -- | The name of the field that the filter operates on.
    operandFieldName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryFilter', 'topicFilter_categoryFilter' - The category filter that is associated with this filter.
--
-- 'dateRangeFilter', 'topicFilter_dateRangeFilter' - The date range filter.
--
-- 'filterClass', 'topicFilter_filterClass' - The class of the filter. Valid values for this structure are
-- @ENFORCED_VALUE_FILTER@, @CONDITIONAL_VALUE_FILTER@, and
-- @NAMED_VALUE_FILTER@.
--
-- 'filterDescription', 'topicFilter_filterDescription' - A description of the filter used to select items for a topic.
--
-- 'filterSynonyms', 'topicFilter_filterSynonyms' - The other names or aliases for the filter.
--
-- 'filterType', 'topicFilter_filterType' - The type of the filter. Valid values for this structure are
-- @CATEGORY_FILTER@, @NUMERIC_EQUALITY_FILTER@, @NUMERIC_RANGE_FILTER@,
-- @DATE_RANGE_FILTER@, and @RELATIVE_DATE_FILTER@.
--
-- 'numericEqualityFilter', 'topicFilter_numericEqualityFilter' - The numeric equality filter.
--
-- 'numericRangeFilter', 'topicFilter_numericRangeFilter' - The numeric range filter.
--
-- 'relativeDateFilter', 'topicFilter_relativeDateFilter' - The relative date filter.
--
-- 'filterName', 'topicFilter_filterName' - The name of the filter.
--
-- 'operandFieldName', 'topicFilter_operandFieldName' - The name of the field that the filter operates on.
newTopicFilter ::
  -- | 'filterName'
  Prelude.Text ->
  -- | 'operandFieldName'
  Prelude.Text ->
  TopicFilter
newTopicFilter pFilterName_ pOperandFieldName_ =
  TopicFilter'
    { categoryFilter = Prelude.Nothing,
      dateRangeFilter = Prelude.Nothing,
      filterClass = Prelude.Nothing,
      filterDescription = Prelude.Nothing,
      filterSynonyms = Prelude.Nothing,
      filterType = Prelude.Nothing,
      numericEqualityFilter = Prelude.Nothing,
      numericRangeFilter = Prelude.Nothing,
      relativeDateFilter = Prelude.Nothing,
      filterName = pFilterName_,
      operandFieldName = pOperandFieldName_
    }

-- | The category filter that is associated with this filter.
topicFilter_categoryFilter :: Lens.Lens' TopicFilter (Prelude.Maybe TopicCategoryFilter)
topicFilter_categoryFilter = Lens.lens (\TopicFilter' {categoryFilter} -> categoryFilter) (\s@TopicFilter' {} a -> s {categoryFilter = a} :: TopicFilter)

-- | The date range filter.
topicFilter_dateRangeFilter :: Lens.Lens' TopicFilter (Prelude.Maybe TopicDateRangeFilter)
topicFilter_dateRangeFilter = Lens.lens (\TopicFilter' {dateRangeFilter} -> dateRangeFilter) (\s@TopicFilter' {} a -> s {dateRangeFilter = a} :: TopicFilter)

-- | The class of the filter. Valid values for this structure are
-- @ENFORCED_VALUE_FILTER@, @CONDITIONAL_VALUE_FILTER@, and
-- @NAMED_VALUE_FILTER@.
topicFilter_filterClass :: Lens.Lens' TopicFilter (Prelude.Maybe FilterClass)
topicFilter_filterClass = Lens.lens (\TopicFilter' {filterClass} -> filterClass) (\s@TopicFilter' {} a -> s {filterClass = a} :: TopicFilter)

-- | A description of the filter used to select items for a topic.
topicFilter_filterDescription :: Lens.Lens' TopicFilter (Prelude.Maybe Prelude.Text)
topicFilter_filterDescription = Lens.lens (\TopicFilter' {filterDescription} -> filterDescription) (\s@TopicFilter' {} a -> s {filterDescription = a} :: TopicFilter)

-- | The other names or aliases for the filter.
topicFilter_filterSynonyms :: Lens.Lens' TopicFilter (Prelude.Maybe [Prelude.Text])
topicFilter_filterSynonyms = Lens.lens (\TopicFilter' {filterSynonyms} -> filterSynonyms) (\s@TopicFilter' {} a -> s {filterSynonyms = a} :: TopicFilter) Prelude.. Lens.mapping Lens.coerced

-- | The type of the filter. Valid values for this structure are
-- @CATEGORY_FILTER@, @NUMERIC_EQUALITY_FILTER@, @NUMERIC_RANGE_FILTER@,
-- @DATE_RANGE_FILTER@, and @RELATIVE_DATE_FILTER@.
topicFilter_filterType :: Lens.Lens' TopicFilter (Prelude.Maybe NamedFilterType)
topicFilter_filterType = Lens.lens (\TopicFilter' {filterType} -> filterType) (\s@TopicFilter' {} a -> s {filterType = a} :: TopicFilter)

-- | The numeric equality filter.
topicFilter_numericEqualityFilter :: Lens.Lens' TopicFilter (Prelude.Maybe TopicNumericEqualityFilter)
topicFilter_numericEqualityFilter = Lens.lens (\TopicFilter' {numericEqualityFilter} -> numericEqualityFilter) (\s@TopicFilter' {} a -> s {numericEqualityFilter = a} :: TopicFilter)

-- | The numeric range filter.
topicFilter_numericRangeFilter :: Lens.Lens' TopicFilter (Prelude.Maybe TopicNumericRangeFilter)
topicFilter_numericRangeFilter = Lens.lens (\TopicFilter' {numericRangeFilter} -> numericRangeFilter) (\s@TopicFilter' {} a -> s {numericRangeFilter = a} :: TopicFilter)

-- | The relative date filter.
topicFilter_relativeDateFilter :: Lens.Lens' TopicFilter (Prelude.Maybe TopicRelativeDateFilter)
topicFilter_relativeDateFilter = Lens.lens (\TopicFilter' {relativeDateFilter} -> relativeDateFilter) (\s@TopicFilter' {} a -> s {relativeDateFilter = a} :: TopicFilter)

-- | The name of the filter.
topicFilter_filterName :: Lens.Lens' TopicFilter Prelude.Text
topicFilter_filterName = Lens.lens (\TopicFilter' {filterName} -> filterName) (\s@TopicFilter' {} a -> s {filterName = a} :: TopicFilter)

-- | The name of the field that the filter operates on.
topicFilter_operandFieldName :: Lens.Lens' TopicFilter Prelude.Text
topicFilter_operandFieldName = Lens.lens (\TopicFilter' {operandFieldName} -> operandFieldName) (\s@TopicFilter' {} a -> s {operandFieldName = a} :: TopicFilter)

instance Data.FromJSON TopicFilter where
  parseJSON =
    Data.withObject
      "TopicFilter"
      ( \x ->
          TopicFilter'
            Prelude.<$> (x Data..:? "CategoryFilter")
            Prelude.<*> (x Data..:? "DateRangeFilter")
            Prelude.<*> (x Data..:? "FilterClass")
            Prelude.<*> (x Data..:? "FilterDescription")
            Prelude.<*> (x Data..:? "FilterSynonyms" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "FilterType")
            Prelude.<*> (x Data..:? "NumericEqualityFilter")
            Prelude.<*> (x Data..:? "NumericRangeFilter")
            Prelude.<*> (x Data..:? "RelativeDateFilter")
            Prelude.<*> (x Data..: "FilterName")
            Prelude.<*> (x Data..: "OperandFieldName")
      )

instance Prelude.Hashable TopicFilter where
  hashWithSalt _salt TopicFilter' {..} =
    _salt
      `Prelude.hashWithSalt` categoryFilter
      `Prelude.hashWithSalt` dateRangeFilter
      `Prelude.hashWithSalt` filterClass
      `Prelude.hashWithSalt` filterDescription
      `Prelude.hashWithSalt` filterSynonyms
      `Prelude.hashWithSalt` filterType
      `Prelude.hashWithSalt` numericEqualityFilter
      `Prelude.hashWithSalt` numericRangeFilter
      `Prelude.hashWithSalt` relativeDateFilter
      `Prelude.hashWithSalt` filterName
      `Prelude.hashWithSalt` operandFieldName

instance Prelude.NFData TopicFilter where
  rnf TopicFilter' {..} =
    Prelude.rnf categoryFilter
      `Prelude.seq` Prelude.rnf dateRangeFilter
      `Prelude.seq` Prelude.rnf filterClass
      `Prelude.seq` Prelude.rnf filterDescription
      `Prelude.seq` Prelude.rnf filterSynonyms
      `Prelude.seq` Prelude.rnf filterType
      `Prelude.seq` Prelude.rnf numericEqualityFilter
      `Prelude.seq` Prelude.rnf numericRangeFilter
      `Prelude.seq` Prelude.rnf relativeDateFilter
      `Prelude.seq` Prelude.rnf filterName
      `Prelude.seq` Prelude.rnf operandFieldName

instance Data.ToJSON TopicFilter where
  toJSON TopicFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryFilter" Data..=)
              Prelude.<$> categoryFilter,
            ("DateRangeFilter" Data..=)
              Prelude.<$> dateRangeFilter,
            ("FilterClass" Data..=) Prelude.<$> filterClass,
            ("FilterDescription" Data..=)
              Prelude.<$> filterDescription,
            ("FilterSynonyms" Data..=)
              Prelude.<$> filterSynonyms,
            ("FilterType" Data..=) Prelude.<$> filterType,
            ("NumericEqualityFilter" Data..=)
              Prelude.<$> numericEqualityFilter,
            ("NumericRangeFilter" Data..=)
              Prelude.<$> numericRangeFilter,
            ("RelativeDateFilter" Data..=)
              Prelude.<$> relativeDateFilter,
            Prelude.Just ("FilterName" Data..= filterName),
            Prelude.Just
              ("OperandFieldName" Data..= operandFieldName)
          ]
      )
