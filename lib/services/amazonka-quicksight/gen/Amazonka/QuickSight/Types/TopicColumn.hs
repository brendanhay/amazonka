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
-- Module      : Amazonka.QuickSight.Types.TopicColumn
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicColumn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AuthorSpecifiedAggregation
import Amazonka.QuickSight.Types.CellValueSynonym
import Amazonka.QuickSight.Types.ColumnDataRole
import Amazonka.QuickSight.Types.ComparativeOrder
import Amazonka.QuickSight.Types.DefaultAggregation
import Amazonka.QuickSight.Types.DefaultFormatting
import Amazonka.QuickSight.Types.SemanticType
import Amazonka.QuickSight.Types.TopicTimeGranularity

-- | Represents a column in a dataset.
--
-- /See:/ 'newTopicColumn' smart constructor.
data TopicColumn = TopicColumn'
  { -- | The type of aggregation that is performed on the column data when it\'s
    -- queried. Valid values for this structure are @SUM@, @MAX@, @MIN@,
    -- @COUNT@, @DISTINCT_COUNT@, and @AVERAGE@.
    aggregation :: Prelude.Maybe DefaultAggregation,
    -- | The list of aggregation types that are allowed for the column. Valid
    -- values for this structure are @COUNT@, @DISTINCT_COUNT@, @MIN@, @MAX@,
    -- @MEDIAN@, @SUM@, @AVERAGE@, @STDEV@, @STDEVP@, @VAR@, @VARP@, and
    -- @PERCENTILE@.
    allowedAggregations :: Prelude.Maybe [AuthorSpecifiedAggregation],
    -- | The other names or aliases for the column cell value.
    cellValueSynonyms :: Prelude.Maybe [CellValueSynonym],
    -- | The role of the column in the data. Valid values are @DIMENSION@ and
    -- @MEASURE@.
    columnDataRole :: Prelude.Maybe ColumnDataRole,
    -- | A description of the column and its contents.
    columnDescription :: Prelude.Maybe Prelude.Text,
    -- | A user-friendly name for the column.
    columnFriendlyName :: Prelude.Maybe Prelude.Text,
    -- | The other names or aliases for the column.
    columnSynonyms :: Prelude.Maybe [Prelude.Text],
    -- | The order in which data is displayed for the column when it\'s used in a
    -- comparative context.
    comparativeOrder :: Prelude.Maybe ComparativeOrder,
    -- | The default formatting used for values in the column.
    defaultFormatting :: Prelude.Maybe DefaultFormatting,
    -- | A Boolean value that indicates whether the column shows in the
    -- autocomplete functionality.
    disableIndexing :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean value that indicates whether the column is included in the
    -- query results.
    isIncludedInTopic :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean value that indicates whether to aggregate the column data when
    -- it\'s used in a filter context.
    neverAggregateInFilter :: Prelude.Maybe Prelude.Bool,
    -- | The list of aggregation types that are not allowed for the column. Valid
    -- values for this structure are @COUNT@, @DISTINCT_COUNT@, @MIN@, @MAX@,
    -- @MEDIAN@, @SUM@, @AVERAGE@, @STDEV@, @STDEVP@, @VAR@, @VARP@, and
    -- @PERCENTILE@.
    notAllowedAggregations :: Prelude.Maybe [AuthorSpecifiedAggregation],
    -- | The semantic type of data contained in the column.
    semanticType :: Prelude.Maybe SemanticType,
    -- | The level of time precision that is used to aggregate @DateTime@ values.
    timeGranularity :: Prelude.Maybe TopicTimeGranularity,
    -- | The name of the column.
    columnName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicColumn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregation', 'topicColumn_aggregation' - The type of aggregation that is performed on the column data when it\'s
-- queried. Valid values for this structure are @SUM@, @MAX@, @MIN@,
-- @COUNT@, @DISTINCT_COUNT@, and @AVERAGE@.
--
-- 'allowedAggregations', 'topicColumn_allowedAggregations' - The list of aggregation types that are allowed for the column. Valid
-- values for this structure are @COUNT@, @DISTINCT_COUNT@, @MIN@, @MAX@,
-- @MEDIAN@, @SUM@, @AVERAGE@, @STDEV@, @STDEVP@, @VAR@, @VARP@, and
-- @PERCENTILE@.
--
-- 'cellValueSynonyms', 'topicColumn_cellValueSynonyms' - The other names or aliases for the column cell value.
--
-- 'columnDataRole', 'topicColumn_columnDataRole' - The role of the column in the data. Valid values are @DIMENSION@ and
-- @MEASURE@.
--
-- 'columnDescription', 'topicColumn_columnDescription' - A description of the column and its contents.
--
-- 'columnFriendlyName', 'topicColumn_columnFriendlyName' - A user-friendly name for the column.
--
-- 'columnSynonyms', 'topicColumn_columnSynonyms' - The other names or aliases for the column.
--
-- 'comparativeOrder', 'topicColumn_comparativeOrder' - The order in which data is displayed for the column when it\'s used in a
-- comparative context.
--
-- 'defaultFormatting', 'topicColumn_defaultFormatting' - The default formatting used for values in the column.
--
-- 'disableIndexing', 'topicColumn_disableIndexing' - A Boolean value that indicates whether the column shows in the
-- autocomplete functionality.
--
-- 'isIncludedInTopic', 'topicColumn_isIncludedInTopic' - A Boolean value that indicates whether the column is included in the
-- query results.
--
-- 'neverAggregateInFilter', 'topicColumn_neverAggregateInFilter' - A Boolean value that indicates whether to aggregate the column data when
-- it\'s used in a filter context.
--
-- 'notAllowedAggregations', 'topicColumn_notAllowedAggregations' - The list of aggregation types that are not allowed for the column. Valid
-- values for this structure are @COUNT@, @DISTINCT_COUNT@, @MIN@, @MAX@,
-- @MEDIAN@, @SUM@, @AVERAGE@, @STDEV@, @STDEVP@, @VAR@, @VARP@, and
-- @PERCENTILE@.
--
-- 'semanticType', 'topicColumn_semanticType' - The semantic type of data contained in the column.
--
-- 'timeGranularity', 'topicColumn_timeGranularity' - The level of time precision that is used to aggregate @DateTime@ values.
--
-- 'columnName', 'topicColumn_columnName' - The name of the column.
newTopicColumn ::
  -- | 'columnName'
  Prelude.Text ->
  TopicColumn
newTopicColumn pColumnName_ =
  TopicColumn'
    { aggregation = Prelude.Nothing,
      allowedAggregations = Prelude.Nothing,
      cellValueSynonyms = Prelude.Nothing,
      columnDataRole = Prelude.Nothing,
      columnDescription = Prelude.Nothing,
      columnFriendlyName = Prelude.Nothing,
      columnSynonyms = Prelude.Nothing,
      comparativeOrder = Prelude.Nothing,
      defaultFormatting = Prelude.Nothing,
      disableIndexing = Prelude.Nothing,
      isIncludedInTopic = Prelude.Nothing,
      neverAggregateInFilter = Prelude.Nothing,
      notAllowedAggregations = Prelude.Nothing,
      semanticType = Prelude.Nothing,
      timeGranularity = Prelude.Nothing,
      columnName = pColumnName_
    }

-- | The type of aggregation that is performed on the column data when it\'s
-- queried. Valid values for this structure are @SUM@, @MAX@, @MIN@,
-- @COUNT@, @DISTINCT_COUNT@, and @AVERAGE@.
topicColumn_aggregation :: Lens.Lens' TopicColumn (Prelude.Maybe DefaultAggregation)
topicColumn_aggregation = Lens.lens (\TopicColumn' {aggregation} -> aggregation) (\s@TopicColumn' {} a -> s {aggregation = a} :: TopicColumn)

-- | The list of aggregation types that are allowed for the column. Valid
-- values for this structure are @COUNT@, @DISTINCT_COUNT@, @MIN@, @MAX@,
-- @MEDIAN@, @SUM@, @AVERAGE@, @STDEV@, @STDEVP@, @VAR@, @VARP@, and
-- @PERCENTILE@.
topicColumn_allowedAggregations :: Lens.Lens' TopicColumn (Prelude.Maybe [AuthorSpecifiedAggregation])
topicColumn_allowedAggregations = Lens.lens (\TopicColumn' {allowedAggregations} -> allowedAggregations) (\s@TopicColumn' {} a -> s {allowedAggregations = a} :: TopicColumn) Prelude.. Lens.mapping Lens.coerced

-- | The other names or aliases for the column cell value.
topicColumn_cellValueSynonyms :: Lens.Lens' TopicColumn (Prelude.Maybe [CellValueSynonym])
topicColumn_cellValueSynonyms = Lens.lens (\TopicColumn' {cellValueSynonyms} -> cellValueSynonyms) (\s@TopicColumn' {} a -> s {cellValueSynonyms = a} :: TopicColumn) Prelude.. Lens.mapping Lens.coerced

-- | The role of the column in the data. Valid values are @DIMENSION@ and
-- @MEASURE@.
topicColumn_columnDataRole :: Lens.Lens' TopicColumn (Prelude.Maybe ColumnDataRole)
topicColumn_columnDataRole = Lens.lens (\TopicColumn' {columnDataRole} -> columnDataRole) (\s@TopicColumn' {} a -> s {columnDataRole = a} :: TopicColumn)

-- | A description of the column and its contents.
topicColumn_columnDescription :: Lens.Lens' TopicColumn (Prelude.Maybe Prelude.Text)
topicColumn_columnDescription = Lens.lens (\TopicColumn' {columnDescription} -> columnDescription) (\s@TopicColumn' {} a -> s {columnDescription = a} :: TopicColumn)

-- | A user-friendly name for the column.
topicColumn_columnFriendlyName :: Lens.Lens' TopicColumn (Prelude.Maybe Prelude.Text)
topicColumn_columnFriendlyName = Lens.lens (\TopicColumn' {columnFriendlyName} -> columnFriendlyName) (\s@TopicColumn' {} a -> s {columnFriendlyName = a} :: TopicColumn)

-- | The other names or aliases for the column.
topicColumn_columnSynonyms :: Lens.Lens' TopicColumn (Prelude.Maybe [Prelude.Text])
topicColumn_columnSynonyms = Lens.lens (\TopicColumn' {columnSynonyms} -> columnSynonyms) (\s@TopicColumn' {} a -> s {columnSynonyms = a} :: TopicColumn) Prelude.. Lens.mapping Lens.coerced

-- | The order in which data is displayed for the column when it\'s used in a
-- comparative context.
topicColumn_comparativeOrder :: Lens.Lens' TopicColumn (Prelude.Maybe ComparativeOrder)
topicColumn_comparativeOrder = Lens.lens (\TopicColumn' {comparativeOrder} -> comparativeOrder) (\s@TopicColumn' {} a -> s {comparativeOrder = a} :: TopicColumn)

-- | The default formatting used for values in the column.
topicColumn_defaultFormatting :: Lens.Lens' TopicColumn (Prelude.Maybe DefaultFormatting)
topicColumn_defaultFormatting = Lens.lens (\TopicColumn' {defaultFormatting} -> defaultFormatting) (\s@TopicColumn' {} a -> s {defaultFormatting = a} :: TopicColumn)

-- | A Boolean value that indicates whether the column shows in the
-- autocomplete functionality.
topicColumn_disableIndexing :: Lens.Lens' TopicColumn (Prelude.Maybe Prelude.Bool)
topicColumn_disableIndexing = Lens.lens (\TopicColumn' {disableIndexing} -> disableIndexing) (\s@TopicColumn' {} a -> s {disableIndexing = a} :: TopicColumn)

-- | A Boolean value that indicates whether the column is included in the
-- query results.
topicColumn_isIncludedInTopic :: Lens.Lens' TopicColumn (Prelude.Maybe Prelude.Bool)
topicColumn_isIncludedInTopic = Lens.lens (\TopicColumn' {isIncludedInTopic} -> isIncludedInTopic) (\s@TopicColumn' {} a -> s {isIncludedInTopic = a} :: TopicColumn)

-- | A Boolean value that indicates whether to aggregate the column data when
-- it\'s used in a filter context.
topicColumn_neverAggregateInFilter :: Lens.Lens' TopicColumn (Prelude.Maybe Prelude.Bool)
topicColumn_neverAggregateInFilter = Lens.lens (\TopicColumn' {neverAggregateInFilter} -> neverAggregateInFilter) (\s@TopicColumn' {} a -> s {neverAggregateInFilter = a} :: TopicColumn)

-- | The list of aggregation types that are not allowed for the column. Valid
-- values for this structure are @COUNT@, @DISTINCT_COUNT@, @MIN@, @MAX@,
-- @MEDIAN@, @SUM@, @AVERAGE@, @STDEV@, @STDEVP@, @VAR@, @VARP@, and
-- @PERCENTILE@.
topicColumn_notAllowedAggregations :: Lens.Lens' TopicColumn (Prelude.Maybe [AuthorSpecifiedAggregation])
topicColumn_notAllowedAggregations = Lens.lens (\TopicColumn' {notAllowedAggregations} -> notAllowedAggregations) (\s@TopicColumn' {} a -> s {notAllowedAggregations = a} :: TopicColumn) Prelude.. Lens.mapping Lens.coerced

-- | The semantic type of data contained in the column.
topicColumn_semanticType :: Lens.Lens' TopicColumn (Prelude.Maybe SemanticType)
topicColumn_semanticType = Lens.lens (\TopicColumn' {semanticType} -> semanticType) (\s@TopicColumn' {} a -> s {semanticType = a} :: TopicColumn)

-- | The level of time precision that is used to aggregate @DateTime@ values.
topicColumn_timeGranularity :: Lens.Lens' TopicColumn (Prelude.Maybe TopicTimeGranularity)
topicColumn_timeGranularity = Lens.lens (\TopicColumn' {timeGranularity} -> timeGranularity) (\s@TopicColumn' {} a -> s {timeGranularity = a} :: TopicColumn)

-- | The name of the column.
topicColumn_columnName :: Lens.Lens' TopicColumn Prelude.Text
topicColumn_columnName = Lens.lens (\TopicColumn' {columnName} -> columnName) (\s@TopicColumn' {} a -> s {columnName = a} :: TopicColumn)

instance Data.FromJSON TopicColumn where
  parseJSON =
    Data.withObject
      "TopicColumn"
      ( \x ->
          TopicColumn'
            Prelude.<$> (x Data..:? "Aggregation")
            Prelude.<*> ( x
                            Data..:? "AllowedAggregations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "CellValueSynonyms"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ColumnDataRole")
            Prelude.<*> (x Data..:? "ColumnDescription")
            Prelude.<*> (x Data..:? "ColumnFriendlyName")
            Prelude.<*> (x Data..:? "ColumnSynonyms" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ComparativeOrder")
            Prelude.<*> (x Data..:? "DefaultFormatting")
            Prelude.<*> (x Data..:? "DisableIndexing")
            Prelude.<*> (x Data..:? "IsIncludedInTopic")
            Prelude.<*> (x Data..:? "NeverAggregateInFilter")
            Prelude.<*> ( x
                            Data..:? "NotAllowedAggregations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SemanticType")
            Prelude.<*> (x Data..:? "TimeGranularity")
            Prelude.<*> (x Data..: "ColumnName")
      )

instance Prelude.Hashable TopicColumn where
  hashWithSalt _salt TopicColumn' {..} =
    _salt
      `Prelude.hashWithSalt` aggregation
      `Prelude.hashWithSalt` allowedAggregations
      `Prelude.hashWithSalt` cellValueSynonyms
      `Prelude.hashWithSalt` columnDataRole
      `Prelude.hashWithSalt` columnDescription
      `Prelude.hashWithSalt` columnFriendlyName
      `Prelude.hashWithSalt` columnSynonyms
      `Prelude.hashWithSalt` comparativeOrder
      `Prelude.hashWithSalt` defaultFormatting
      `Prelude.hashWithSalt` disableIndexing
      `Prelude.hashWithSalt` isIncludedInTopic
      `Prelude.hashWithSalt` neverAggregateInFilter
      `Prelude.hashWithSalt` notAllowedAggregations
      `Prelude.hashWithSalt` semanticType
      `Prelude.hashWithSalt` timeGranularity
      `Prelude.hashWithSalt` columnName

instance Prelude.NFData TopicColumn where
  rnf TopicColumn' {..} =
    Prelude.rnf aggregation
      `Prelude.seq` Prelude.rnf allowedAggregations
      `Prelude.seq` Prelude.rnf cellValueSynonyms
      `Prelude.seq` Prelude.rnf columnDataRole
      `Prelude.seq` Prelude.rnf columnDescription
      `Prelude.seq` Prelude.rnf columnFriendlyName
      `Prelude.seq` Prelude.rnf columnSynonyms
      `Prelude.seq` Prelude.rnf comparativeOrder
      `Prelude.seq` Prelude.rnf defaultFormatting
      `Prelude.seq` Prelude.rnf disableIndexing
      `Prelude.seq` Prelude.rnf isIncludedInTopic
      `Prelude.seq` Prelude.rnf neverAggregateInFilter
      `Prelude.seq` Prelude.rnf notAllowedAggregations
      `Prelude.seq` Prelude.rnf semanticType
      `Prelude.seq` Prelude.rnf timeGranularity
      `Prelude.seq` Prelude.rnf columnName

instance Data.ToJSON TopicColumn where
  toJSON TopicColumn' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Aggregation" Data..=) Prelude.<$> aggregation,
            ("AllowedAggregations" Data..=)
              Prelude.<$> allowedAggregations,
            ("CellValueSynonyms" Data..=)
              Prelude.<$> cellValueSynonyms,
            ("ColumnDataRole" Data..=)
              Prelude.<$> columnDataRole,
            ("ColumnDescription" Data..=)
              Prelude.<$> columnDescription,
            ("ColumnFriendlyName" Data..=)
              Prelude.<$> columnFriendlyName,
            ("ColumnSynonyms" Data..=)
              Prelude.<$> columnSynonyms,
            ("ComparativeOrder" Data..=)
              Prelude.<$> comparativeOrder,
            ("DefaultFormatting" Data..=)
              Prelude.<$> defaultFormatting,
            ("DisableIndexing" Data..=)
              Prelude.<$> disableIndexing,
            ("IsIncludedInTopic" Data..=)
              Prelude.<$> isIncludedInTopic,
            ("NeverAggregateInFilter" Data..=)
              Prelude.<$> neverAggregateInFilter,
            ("NotAllowedAggregations" Data..=)
              Prelude.<$> notAllowedAggregations,
            ("SemanticType" Data..=) Prelude.<$> semanticType,
            ("TimeGranularity" Data..=)
              Prelude.<$> timeGranularity,
            Prelude.Just ("ColumnName" Data..= columnName)
          ]
      )
