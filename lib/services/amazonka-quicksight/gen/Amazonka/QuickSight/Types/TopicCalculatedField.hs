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
-- Module      : Amazonka.QuickSight.Types.TopicCalculatedField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicCalculatedField where

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

-- | A structure that represents a calculated field.
--
-- /See:/ 'newTopicCalculatedField' smart constructor.
data TopicCalculatedField = TopicCalculatedField'
  { -- | The default aggregation. Valid values for this structure are @SUM@,
    -- @MAX@, @MIN@, @COUNT@, @DISTINCT_COUNT@, and @AVERAGE@.
    aggregation :: Prelude.Maybe DefaultAggregation,
    -- | The list of aggregation types that are allowed for the calculated field.
    -- Valid values for this structure are @COUNT@, @DISTINCT_COUNT@, @MIN@,
    -- @MAX@, @MEDIAN@, @SUM@, @AVERAGE@, @STDEV@, @STDEVP@, @VAR@, @VARP@, and
    -- @PERCENTILE@.
    allowedAggregations :: Prelude.Maybe [AuthorSpecifiedAggregation],
    -- | The calculated field description.
    calculatedFieldDescription :: Prelude.Maybe Prelude.Text,
    -- | The other names or aliases for the calculated field.
    calculatedFieldSynonyms :: Prelude.Maybe [Prelude.Text],
    -- | The other names or aliases for the calculated field cell value.
    cellValueSynonyms :: Prelude.Maybe [CellValueSynonym],
    -- | The column data role for a calculated field. Valid values for this
    -- structure are @DIMENSION@ and @MEASURE@.
    columnDataRole :: Prelude.Maybe ColumnDataRole,
    -- | The order in which data is displayed for the calculated field when it\'s
    -- used in a comparative context.
    comparativeOrder :: Prelude.Maybe ComparativeOrder,
    -- | The default formatting definition.
    defaultFormatting :: Prelude.Maybe DefaultFormatting,
    -- | A Boolean value that indicates if a calculated field is visible in the
    -- autocomplete.
    disableIndexing :: Prelude.Maybe Prelude.Bool,
    -- | A boolean value that indicates if a calculated field is included in the
    -- topic.
    isIncludedInTopic :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean value that indicates whether to never aggregate calculated
    -- field in filters.
    neverAggregateInFilter :: Prelude.Maybe Prelude.Bool,
    -- | The list of aggregation types that are not allowed for the calculated
    -- field. Valid values for this structure are @COUNT@, @DISTINCT_COUNT@,
    -- @MIN@, @MAX@, @MEDIAN@, @SUM@, @AVERAGE@, @STDEV@, @STDEVP@, @VAR@,
    -- @VARP@, and @PERCENTILE@.
    notAllowedAggregations :: Prelude.Maybe [AuthorSpecifiedAggregation],
    -- | The semantic type.
    semanticType :: Prelude.Maybe SemanticType,
    -- | The level of time precision that is used to aggregate @DateTime@ values.
    timeGranularity :: Prelude.Maybe TopicTimeGranularity,
    -- | The calculated field name.
    calculatedFieldName :: Prelude.Text,
    -- | The calculated field expression.
    expression :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicCalculatedField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregation', 'topicCalculatedField_aggregation' - The default aggregation. Valid values for this structure are @SUM@,
-- @MAX@, @MIN@, @COUNT@, @DISTINCT_COUNT@, and @AVERAGE@.
--
-- 'allowedAggregations', 'topicCalculatedField_allowedAggregations' - The list of aggregation types that are allowed for the calculated field.
-- Valid values for this structure are @COUNT@, @DISTINCT_COUNT@, @MIN@,
-- @MAX@, @MEDIAN@, @SUM@, @AVERAGE@, @STDEV@, @STDEVP@, @VAR@, @VARP@, and
-- @PERCENTILE@.
--
-- 'calculatedFieldDescription', 'topicCalculatedField_calculatedFieldDescription' - The calculated field description.
--
-- 'calculatedFieldSynonyms', 'topicCalculatedField_calculatedFieldSynonyms' - The other names or aliases for the calculated field.
--
-- 'cellValueSynonyms', 'topicCalculatedField_cellValueSynonyms' - The other names or aliases for the calculated field cell value.
--
-- 'columnDataRole', 'topicCalculatedField_columnDataRole' - The column data role for a calculated field. Valid values for this
-- structure are @DIMENSION@ and @MEASURE@.
--
-- 'comparativeOrder', 'topicCalculatedField_comparativeOrder' - The order in which data is displayed for the calculated field when it\'s
-- used in a comparative context.
--
-- 'defaultFormatting', 'topicCalculatedField_defaultFormatting' - The default formatting definition.
--
-- 'disableIndexing', 'topicCalculatedField_disableIndexing' - A Boolean value that indicates if a calculated field is visible in the
-- autocomplete.
--
-- 'isIncludedInTopic', 'topicCalculatedField_isIncludedInTopic' - A boolean value that indicates if a calculated field is included in the
-- topic.
--
-- 'neverAggregateInFilter', 'topicCalculatedField_neverAggregateInFilter' - A Boolean value that indicates whether to never aggregate calculated
-- field in filters.
--
-- 'notAllowedAggregations', 'topicCalculatedField_notAllowedAggregations' - The list of aggregation types that are not allowed for the calculated
-- field. Valid values for this structure are @COUNT@, @DISTINCT_COUNT@,
-- @MIN@, @MAX@, @MEDIAN@, @SUM@, @AVERAGE@, @STDEV@, @STDEVP@, @VAR@,
-- @VARP@, and @PERCENTILE@.
--
-- 'semanticType', 'topicCalculatedField_semanticType' - The semantic type.
--
-- 'timeGranularity', 'topicCalculatedField_timeGranularity' - The level of time precision that is used to aggregate @DateTime@ values.
--
-- 'calculatedFieldName', 'topicCalculatedField_calculatedFieldName' - The calculated field name.
--
-- 'expression', 'topicCalculatedField_expression' - The calculated field expression.
newTopicCalculatedField ::
  -- | 'calculatedFieldName'
  Prelude.Text ->
  -- | 'expression'
  Prelude.Text ->
  TopicCalculatedField
newTopicCalculatedField
  pCalculatedFieldName_
  pExpression_ =
    TopicCalculatedField'
      { aggregation =
          Prelude.Nothing,
        allowedAggregations = Prelude.Nothing,
        calculatedFieldDescription = Prelude.Nothing,
        calculatedFieldSynonyms = Prelude.Nothing,
        cellValueSynonyms = Prelude.Nothing,
        columnDataRole = Prelude.Nothing,
        comparativeOrder = Prelude.Nothing,
        defaultFormatting = Prelude.Nothing,
        disableIndexing = Prelude.Nothing,
        isIncludedInTopic = Prelude.Nothing,
        neverAggregateInFilter = Prelude.Nothing,
        notAllowedAggregations = Prelude.Nothing,
        semanticType = Prelude.Nothing,
        timeGranularity = Prelude.Nothing,
        calculatedFieldName = pCalculatedFieldName_,
        expression = Data._Sensitive Lens.# pExpression_
      }

-- | The default aggregation. Valid values for this structure are @SUM@,
-- @MAX@, @MIN@, @COUNT@, @DISTINCT_COUNT@, and @AVERAGE@.
topicCalculatedField_aggregation :: Lens.Lens' TopicCalculatedField (Prelude.Maybe DefaultAggregation)
topicCalculatedField_aggregation = Lens.lens (\TopicCalculatedField' {aggregation} -> aggregation) (\s@TopicCalculatedField' {} a -> s {aggregation = a} :: TopicCalculatedField)

-- | The list of aggregation types that are allowed for the calculated field.
-- Valid values for this structure are @COUNT@, @DISTINCT_COUNT@, @MIN@,
-- @MAX@, @MEDIAN@, @SUM@, @AVERAGE@, @STDEV@, @STDEVP@, @VAR@, @VARP@, and
-- @PERCENTILE@.
topicCalculatedField_allowedAggregations :: Lens.Lens' TopicCalculatedField (Prelude.Maybe [AuthorSpecifiedAggregation])
topicCalculatedField_allowedAggregations = Lens.lens (\TopicCalculatedField' {allowedAggregations} -> allowedAggregations) (\s@TopicCalculatedField' {} a -> s {allowedAggregations = a} :: TopicCalculatedField) Prelude.. Lens.mapping Lens.coerced

-- | The calculated field description.
topicCalculatedField_calculatedFieldDescription :: Lens.Lens' TopicCalculatedField (Prelude.Maybe Prelude.Text)
topicCalculatedField_calculatedFieldDescription = Lens.lens (\TopicCalculatedField' {calculatedFieldDescription} -> calculatedFieldDescription) (\s@TopicCalculatedField' {} a -> s {calculatedFieldDescription = a} :: TopicCalculatedField)

-- | The other names or aliases for the calculated field.
topicCalculatedField_calculatedFieldSynonyms :: Lens.Lens' TopicCalculatedField (Prelude.Maybe [Prelude.Text])
topicCalculatedField_calculatedFieldSynonyms = Lens.lens (\TopicCalculatedField' {calculatedFieldSynonyms} -> calculatedFieldSynonyms) (\s@TopicCalculatedField' {} a -> s {calculatedFieldSynonyms = a} :: TopicCalculatedField) Prelude.. Lens.mapping Lens.coerced

-- | The other names or aliases for the calculated field cell value.
topicCalculatedField_cellValueSynonyms :: Lens.Lens' TopicCalculatedField (Prelude.Maybe [CellValueSynonym])
topicCalculatedField_cellValueSynonyms = Lens.lens (\TopicCalculatedField' {cellValueSynonyms} -> cellValueSynonyms) (\s@TopicCalculatedField' {} a -> s {cellValueSynonyms = a} :: TopicCalculatedField) Prelude.. Lens.mapping Lens.coerced

-- | The column data role for a calculated field. Valid values for this
-- structure are @DIMENSION@ and @MEASURE@.
topicCalculatedField_columnDataRole :: Lens.Lens' TopicCalculatedField (Prelude.Maybe ColumnDataRole)
topicCalculatedField_columnDataRole = Lens.lens (\TopicCalculatedField' {columnDataRole} -> columnDataRole) (\s@TopicCalculatedField' {} a -> s {columnDataRole = a} :: TopicCalculatedField)

-- | The order in which data is displayed for the calculated field when it\'s
-- used in a comparative context.
topicCalculatedField_comparativeOrder :: Lens.Lens' TopicCalculatedField (Prelude.Maybe ComparativeOrder)
topicCalculatedField_comparativeOrder = Lens.lens (\TopicCalculatedField' {comparativeOrder} -> comparativeOrder) (\s@TopicCalculatedField' {} a -> s {comparativeOrder = a} :: TopicCalculatedField)

-- | The default formatting definition.
topicCalculatedField_defaultFormatting :: Lens.Lens' TopicCalculatedField (Prelude.Maybe DefaultFormatting)
topicCalculatedField_defaultFormatting = Lens.lens (\TopicCalculatedField' {defaultFormatting} -> defaultFormatting) (\s@TopicCalculatedField' {} a -> s {defaultFormatting = a} :: TopicCalculatedField)

-- | A Boolean value that indicates if a calculated field is visible in the
-- autocomplete.
topicCalculatedField_disableIndexing :: Lens.Lens' TopicCalculatedField (Prelude.Maybe Prelude.Bool)
topicCalculatedField_disableIndexing = Lens.lens (\TopicCalculatedField' {disableIndexing} -> disableIndexing) (\s@TopicCalculatedField' {} a -> s {disableIndexing = a} :: TopicCalculatedField)

-- | A boolean value that indicates if a calculated field is included in the
-- topic.
topicCalculatedField_isIncludedInTopic :: Lens.Lens' TopicCalculatedField (Prelude.Maybe Prelude.Bool)
topicCalculatedField_isIncludedInTopic = Lens.lens (\TopicCalculatedField' {isIncludedInTopic} -> isIncludedInTopic) (\s@TopicCalculatedField' {} a -> s {isIncludedInTopic = a} :: TopicCalculatedField)

-- | A Boolean value that indicates whether to never aggregate calculated
-- field in filters.
topicCalculatedField_neverAggregateInFilter :: Lens.Lens' TopicCalculatedField (Prelude.Maybe Prelude.Bool)
topicCalculatedField_neverAggregateInFilter = Lens.lens (\TopicCalculatedField' {neverAggregateInFilter} -> neverAggregateInFilter) (\s@TopicCalculatedField' {} a -> s {neverAggregateInFilter = a} :: TopicCalculatedField)

-- | The list of aggregation types that are not allowed for the calculated
-- field. Valid values for this structure are @COUNT@, @DISTINCT_COUNT@,
-- @MIN@, @MAX@, @MEDIAN@, @SUM@, @AVERAGE@, @STDEV@, @STDEVP@, @VAR@,
-- @VARP@, and @PERCENTILE@.
topicCalculatedField_notAllowedAggregations :: Lens.Lens' TopicCalculatedField (Prelude.Maybe [AuthorSpecifiedAggregation])
topicCalculatedField_notAllowedAggregations = Lens.lens (\TopicCalculatedField' {notAllowedAggregations} -> notAllowedAggregations) (\s@TopicCalculatedField' {} a -> s {notAllowedAggregations = a} :: TopicCalculatedField) Prelude.. Lens.mapping Lens.coerced

-- | The semantic type.
topicCalculatedField_semanticType :: Lens.Lens' TopicCalculatedField (Prelude.Maybe SemanticType)
topicCalculatedField_semanticType = Lens.lens (\TopicCalculatedField' {semanticType} -> semanticType) (\s@TopicCalculatedField' {} a -> s {semanticType = a} :: TopicCalculatedField)

-- | The level of time precision that is used to aggregate @DateTime@ values.
topicCalculatedField_timeGranularity :: Lens.Lens' TopicCalculatedField (Prelude.Maybe TopicTimeGranularity)
topicCalculatedField_timeGranularity = Lens.lens (\TopicCalculatedField' {timeGranularity} -> timeGranularity) (\s@TopicCalculatedField' {} a -> s {timeGranularity = a} :: TopicCalculatedField)

-- | The calculated field name.
topicCalculatedField_calculatedFieldName :: Lens.Lens' TopicCalculatedField Prelude.Text
topicCalculatedField_calculatedFieldName = Lens.lens (\TopicCalculatedField' {calculatedFieldName} -> calculatedFieldName) (\s@TopicCalculatedField' {} a -> s {calculatedFieldName = a} :: TopicCalculatedField)

-- | The calculated field expression.
topicCalculatedField_expression :: Lens.Lens' TopicCalculatedField Prelude.Text
topicCalculatedField_expression = Lens.lens (\TopicCalculatedField' {expression} -> expression) (\s@TopicCalculatedField' {} a -> s {expression = a} :: TopicCalculatedField) Prelude.. Data._Sensitive

instance Data.FromJSON TopicCalculatedField where
  parseJSON =
    Data.withObject
      "TopicCalculatedField"
      ( \x ->
          TopicCalculatedField'
            Prelude.<$> (x Data..:? "Aggregation")
            Prelude.<*> ( x
                            Data..:? "AllowedAggregations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CalculatedFieldDescription")
            Prelude.<*> ( x
                            Data..:? "CalculatedFieldSynonyms"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "CellValueSynonyms"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ColumnDataRole")
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
            Prelude.<*> (x Data..: "CalculatedFieldName")
            Prelude.<*> (x Data..: "Expression")
      )

instance Prelude.Hashable TopicCalculatedField where
  hashWithSalt _salt TopicCalculatedField' {..} =
    _salt
      `Prelude.hashWithSalt` aggregation
      `Prelude.hashWithSalt` allowedAggregations
      `Prelude.hashWithSalt` calculatedFieldDescription
      `Prelude.hashWithSalt` calculatedFieldSynonyms
      `Prelude.hashWithSalt` cellValueSynonyms
      `Prelude.hashWithSalt` columnDataRole
      `Prelude.hashWithSalt` comparativeOrder
      `Prelude.hashWithSalt` defaultFormatting
      `Prelude.hashWithSalt` disableIndexing
      `Prelude.hashWithSalt` isIncludedInTopic
      `Prelude.hashWithSalt` neverAggregateInFilter
      `Prelude.hashWithSalt` notAllowedAggregations
      `Prelude.hashWithSalt` semanticType
      `Prelude.hashWithSalt` timeGranularity
      `Prelude.hashWithSalt` calculatedFieldName
      `Prelude.hashWithSalt` expression

instance Prelude.NFData TopicCalculatedField where
  rnf TopicCalculatedField' {..} =
    Prelude.rnf aggregation
      `Prelude.seq` Prelude.rnf allowedAggregations
      `Prelude.seq` Prelude.rnf calculatedFieldDescription
      `Prelude.seq` Prelude.rnf calculatedFieldSynonyms
      `Prelude.seq` Prelude.rnf cellValueSynonyms
      `Prelude.seq` Prelude.rnf columnDataRole
      `Prelude.seq` Prelude.rnf comparativeOrder
      `Prelude.seq` Prelude.rnf defaultFormatting
      `Prelude.seq` Prelude.rnf disableIndexing
      `Prelude.seq` Prelude.rnf isIncludedInTopic
      `Prelude.seq` Prelude.rnf neverAggregateInFilter
      `Prelude.seq` Prelude.rnf notAllowedAggregations
      `Prelude.seq` Prelude.rnf semanticType
      `Prelude.seq` Prelude.rnf timeGranularity
      `Prelude.seq` Prelude.rnf calculatedFieldName
      `Prelude.seq` Prelude.rnf expression

instance Data.ToJSON TopicCalculatedField where
  toJSON TopicCalculatedField' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Aggregation" Data..=) Prelude.<$> aggregation,
            ("AllowedAggregations" Data..=)
              Prelude.<$> allowedAggregations,
            ("CalculatedFieldDescription" Data..=)
              Prelude.<$> calculatedFieldDescription,
            ("CalculatedFieldSynonyms" Data..=)
              Prelude.<$> calculatedFieldSynonyms,
            ("CellValueSynonyms" Data..=)
              Prelude.<$> cellValueSynonyms,
            ("ColumnDataRole" Data..=)
              Prelude.<$> columnDataRole,
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
            Prelude.Just
              ("CalculatedFieldName" Data..= calculatedFieldName),
            Prelude.Just ("Expression" Data..= expression)
          ]
      )
