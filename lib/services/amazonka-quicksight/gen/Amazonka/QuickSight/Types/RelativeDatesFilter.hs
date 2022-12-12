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
-- Module      : Amazonka.QuickSight.Types.RelativeDatesFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RelativeDatesFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AnchorDateConfiguration
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.ExcludePeriodConfiguration
import Amazonka.QuickSight.Types.FilterNullOption
import Amazonka.QuickSight.Types.RelativeDateType
import Amazonka.QuickSight.Types.TimeGranularity

-- | A @RelativeDatesFilter@ filters relative dates values.
--
-- /See:/ 'newRelativeDatesFilter' smart constructor.
data RelativeDatesFilter = RelativeDatesFilter'
  { -- | The configuration for the exclude period of the filter.
    excludePeriodConfiguration :: Prelude.Maybe ExcludePeriodConfiguration,
    -- | The minimum granularity (period granularity) of the relative dates
    -- filter.
    minimumGranularity :: Prelude.Maybe TimeGranularity,
    -- | The parameter whose value should be used for the filter value.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | The date value of the filter.
    relativeDateValue :: Prelude.Maybe Prelude.Int,
    -- | An identifier that uniquely identifies a filter within a dashboard,
    -- analysis, or template.
    filterId :: Prelude.Text,
    -- | The column that the filter is applied to.
    column :: ColumnIdentifier,
    -- | The date configuration of the filter.
    anchorDateConfiguration :: AnchorDateConfiguration,
    -- | The level of time precision that is used to aggregate @DateTime@ values.
    timeGranularity :: TimeGranularity,
    -- | The range date type of the filter. Choose one of the options below:
    --
    -- -   @PREVIOUS@
    --
    -- -   @THIS@
    --
    -- -   @LAST@
    --
    -- -   @NOW@
    --
    -- -   @NEXT@
    relativeDateType :: RelativeDateType,
    -- | This option determines how null values should be treated when filtering
    -- data.
    --
    -- -   @ALL_VALUES@: Include null values in filtered results.
    --
    -- -   @NULLS_ONLY@: Only include null values in filtered results.
    --
    -- -   @NON_NULLS_ONLY@: Exclude null values from filtered results.
    nullOption :: FilterNullOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelativeDatesFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludePeriodConfiguration', 'relativeDatesFilter_excludePeriodConfiguration' - The configuration for the exclude period of the filter.
--
-- 'minimumGranularity', 'relativeDatesFilter_minimumGranularity' - The minimum granularity (period granularity) of the relative dates
-- filter.
--
-- 'parameterName', 'relativeDatesFilter_parameterName' - The parameter whose value should be used for the filter value.
--
-- 'relativeDateValue', 'relativeDatesFilter_relativeDateValue' - The date value of the filter.
--
-- 'filterId', 'relativeDatesFilter_filterId' - An identifier that uniquely identifies a filter within a dashboard,
-- analysis, or template.
--
-- 'column', 'relativeDatesFilter_column' - The column that the filter is applied to.
--
-- 'anchorDateConfiguration', 'relativeDatesFilter_anchorDateConfiguration' - The date configuration of the filter.
--
-- 'timeGranularity', 'relativeDatesFilter_timeGranularity' - The level of time precision that is used to aggregate @DateTime@ values.
--
-- 'relativeDateType', 'relativeDatesFilter_relativeDateType' - The range date type of the filter. Choose one of the options below:
--
-- -   @PREVIOUS@
--
-- -   @THIS@
--
-- -   @LAST@
--
-- -   @NOW@
--
-- -   @NEXT@
--
-- 'nullOption', 'relativeDatesFilter_nullOption' - This option determines how null values should be treated when filtering
-- data.
--
-- -   @ALL_VALUES@: Include null values in filtered results.
--
-- -   @NULLS_ONLY@: Only include null values in filtered results.
--
-- -   @NON_NULLS_ONLY@: Exclude null values from filtered results.
newRelativeDatesFilter ::
  -- | 'filterId'
  Prelude.Text ->
  -- | 'column'
  ColumnIdentifier ->
  -- | 'anchorDateConfiguration'
  AnchorDateConfiguration ->
  -- | 'timeGranularity'
  TimeGranularity ->
  -- | 'relativeDateType'
  RelativeDateType ->
  -- | 'nullOption'
  FilterNullOption ->
  RelativeDatesFilter
newRelativeDatesFilter
  pFilterId_
  pColumn_
  pAnchorDateConfiguration_
  pTimeGranularity_
  pRelativeDateType_
  pNullOption_ =
    RelativeDatesFilter'
      { excludePeriodConfiguration =
          Prelude.Nothing,
        minimumGranularity = Prelude.Nothing,
        parameterName = Prelude.Nothing,
        relativeDateValue = Prelude.Nothing,
        filterId = pFilterId_,
        column = pColumn_,
        anchorDateConfiguration = pAnchorDateConfiguration_,
        timeGranularity = pTimeGranularity_,
        relativeDateType = pRelativeDateType_,
        nullOption = pNullOption_
      }

-- | The configuration for the exclude period of the filter.
relativeDatesFilter_excludePeriodConfiguration :: Lens.Lens' RelativeDatesFilter (Prelude.Maybe ExcludePeriodConfiguration)
relativeDatesFilter_excludePeriodConfiguration = Lens.lens (\RelativeDatesFilter' {excludePeriodConfiguration} -> excludePeriodConfiguration) (\s@RelativeDatesFilter' {} a -> s {excludePeriodConfiguration = a} :: RelativeDatesFilter)

-- | The minimum granularity (period granularity) of the relative dates
-- filter.
relativeDatesFilter_minimumGranularity :: Lens.Lens' RelativeDatesFilter (Prelude.Maybe TimeGranularity)
relativeDatesFilter_minimumGranularity = Lens.lens (\RelativeDatesFilter' {minimumGranularity} -> minimumGranularity) (\s@RelativeDatesFilter' {} a -> s {minimumGranularity = a} :: RelativeDatesFilter)

-- | The parameter whose value should be used for the filter value.
relativeDatesFilter_parameterName :: Lens.Lens' RelativeDatesFilter (Prelude.Maybe Prelude.Text)
relativeDatesFilter_parameterName = Lens.lens (\RelativeDatesFilter' {parameterName} -> parameterName) (\s@RelativeDatesFilter' {} a -> s {parameterName = a} :: RelativeDatesFilter)

-- | The date value of the filter.
relativeDatesFilter_relativeDateValue :: Lens.Lens' RelativeDatesFilter (Prelude.Maybe Prelude.Int)
relativeDatesFilter_relativeDateValue = Lens.lens (\RelativeDatesFilter' {relativeDateValue} -> relativeDateValue) (\s@RelativeDatesFilter' {} a -> s {relativeDateValue = a} :: RelativeDatesFilter)

-- | An identifier that uniquely identifies a filter within a dashboard,
-- analysis, or template.
relativeDatesFilter_filterId :: Lens.Lens' RelativeDatesFilter Prelude.Text
relativeDatesFilter_filterId = Lens.lens (\RelativeDatesFilter' {filterId} -> filterId) (\s@RelativeDatesFilter' {} a -> s {filterId = a} :: RelativeDatesFilter)

-- | The column that the filter is applied to.
relativeDatesFilter_column :: Lens.Lens' RelativeDatesFilter ColumnIdentifier
relativeDatesFilter_column = Lens.lens (\RelativeDatesFilter' {column} -> column) (\s@RelativeDatesFilter' {} a -> s {column = a} :: RelativeDatesFilter)

-- | The date configuration of the filter.
relativeDatesFilter_anchorDateConfiguration :: Lens.Lens' RelativeDatesFilter AnchorDateConfiguration
relativeDatesFilter_anchorDateConfiguration = Lens.lens (\RelativeDatesFilter' {anchorDateConfiguration} -> anchorDateConfiguration) (\s@RelativeDatesFilter' {} a -> s {anchorDateConfiguration = a} :: RelativeDatesFilter)

-- | The level of time precision that is used to aggregate @DateTime@ values.
relativeDatesFilter_timeGranularity :: Lens.Lens' RelativeDatesFilter TimeGranularity
relativeDatesFilter_timeGranularity = Lens.lens (\RelativeDatesFilter' {timeGranularity} -> timeGranularity) (\s@RelativeDatesFilter' {} a -> s {timeGranularity = a} :: RelativeDatesFilter)

-- | The range date type of the filter. Choose one of the options below:
--
-- -   @PREVIOUS@
--
-- -   @THIS@
--
-- -   @LAST@
--
-- -   @NOW@
--
-- -   @NEXT@
relativeDatesFilter_relativeDateType :: Lens.Lens' RelativeDatesFilter RelativeDateType
relativeDatesFilter_relativeDateType = Lens.lens (\RelativeDatesFilter' {relativeDateType} -> relativeDateType) (\s@RelativeDatesFilter' {} a -> s {relativeDateType = a} :: RelativeDatesFilter)

-- | This option determines how null values should be treated when filtering
-- data.
--
-- -   @ALL_VALUES@: Include null values in filtered results.
--
-- -   @NULLS_ONLY@: Only include null values in filtered results.
--
-- -   @NON_NULLS_ONLY@: Exclude null values from filtered results.
relativeDatesFilter_nullOption :: Lens.Lens' RelativeDatesFilter FilterNullOption
relativeDatesFilter_nullOption = Lens.lens (\RelativeDatesFilter' {nullOption} -> nullOption) (\s@RelativeDatesFilter' {} a -> s {nullOption = a} :: RelativeDatesFilter)

instance Data.FromJSON RelativeDatesFilter where
  parseJSON =
    Data.withObject
      "RelativeDatesFilter"
      ( \x ->
          RelativeDatesFilter'
            Prelude.<$> (x Data..:? "ExcludePeriodConfiguration")
            Prelude.<*> (x Data..:? "MinimumGranularity")
            Prelude.<*> (x Data..:? "ParameterName")
            Prelude.<*> (x Data..:? "RelativeDateValue")
            Prelude.<*> (x Data..: "FilterId")
            Prelude.<*> (x Data..: "Column")
            Prelude.<*> (x Data..: "AnchorDateConfiguration")
            Prelude.<*> (x Data..: "TimeGranularity")
            Prelude.<*> (x Data..: "RelativeDateType")
            Prelude.<*> (x Data..: "NullOption")
      )

instance Prelude.Hashable RelativeDatesFilter where
  hashWithSalt _salt RelativeDatesFilter' {..} =
    _salt
      `Prelude.hashWithSalt` excludePeriodConfiguration
      `Prelude.hashWithSalt` minimumGranularity
      `Prelude.hashWithSalt` parameterName
      `Prelude.hashWithSalt` relativeDateValue
      `Prelude.hashWithSalt` filterId
      `Prelude.hashWithSalt` column
      `Prelude.hashWithSalt` anchorDateConfiguration
      `Prelude.hashWithSalt` timeGranularity
      `Prelude.hashWithSalt` relativeDateType
      `Prelude.hashWithSalt` nullOption

instance Prelude.NFData RelativeDatesFilter where
  rnf RelativeDatesFilter' {..} =
    Prelude.rnf excludePeriodConfiguration
      `Prelude.seq` Prelude.rnf minimumGranularity
      `Prelude.seq` Prelude.rnf parameterName
      `Prelude.seq` Prelude.rnf relativeDateValue
      `Prelude.seq` Prelude.rnf filterId
      `Prelude.seq` Prelude.rnf column
      `Prelude.seq` Prelude.rnf anchorDateConfiguration
      `Prelude.seq` Prelude.rnf timeGranularity
      `Prelude.seq` Prelude.rnf relativeDateType
      `Prelude.seq` Prelude.rnf nullOption

instance Data.ToJSON RelativeDatesFilter where
  toJSON RelativeDatesFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExcludePeriodConfiguration" Data..=)
              Prelude.<$> excludePeriodConfiguration,
            ("MinimumGranularity" Data..=)
              Prelude.<$> minimumGranularity,
            ("ParameterName" Data..=) Prelude.<$> parameterName,
            ("RelativeDateValue" Data..=)
              Prelude.<$> relativeDateValue,
            Prelude.Just ("FilterId" Data..= filterId),
            Prelude.Just ("Column" Data..= column),
            Prelude.Just
              ( "AnchorDateConfiguration"
                  Data..= anchorDateConfiguration
              ),
            Prelude.Just
              ("TimeGranularity" Data..= timeGranularity),
            Prelude.Just
              ("RelativeDateType" Data..= relativeDateType),
            Prelude.Just ("NullOption" Data..= nullOption)
          ]
      )
