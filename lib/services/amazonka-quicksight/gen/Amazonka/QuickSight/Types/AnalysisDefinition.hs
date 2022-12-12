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
-- Module      : Amazonka.QuickSight.Types.AnalysisDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AnalysisDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AnalysisDefaults
import Amazonka.QuickSight.Types.CalculatedField
import Amazonka.QuickSight.Types.ColumnConfiguration
import Amazonka.QuickSight.Types.DataSetIdentifierDeclaration
import Amazonka.QuickSight.Types.FilterGroup
import Amazonka.QuickSight.Types.ParameterDeclaration
import Amazonka.QuickSight.Types.SheetDefinition

-- | The definition of an analysis.
--
-- /See:/ 'newAnalysisDefinition' smart constructor.
data AnalysisDefinition = AnalysisDefinition'
  { analysisDefaults :: Prelude.Maybe AnalysisDefaults,
    -- | An array of calculated field definitions for the analysis.
    calculatedFields :: Prelude.Maybe [CalculatedField],
    -- | An array of analysis-level column configurations. Column configurations
    -- can be used to set default formatting for a column to be used throughout
    -- an analysis.
    columnConfigurations :: Prelude.Maybe [ColumnConfiguration],
    -- | Filter definitions for an analysis.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/adding-a-filter.html Filtering Data in Amazon QuickSight>
    -- in the /Amazon QuickSight User Guide/.
    filterGroups :: Prelude.Maybe [FilterGroup],
    -- | An array of parameter declarations for an analysis.
    --
    -- Parameters are named variables that can transfer a value for use by an
    -- action or an object.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/parameters-in-quicksight.html Parameters in Amazon QuickSight>
    -- in the /Amazon QuickSight User Guide/.
    parameterDeclarations :: Prelude.Maybe [ParameterDeclaration],
    -- | An array of sheet definitions for an analysis. Each @SheetDefinition@
    -- provides detailed information about a sheet within this analysis.
    sheets :: Prelude.Maybe [SheetDefinition],
    -- | An array of dataset identifier declarations. This mapping allows the
    -- usage of dataset identifiers instead of dataset ARNs throughout analysis
    -- sub-structures.
    dataSetIdentifierDeclarations :: Prelude.NonEmpty DataSetIdentifierDeclaration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisDefaults', 'analysisDefinition_analysisDefaults' - Undocumented member.
--
-- 'calculatedFields', 'analysisDefinition_calculatedFields' - An array of calculated field definitions for the analysis.
--
-- 'columnConfigurations', 'analysisDefinition_columnConfigurations' - An array of analysis-level column configurations. Column configurations
-- can be used to set default formatting for a column to be used throughout
-- an analysis.
--
-- 'filterGroups', 'analysisDefinition_filterGroups' - Filter definitions for an analysis.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/adding-a-filter.html Filtering Data in Amazon QuickSight>
-- in the /Amazon QuickSight User Guide/.
--
-- 'parameterDeclarations', 'analysisDefinition_parameterDeclarations' - An array of parameter declarations for an analysis.
--
-- Parameters are named variables that can transfer a value for use by an
-- action or an object.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/parameters-in-quicksight.html Parameters in Amazon QuickSight>
-- in the /Amazon QuickSight User Guide/.
--
-- 'sheets', 'analysisDefinition_sheets' - An array of sheet definitions for an analysis. Each @SheetDefinition@
-- provides detailed information about a sheet within this analysis.
--
-- 'dataSetIdentifierDeclarations', 'analysisDefinition_dataSetIdentifierDeclarations' - An array of dataset identifier declarations. This mapping allows the
-- usage of dataset identifiers instead of dataset ARNs throughout analysis
-- sub-structures.
newAnalysisDefinition ::
  -- | 'dataSetIdentifierDeclarations'
  Prelude.NonEmpty DataSetIdentifierDeclaration ->
  AnalysisDefinition
newAnalysisDefinition pDataSetIdentifierDeclarations_ =
  AnalysisDefinition'
    { analysisDefaults =
        Prelude.Nothing,
      calculatedFields = Prelude.Nothing,
      columnConfigurations = Prelude.Nothing,
      filterGroups = Prelude.Nothing,
      parameterDeclarations = Prelude.Nothing,
      sheets = Prelude.Nothing,
      dataSetIdentifierDeclarations =
        Lens.coerced Lens.# pDataSetIdentifierDeclarations_
    }

-- | Undocumented member.
analysisDefinition_analysisDefaults :: Lens.Lens' AnalysisDefinition (Prelude.Maybe AnalysisDefaults)
analysisDefinition_analysisDefaults = Lens.lens (\AnalysisDefinition' {analysisDefaults} -> analysisDefaults) (\s@AnalysisDefinition' {} a -> s {analysisDefaults = a} :: AnalysisDefinition)

-- | An array of calculated field definitions for the analysis.
analysisDefinition_calculatedFields :: Lens.Lens' AnalysisDefinition (Prelude.Maybe [CalculatedField])
analysisDefinition_calculatedFields = Lens.lens (\AnalysisDefinition' {calculatedFields} -> calculatedFields) (\s@AnalysisDefinition' {} a -> s {calculatedFields = a} :: AnalysisDefinition) Prelude.. Lens.mapping Lens.coerced

-- | An array of analysis-level column configurations. Column configurations
-- can be used to set default formatting for a column to be used throughout
-- an analysis.
analysisDefinition_columnConfigurations :: Lens.Lens' AnalysisDefinition (Prelude.Maybe [ColumnConfiguration])
analysisDefinition_columnConfigurations = Lens.lens (\AnalysisDefinition' {columnConfigurations} -> columnConfigurations) (\s@AnalysisDefinition' {} a -> s {columnConfigurations = a} :: AnalysisDefinition) Prelude.. Lens.mapping Lens.coerced

-- | Filter definitions for an analysis.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/adding-a-filter.html Filtering Data in Amazon QuickSight>
-- in the /Amazon QuickSight User Guide/.
analysisDefinition_filterGroups :: Lens.Lens' AnalysisDefinition (Prelude.Maybe [FilterGroup])
analysisDefinition_filterGroups = Lens.lens (\AnalysisDefinition' {filterGroups} -> filterGroups) (\s@AnalysisDefinition' {} a -> s {filterGroups = a} :: AnalysisDefinition) Prelude.. Lens.mapping Lens.coerced

-- | An array of parameter declarations for an analysis.
--
-- Parameters are named variables that can transfer a value for use by an
-- action or an object.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/parameters-in-quicksight.html Parameters in Amazon QuickSight>
-- in the /Amazon QuickSight User Guide/.
analysisDefinition_parameterDeclarations :: Lens.Lens' AnalysisDefinition (Prelude.Maybe [ParameterDeclaration])
analysisDefinition_parameterDeclarations = Lens.lens (\AnalysisDefinition' {parameterDeclarations} -> parameterDeclarations) (\s@AnalysisDefinition' {} a -> s {parameterDeclarations = a} :: AnalysisDefinition) Prelude.. Lens.mapping Lens.coerced

-- | An array of sheet definitions for an analysis. Each @SheetDefinition@
-- provides detailed information about a sheet within this analysis.
analysisDefinition_sheets :: Lens.Lens' AnalysisDefinition (Prelude.Maybe [SheetDefinition])
analysisDefinition_sheets = Lens.lens (\AnalysisDefinition' {sheets} -> sheets) (\s@AnalysisDefinition' {} a -> s {sheets = a} :: AnalysisDefinition) Prelude.. Lens.mapping Lens.coerced

-- | An array of dataset identifier declarations. This mapping allows the
-- usage of dataset identifiers instead of dataset ARNs throughout analysis
-- sub-structures.
analysisDefinition_dataSetIdentifierDeclarations :: Lens.Lens' AnalysisDefinition (Prelude.NonEmpty DataSetIdentifierDeclaration)
analysisDefinition_dataSetIdentifierDeclarations = Lens.lens (\AnalysisDefinition' {dataSetIdentifierDeclarations} -> dataSetIdentifierDeclarations) (\s@AnalysisDefinition' {} a -> s {dataSetIdentifierDeclarations = a} :: AnalysisDefinition) Prelude.. Lens.coerced

instance Data.FromJSON AnalysisDefinition where
  parseJSON =
    Data.withObject
      "AnalysisDefinition"
      ( \x ->
          AnalysisDefinition'
            Prelude.<$> (x Data..:? "AnalysisDefaults")
            Prelude.<*> ( x Data..:? "CalculatedFields"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ColumnConfigurations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "FilterGroups" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "ParameterDeclarations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Sheets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "DataSetIdentifierDeclarations")
      )

instance Prelude.Hashable AnalysisDefinition where
  hashWithSalt _salt AnalysisDefinition' {..} =
    _salt `Prelude.hashWithSalt` analysisDefaults
      `Prelude.hashWithSalt` calculatedFields
      `Prelude.hashWithSalt` columnConfigurations
      `Prelude.hashWithSalt` filterGroups
      `Prelude.hashWithSalt` parameterDeclarations
      `Prelude.hashWithSalt` sheets
      `Prelude.hashWithSalt` dataSetIdentifierDeclarations

instance Prelude.NFData AnalysisDefinition where
  rnf AnalysisDefinition' {..} =
    Prelude.rnf analysisDefaults
      `Prelude.seq` Prelude.rnf calculatedFields
      `Prelude.seq` Prelude.rnf columnConfigurations
      `Prelude.seq` Prelude.rnf filterGroups
      `Prelude.seq` Prelude.rnf parameterDeclarations
      `Prelude.seq` Prelude.rnf sheets
      `Prelude.seq` Prelude.rnf dataSetIdentifierDeclarations

instance Data.ToJSON AnalysisDefinition where
  toJSON AnalysisDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnalysisDefaults" Data..=)
              Prelude.<$> analysisDefaults,
            ("CalculatedFields" Data..=)
              Prelude.<$> calculatedFields,
            ("ColumnConfigurations" Data..=)
              Prelude.<$> columnConfigurations,
            ("FilterGroups" Data..=) Prelude.<$> filterGroups,
            ("ParameterDeclarations" Data..=)
              Prelude.<$> parameterDeclarations,
            ("Sheets" Data..=) Prelude.<$> sheets,
            Prelude.Just
              ( "DataSetIdentifierDeclarations"
                  Data..= dataSetIdentifierDeclarations
              )
          ]
      )
