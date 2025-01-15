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
-- Module      : Amazonka.QuickSight.Types.DashboardVersionDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardVersionDefinition where

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

-- | The contents of a dashboard.
--
-- /See:/ 'newDashboardVersionDefinition' smart constructor.
data DashboardVersionDefinition = DashboardVersionDefinition'
  { analysisDefaults :: Prelude.Maybe AnalysisDefaults,
    -- | An array of calculated field definitions for the dashboard.
    calculatedFields :: Prelude.Maybe [CalculatedField],
    -- | An array of dashboard-level column configurations. Column configurations
    -- are used to set the default formatting for a column that is used
    -- throughout a dashboard.
    columnConfigurations :: Prelude.Maybe [ColumnConfiguration],
    -- | The filter definitions for a dashboard.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/adding-a-filter.html Filtering Data in Amazon QuickSight>
    -- in the /Amazon QuickSight User Guide/.
    filterGroups :: Prelude.Maybe [FilterGroup],
    -- | The parameter declarations for a dashboard. Parameters are named
    -- variables that can transfer a value for use by an action or an object.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/parameters-in-quicksight.html Parameters in Amazon QuickSight>
    -- in the /Amazon QuickSight User Guide/.
    parameterDeclarations :: Prelude.Maybe [ParameterDeclaration],
    -- | An array of sheet definitions for a dashboard.
    sheets :: Prelude.Maybe [SheetDefinition],
    -- | An array of dataset identifier declarations. With this mapping,you can
    -- use dataset identifiers instead of dataset Amazon Resource Names (ARNs)
    -- throughout the dashboard\'s sub-structures.
    dataSetIdentifierDeclarations :: Prelude.NonEmpty DataSetIdentifierDeclaration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashboardVersionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisDefaults', 'dashboardVersionDefinition_analysisDefaults' - Undocumented member.
--
-- 'calculatedFields', 'dashboardVersionDefinition_calculatedFields' - An array of calculated field definitions for the dashboard.
--
-- 'columnConfigurations', 'dashboardVersionDefinition_columnConfigurations' - An array of dashboard-level column configurations. Column configurations
-- are used to set the default formatting for a column that is used
-- throughout a dashboard.
--
-- 'filterGroups', 'dashboardVersionDefinition_filterGroups' - The filter definitions for a dashboard.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/adding-a-filter.html Filtering Data in Amazon QuickSight>
-- in the /Amazon QuickSight User Guide/.
--
-- 'parameterDeclarations', 'dashboardVersionDefinition_parameterDeclarations' - The parameter declarations for a dashboard. Parameters are named
-- variables that can transfer a value for use by an action or an object.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/parameters-in-quicksight.html Parameters in Amazon QuickSight>
-- in the /Amazon QuickSight User Guide/.
--
-- 'sheets', 'dashboardVersionDefinition_sheets' - An array of sheet definitions for a dashboard.
--
-- 'dataSetIdentifierDeclarations', 'dashboardVersionDefinition_dataSetIdentifierDeclarations' - An array of dataset identifier declarations. With this mapping,you can
-- use dataset identifiers instead of dataset Amazon Resource Names (ARNs)
-- throughout the dashboard\'s sub-structures.
newDashboardVersionDefinition ::
  -- | 'dataSetIdentifierDeclarations'
  Prelude.NonEmpty DataSetIdentifierDeclaration ->
  DashboardVersionDefinition
newDashboardVersionDefinition
  pDataSetIdentifierDeclarations_ =
    DashboardVersionDefinition'
      { analysisDefaults =
          Prelude.Nothing,
        calculatedFields = Prelude.Nothing,
        columnConfigurations = Prelude.Nothing,
        filterGroups = Prelude.Nothing,
        parameterDeclarations = Prelude.Nothing,
        sheets = Prelude.Nothing,
        dataSetIdentifierDeclarations =
          Lens.coerced
            Lens.# pDataSetIdentifierDeclarations_
      }

-- | Undocumented member.
dashboardVersionDefinition_analysisDefaults :: Lens.Lens' DashboardVersionDefinition (Prelude.Maybe AnalysisDefaults)
dashboardVersionDefinition_analysisDefaults = Lens.lens (\DashboardVersionDefinition' {analysisDefaults} -> analysisDefaults) (\s@DashboardVersionDefinition' {} a -> s {analysisDefaults = a} :: DashboardVersionDefinition)

-- | An array of calculated field definitions for the dashboard.
dashboardVersionDefinition_calculatedFields :: Lens.Lens' DashboardVersionDefinition (Prelude.Maybe [CalculatedField])
dashboardVersionDefinition_calculatedFields = Lens.lens (\DashboardVersionDefinition' {calculatedFields} -> calculatedFields) (\s@DashboardVersionDefinition' {} a -> s {calculatedFields = a} :: DashboardVersionDefinition) Prelude.. Lens.mapping Lens.coerced

-- | An array of dashboard-level column configurations. Column configurations
-- are used to set the default formatting for a column that is used
-- throughout a dashboard.
dashboardVersionDefinition_columnConfigurations :: Lens.Lens' DashboardVersionDefinition (Prelude.Maybe [ColumnConfiguration])
dashboardVersionDefinition_columnConfigurations = Lens.lens (\DashboardVersionDefinition' {columnConfigurations} -> columnConfigurations) (\s@DashboardVersionDefinition' {} a -> s {columnConfigurations = a} :: DashboardVersionDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The filter definitions for a dashboard.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/adding-a-filter.html Filtering Data in Amazon QuickSight>
-- in the /Amazon QuickSight User Guide/.
dashboardVersionDefinition_filterGroups :: Lens.Lens' DashboardVersionDefinition (Prelude.Maybe [FilterGroup])
dashboardVersionDefinition_filterGroups = Lens.lens (\DashboardVersionDefinition' {filterGroups} -> filterGroups) (\s@DashboardVersionDefinition' {} a -> s {filterGroups = a} :: DashboardVersionDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The parameter declarations for a dashboard. Parameters are named
-- variables that can transfer a value for use by an action or an object.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/parameters-in-quicksight.html Parameters in Amazon QuickSight>
-- in the /Amazon QuickSight User Guide/.
dashboardVersionDefinition_parameterDeclarations :: Lens.Lens' DashboardVersionDefinition (Prelude.Maybe [ParameterDeclaration])
dashboardVersionDefinition_parameterDeclarations = Lens.lens (\DashboardVersionDefinition' {parameterDeclarations} -> parameterDeclarations) (\s@DashboardVersionDefinition' {} a -> s {parameterDeclarations = a} :: DashboardVersionDefinition) Prelude.. Lens.mapping Lens.coerced

-- | An array of sheet definitions for a dashboard.
dashboardVersionDefinition_sheets :: Lens.Lens' DashboardVersionDefinition (Prelude.Maybe [SheetDefinition])
dashboardVersionDefinition_sheets = Lens.lens (\DashboardVersionDefinition' {sheets} -> sheets) (\s@DashboardVersionDefinition' {} a -> s {sheets = a} :: DashboardVersionDefinition) Prelude.. Lens.mapping Lens.coerced

-- | An array of dataset identifier declarations. With this mapping,you can
-- use dataset identifiers instead of dataset Amazon Resource Names (ARNs)
-- throughout the dashboard\'s sub-structures.
dashboardVersionDefinition_dataSetIdentifierDeclarations :: Lens.Lens' DashboardVersionDefinition (Prelude.NonEmpty DataSetIdentifierDeclaration)
dashboardVersionDefinition_dataSetIdentifierDeclarations = Lens.lens (\DashboardVersionDefinition' {dataSetIdentifierDeclarations} -> dataSetIdentifierDeclarations) (\s@DashboardVersionDefinition' {} a -> s {dataSetIdentifierDeclarations = a} :: DashboardVersionDefinition) Prelude.. Lens.coerced

instance Data.FromJSON DashboardVersionDefinition where
  parseJSON =
    Data.withObject
      "DashboardVersionDefinition"
      ( \x ->
          DashboardVersionDefinition'
            Prelude.<$> (x Data..:? "AnalysisDefaults")
            Prelude.<*> ( x
                            Data..:? "CalculatedFields"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "ColumnConfigurations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "FilterGroups" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "ParameterDeclarations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Sheets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "DataSetIdentifierDeclarations")
      )

instance Prelude.Hashable DashboardVersionDefinition where
  hashWithSalt _salt DashboardVersionDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` analysisDefaults
      `Prelude.hashWithSalt` calculatedFields
      `Prelude.hashWithSalt` columnConfigurations
      `Prelude.hashWithSalt` filterGroups
      `Prelude.hashWithSalt` parameterDeclarations
      `Prelude.hashWithSalt` sheets
      `Prelude.hashWithSalt` dataSetIdentifierDeclarations

instance Prelude.NFData DashboardVersionDefinition where
  rnf DashboardVersionDefinition' {..} =
    Prelude.rnf analysisDefaults `Prelude.seq`
      Prelude.rnf calculatedFields `Prelude.seq`
        Prelude.rnf columnConfigurations `Prelude.seq`
          Prelude.rnf filterGroups `Prelude.seq`
            Prelude.rnf parameterDeclarations `Prelude.seq`
              Prelude.rnf sheets `Prelude.seq`
                Prelude.rnf dataSetIdentifierDeclarations

instance Data.ToJSON DashboardVersionDefinition where
  toJSON DashboardVersionDefinition' {..} =
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
