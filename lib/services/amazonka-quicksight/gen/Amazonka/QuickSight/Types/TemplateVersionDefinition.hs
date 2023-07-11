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
-- Module      : Amazonka.QuickSight.Types.TemplateVersionDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TemplateVersionDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AnalysisDefaults
import Amazonka.QuickSight.Types.CalculatedField
import Amazonka.QuickSight.Types.ColumnConfiguration
import Amazonka.QuickSight.Types.DataSetConfiguration
import Amazonka.QuickSight.Types.FilterGroup
import Amazonka.QuickSight.Types.ParameterDeclaration
import Amazonka.QuickSight.Types.SheetDefinition

-- | The detailed definition of a template.
--
-- /See:/ 'newTemplateVersionDefinition' smart constructor.
data TemplateVersionDefinition = TemplateVersionDefinition'
  { analysisDefaults :: Prelude.Maybe AnalysisDefaults,
    -- | An array of calculated field definitions for the template.
    calculatedFields :: Prelude.Maybe [CalculatedField],
    -- | An array of template-level column configurations. Column configurations
    -- are used to set default formatting for a column that\'s used throughout
    -- a template.
    columnConfigurations :: Prelude.Maybe [ColumnConfiguration],
    -- | Filter definitions for a template.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/filtering-visual-data.html Filtering Data>
    -- in the /Amazon QuickSight User Guide/.
    filterGroups :: Prelude.Maybe [FilterGroup],
    -- | An array of parameter declarations for a template.
    --
    -- /Parameters/ are named variables that can transfer a value for use by an
    -- action or an object.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/parameters-in-quicksight.html Parameters in Amazon QuickSight>
    -- in the /Amazon QuickSight User Guide/.
    parameterDeclarations :: Prelude.Maybe [ParameterDeclaration],
    -- | An array of sheet definitions for a template.
    sheets :: Prelude.Maybe [SheetDefinition],
    -- | An array of dataset configurations. These configurations define the
    -- required columns for each dataset used within a template.
    dataSetConfigurations :: [DataSetConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateVersionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisDefaults', 'templateVersionDefinition_analysisDefaults' - Undocumented member.
--
-- 'calculatedFields', 'templateVersionDefinition_calculatedFields' - An array of calculated field definitions for the template.
--
-- 'columnConfigurations', 'templateVersionDefinition_columnConfigurations' - An array of template-level column configurations. Column configurations
-- are used to set default formatting for a column that\'s used throughout
-- a template.
--
-- 'filterGroups', 'templateVersionDefinition_filterGroups' - Filter definitions for a template.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/filtering-visual-data.html Filtering Data>
-- in the /Amazon QuickSight User Guide/.
--
-- 'parameterDeclarations', 'templateVersionDefinition_parameterDeclarations' - An array of parameter declarations for a template.
--
-- /Parameters/ are named variables that can transfer a value for use by an
-- action or an object.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/parameters-in-quicksight.html Parameters in Amazon QuickSight>
-- in the /Amazon QuickSight User Guide/.
--
-- 'sheets', 'templateVersionDefinition_sheets' - An array of sheet definitions for a template.
--
-- 'dataSetConfigurations', 'templateVersionDefinition_dataSetConfigurations' - An array of dataset configurations. These configurations define the
-- required columns for each dataset used within a template.
newTemplateVersionDefinition ::
  TemplateVersionDefinition
newTemplateVersionDefinition =
  TemplateVersionDefinition'
    { analysisDefaults =
        Prelude.Nothing,
      calculatedFields = Prelude.Nothing,
      columnConfigurations = Prelude.Nothing,
      filterGroups = Prelude.Nothing,
      parameterDeclarations = Prelude.Nothing,
      sheets = Prelude.Nothing,
      dataSetConfigurations = Prelude.mempty
    }

-- | Undocumented member.
templateVersionDefinition_analysisDefaults :: Lens.Lens' TemplateVersionDefinition (Prelude.Maybe AnalysisDefaults)
templateVersionDefinition_analysisDefaults = Lens.lens (\TemplateVersionDefinition' {analysisDefaults} -> analysisDefaults) (\s@TemplateVersionDefinition' {} a -> s {analysisDefaults = a} :: TemplateVersionDefinition)

-- | An array of calculated field definitions for the template.
templateVersionDefinition_calculatedFields :: Lens.Lens' TemplateVersionDefinition (Prelude.Maybe [CalculatedField])
templateVersionDefinition_calculatedFields = Lens.lens (\TemplateVersionDefinition' {calculatedFields} -> calculatedFields) (\s@TemplateVersionDefinition' {} a -> s {calculatedFields = a} :: TemplateVersionDefinition) Prelude.. Lens.mapping Lens.coerced

-- | An array of template-level column configurations. Column configurations
-- are used to set default formatting for a column that\'s used throughout
-- a template.
templateVersionDefinition_columnConfigurations :: Lens.Lens' TemplateVersionDefinition (Prelude.Maybe [ColumnConfiguration])
templateVersionDefinition_columnConfigurations = Lens.lens (\TemplateVersionDefinition' {columnConfigurations} -> columnConfigurations) (\s@TemplateVersionDefinition' {} a -> s {columnConfigurations = a} :: TemplateVersionDefinition) Prelude.. Lens.mapping Lens.coerced

-- | Filter definitions for a template.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/filtering-visual-data.html Filtering Data>
-- in the /Amazon QuickSight User Guide/.
templateVersionDefinition_filterGroups :: Lens.Lens' TemplateVersionDefinition (Prelude.Maybe [FilterGroup])
templateVersionDefinition_filterGroups = Lens.lens (\TemplateVersionDefinition' {filterGroups} -> filterGroups) (\s@TemplateVersionDefinition' {} a -> s {filterGroups = a} :: TemplateVersionDefinition) Prelude.. Lens.mapping Lens.coerced

-- | An array of parameter declarations for a template.
--
-- /Parameters/ are named variables that can transfer a value for use by an
-- action or an object.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/parameters-in-quicksight.html Parameters in Amazon QuickSight>
-- in the /Amazon QuickSight User Guide/.
templateVersionDefinition_parameterDeclarations :: Lens.Lens' TemplateVersionDefinition (Prelude.Maybe [ParameterDeclaration])
templateVersionDefinition_parameterDeclarations = Lens.lens (\TemplateVersionDefinition' {parameterDeclarations} -> parameterDeclarations) (\s@TemplateVersionDefinition' {} a -> s {parameterDeclarations = a} :: TemplateVersionDefinition) Prelude.. Lens.mapping Lens.coerced

-- | An array of sheet definitions for a template.
templateVersionDefinition_sheets :: Lens.Lens' TemplateVersionDefinition (Prelude.Maybe [SheetDefinition])
templateVersionDefinition_sheets = Lens.lens (\TemplateVersionDefinition' {sheets} -> sheets) (\s@TemplateVersionDefinition' {} a -> s {sheets = a} :: TemplateVersionDefinition) Prelude.. Lens.mapping Lens.coerced

-- | An array of dataset configurations. These configurations define the
-- required columns for each dataset used within a template.
templateVersionDefinition_dataSetConfigurations :: Lens.Lens' TemplateVersionDefinition [DataSetConfiguration]
templateVersionDefinition_dataSetConfigurations = Lens.lens (\TemplateVersionDefinition' {dataSetConfigurations} -> dataSetConfigurations) (\s@TemplateVersionDefinition' {} a -> s {dataSetConfigurations = a} :: TemplateVersionDefinition) Prelude.. Lens.coerced

instance Data.FromJSON TemplateVersionDefinition where
  parseJSON =
    Data.withObject
      "TemplateVersionDefinition"
      ( \x ->
          TemplateVersionDefinition'
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
            Prelude.<*> ( x
                            Data..:? "DataSetConfigurations"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TemplateVersionDefinition where
  hashWithSalt _salt TemplateVersionDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` analysisDefaults
      `Prelude.hashWithSalt` calculatedFields
      `Prelude.hashWithSalt` columnConfigurations
      `Prelude.hashWithSalt` filterGroups
      `Prelude.hashWithSalt` parameterDeclarations
      `Prelude.hashWithSalt` sheets
      `Prelude.hashWithSalt` dataSetConfigurations

instance Prelude.NFData TemplateVersionDefinition where
  rnf TemplateVersionDefinition' {..} =
    Prelude.rnf analysisDefaults
      `Prelude.seq` Prelude.rnf calculatedFields
      `Prelude.seq` Prelude.rnf columnConfigurations
      `Prelude.seq` Prelude.rnf filterGroups
      `Prelude.seq` Prelude.rnf parameterDeclarations
      `Prelude.seq` Prelude.rnf sheets
      `Prelude.seq` Prelude.rnf dataSetConfigurations

instance Data.ToJSON TemplateVersionDefinition where
  toJSON TemplateVersionDefinition' {..} =
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
              ( "DataSetConfigurations"
                  Data..= dataSetConfigurations
              )
          ]
      )
