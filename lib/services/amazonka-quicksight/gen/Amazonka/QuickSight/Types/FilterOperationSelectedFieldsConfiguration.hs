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
-- Module      : Amazonka.QuickSight.Types.FilterOperationSelectedFieldsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterOperationSelectedFieldsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.SelectedFieldOptions

-- | The configuration of selected fields in
-- the@CustomActionFilterOperation@.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newFilterOperationSelectedFieldsConfiguration' smart constructor.
data FilterOperationSelectedFieldsConfiguration = FilterOperationSelectedFieldsConfiguration'
  { -- | The selected columns of a dataset.
    selectedColumns :: Prelude.Maybe [ColumnIdentifier],
    -- | A structure that contains the options that choose which fields are
    -- filtered in the @CustomActionFilterOperation@.
    --
    -- Valid values are defined as follows:
    --
    -- -   @ALL_FIELDS@: Applies the filter operation to all fields.
    selectedFieldOptions :: Prelude.Maybe SelectedFieldOptions,
    -- | Chooses the fields that are filtered in @CustomActionFilterOperation@.
    selectedFields :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterOperationSelectedFieldsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selectedColumns', 'filterOperationSelectedFieldsConfiguration_selectedColumns' - The selected columns of a dataset.
--
-- 'selectedFieldOptions', 'filterOperationSelectedFieldsConfiguration_selectedFieldOptions' - A structure that contains the options that choose which fields are
-- filtered in the @CustomActionFilterOperation@.
--
-- Valid values are defined as follows:
--
-- -   @ALL_FIELDS@: Applies the filter operation to all fields.
--
-- 'selectedFields', 'filterOperationSelectedFieldsConfiguration_selectedFields' - Chooses the fields that are filtered in @CustomActionFilterOperation@.
newFilterOperationSelectedFieldsConfiguration ::
  FilterOperationSelectedFieldsConfiguration
newFilterOperationSelectedFieldsConfiguration =
  FilterOperationSelectedFieldsConfiguration'
    { selectedColumns =
        Prelude.Nothing,
      selectedFieldOptions =
        Prelude.Nothing,
      selectedFields =
        Prelude.Nothing
    }

-- | The selected columns of a dataset.
filterOperationSelectedFieldsConfiguration_selectedColumns :: Lens.Lens' FilterOperationSelectedFieldsConfiguration (Prelude.Maybe [ColumnIdentifier])
filterOperationSelectedFieldsConfiguration_selectedColumns = Lens.lens (\FilterOperationSelectedFieldsConfiguration' {selectedColumns} -> selectedColumns) (\s@FilterOperationSelectedFieldsConfiguration' {} a -> s {selectedColumns = a} :: FilterOperationSelectedFieldsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A structure that contains the options that choose which fields are
-- filtered in the @CustomActionFilterOperation@.
--
-- Valid values are defined as follows:
--
-- -   @ALL_FIELDS@: Applies the filter operation to all fields.
filterOperationSelectedFieldsConfiguration_selectedFieldOptions :: Lens.Lens' FilterOperationSelectedFieldsConfiguration (Prelude.Maybe SelectedFieldOptions)
filterOperationSelectedFieldsConfiguration_selectedFieldOptions = Lens.lens (\FilterOperationSelectedFieldsConfiguration' {selectedFieldOptions} -> selectedFieldOptions) (\s@FilterOperationSelectedFieldsConfiguration' {} a -> s {selectedFieldOptions = a} :: FilterOperationSelectedFieldsConfiguration)

-- | Chooses the fields that are filtered in @CustomActionFilterOperation@.
filterOperationSelectedFieldsConfiguration_selectedFields :: Lens.Lens' FilterOperationSelectedFieldsConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
filterOperationSelectedFieldsConfiguration_selectedFields = Lens.lens (\FilterOperationSelectedFieldsConfiguration' {selectedFields} -> selectedFields) (\s@FilterOperationSelectedFieldsConfiguration' {} a -> s {selectedFields = a} :: FilterOperationSelectedFieldsConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    FilterOperationSelectedFieldsConfiguration
  where
  parseJSON =
    Data.withObject
      "FilterOperationSelectedFieldsConfiguration"
      ( \x ->
          FilterOperationSelectedFieldsConfiguration'
            Prelude.<$> ( x
                            Data..:? "SelectedColumns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SelectedFieldOptions")
            Prelude.<*> (x Data..:? "SelectedFields")
      )

instance
  Prelude.Hashable
    FilterOperationSelectedFieldsConfiguration
  where
  hashWithSalt
    _salt
    FilterOperationSelectedFieldsConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` selectedColumns
        `Prelude.hashWithSalt` selectedFieldOptions
        `Prelude.hashWithSalt` selectedFields

instance
  Prelude.NFData
    FilterOperationSelectedFieldsConfiguration
  where
  rnf FilterOperationSelectedFieldsConfiguration' {..} =
    Prelude.rnf selectedColumns
      `Prelude.seq` Prelude.rnf selectedFieldOptions
      `Prelude.seq` Prelude.rnf selectedFields

instance
  Data.ToJSON
    FilterOperationSelectedFieldsConfiguration
  where
  toJSON
    FilterOperationSelectedFieldsConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("SelectedColumns" Data..=)
                Prelude.<$> selectedColumns,
              ("SelectedFieldOptions" Data..=)
                Prelude.<$> selectedFieldOptions,
              ("SelectedFields" Data..=)
                Prelude.<$> selectedFields
            ]
        )
