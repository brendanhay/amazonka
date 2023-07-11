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
-- Module      : Amazonka.QuickSight.Types.FilterGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CrossDatasetTypes
import Amazonka.QuickSight.Types.Filter
import Amazonka.QuickSight.Types.FilterScopeConfiguration
import Amazonka.QuickSight.Types.WidgetStatus

-- | A grouping of individual filters. Filter groups are applied to the same
-- group of visuals.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/add-a-compound-filter.html Adding filter conditions (group filters) with AND and OR operators>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newFilterGroup' smart constructor.
data FilterGroup = FilterGroup'
  { -- | The status of the @FilterGroup@.
    status :: Prelude.Maybe WidgetStatus,
    -- | The value that uniquely identifies a @FilterGroup@ within a dashboard,
    -- template, or analysis.
    filterGroupId :: Prelude.Text,
    -- | The list of filters that are present in a @FilterGroup@.
    filters :: [Filter],
    -- | The configuration that specifies what scope to apply to a @FilterGroup@.
    --
    -- This is a union type structure. For this structure to be valid, only one
    -- of the attributes can be defined.
    scopeConfiguration :: FilterScopeConfiguration,
    -- | The filter new feature which can apply filter group to all data sets.
    -- Choose one of the following options:
    --
    -- -   @ALL_DATASETS@
    --
    -- -   @SINGLE_DATASET@
    crossDataset :: CrossDatasetTypes
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'filterGroup_status' - The status of the @FilterGroup@.
--
-- 'filterGroupId', 'filterGroup_filterGroupId' - The value that uniquely identifies a @FilterGroup@ within a dashboard,
-- template, or analysis.
--
-- 'filters', 'filterGroup_filters' - The list of filters that are present in a @FilterGroup@.
--
-- 'scopeConfiguration', 'filterGroup_scopeConfiguration' - The configuration that specifies what scope to apply to a @FilterGroup@.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- 'crossDataset', 'filterGroup_crossDataset' - The filter new feature which can apply filter group to all data sets.
-- Choose one of the following options:
--
-- -   @ALL_DATASETS@
--
-- -   @SINGLE_DATASET@
newFilterGroup ::
  -- | 'filterGroupId'
  Prelude.Text ->
  -- | 'scopeConfiguration'
  FilterScopeConfiguration ->
  -- | 'crossDataset'
  CrossDatasetTypes ->
  FilterGroup
newFilterGroup
  pFilterGroupId_
  pScopeConfiguration_
  pCrossDataset_ =
    FilterGroup'
      { status = Prelude.Nothing,
        filterGroupId = pFilterGroupId_,
        filters = Prelude.mempty,
        scopeConfiguration = pScopeConfiguration_,
        crossDataset = pCrossDataset_
      }

-- | The status of the @FilterGroup@.
filterGroup_status :: Lens.Lens' FilterGroup (Prelude.Maybe WidgetStatus)
filterGroup_status = Lens.lens (\FilterGroup' {status} -> status) (\s@FilterGroup' {} a -> s {status = a} :: FilterGroup)

-- | The value that uniquely identifies a @FilterGroup@ within a dashboard,
-- template, or analysis.
filterGroup_filterGroupId :: Lens.Lens' FilterGroup Prelude.Text
filterGroup_filterGroupId = Lens.lens (\FilterGroup' {filterGroupId} -> filterGroupId) (\s@FilterGroup' {} a -> s {filterGroupId = a} :: FilterGroup)

-- | The list of filters that are present in a @FilterGroup@.
filterGroup_filters :: Lens.Lens' FilterGroup [Filter]
filterGroup_filters = Lens.lens (\FilterGroup' {filters} -> filters) (\s@FilterGroup' {} a -> s {filters = a} :: FilterGroup) Prelude.. Lens.coerced

-- | The configuration that specifies what scope to apply to a @FilterGroup@.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
filterGroup_scopeConfiguration :: Lens.Lens' FilterGroup FilterScopeConfiguration
filterGroup_scopeConfiguration = Lens.lens (\FilterGroup' {scopeConfiguration} -> scopeConfiguration) (\s@FilterGroup' {} a -> s {scopeConfiguration = a} :: FilterGroup)

-- | The filter new feature which can apply filter group to all data sets.
-- Choose one of the following options:
--
-- -   @ALL_DATASETS@
--
-- -   @SINGLE_DATASET@
filterGroup_crossDataset :: Lens.Lens' FilterGroup CrossDatasetTypes
filterGroup_crossDataset = Lens.lens (\FilterGroup' {crossDataset} -> crossDataset) (\s@FilterGroup' {} a -> s {crossDataset = a} :: FilterGroup)

instance Data.FromJSON FilterGroup where
  parseJSON =
    Data.withObject
      "FilterGroup"
      ( \x ->
          FilterGroup'
            Prelude.<$> (x Data..:? "Status")
            Prelude.<*> (x Data..: "FilterGroupId")
            Prelude.<*> (x Data..:? "Filters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "ScopeConfiguration")
            Prelude.<*> (x Data..: "CrossDataset")
      )

instance Prelude.Hashable FilterGroup where
  hashWithSalt _salt FilterGroup' {..} =
    _salt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` filterGroupId
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` scopeConfiguration
      `Prelude.hashWithSalt` crossDataset

instance Prelude.NFData FilterGroup where
  rnf FilterGroup' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf filterGroupId
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf scopeConfiguration
      `Prelude.seq` Prelude.rnf crossDataset

instance Data.ToJSON FilterGroup where
  toJSON FilterGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Status" Data..=) Prelude.<$> status,
            Prelude.Just ("FilterGroupId" Data..= filterGroupId),
            Prelude.Just ("Filters" Data..= filters),
            Prelude.Just
              ("ScopeConfiguration" Data..= scopeConfiguration),
            Prelude.Just ("CrossDataset" Data..= crossDataset)
          ]
      )
