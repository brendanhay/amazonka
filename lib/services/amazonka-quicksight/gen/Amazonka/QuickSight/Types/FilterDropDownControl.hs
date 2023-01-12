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
-- Module      : Amazonka.QuickSight.Types.FilterDropDownControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterDropDownControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CascadingControlConfiguration
import Amazonka.QuickSight.Types.DropDownControlDisplayOptions
import Amazonka.QuickSight.Types.FilterSelectableValues
import Amazonka.QuickSight.Types.SheetControlListType

-- | A control to display a dropdown list with buttons that are used to
-- select a single value.
--
-- /See:/ 'newFilterDropDownControl' smart constructor.
data FilterDropDownControl = FilterDropDownControl'
  { -- | The values that are displayed in a control can be configured to only
    -- show values that are valid based on what\'s selected in other controls.
    cascadingControlConfiguration :: Prelude.Maybe CascadingControlConfiguration,
    -- | The display options of the @FilterDropDownControl@.
    displayOptions :: Prelude.Maybe DropDownControlDisplayOptions,
    -- | A list of selectable values that are used in a control.
    selectableValues :: Prelude.Maybe FilterSelectableValues,
    -- | The type of the @FilterDropDownControl@. Choose one of the following
    -- options:
    --
    -- -   @MULTI_SELECT@: The user can select multiple entries from a dropdown
    --     menu.
    --
    -- -   @SINGLE_SELECT@: The user can select a single entry from a dropdown
    --     menu.
    type' :: Prelude.Maybe SheetControlListType,
    -- | The ID of the @FilterDropDownControl@.
    filterControlId :: Prelude.Text,
    -- | The title of the @FilterDropDownControl@.
    title :: Prelude.Text,
    -- | The source filter ID of the @FilterDropDownControl@.
    sourceFilterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterDropDownControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cascadingControlConfiguration', 'filterDropDownControl_cascadingControlConfiguration' - The values that are displayed in a control can be configured to only
-- show values that are valid based on what\'s selected in other controls.
--
-- 'displayOptions', 'filterDropDownControl_displayOptions' - The display options of the @FilterDropDownControl@.
--
-- 'selectableValues', 'filterDropDownControl_selectableValues' - A list of selectable values that are used in a control.
--
-- 'type'', 'filterDropDownControl_type' - The type of the @FilterDropDownControl@. Choose one of the following
-- options:
--
-- -   @MULTI_SELECT@: The user can select multiple entries from a dropdown
--     menu.
--
-- -   @SINGLE_SELECT@: The user can select a single entry from a dropdown
--     menu.
--
-- 'filterControlId', 'filterDropDownControl_filterControlId' - The ID of the @FilterDropDownControl@.
--
-- 'title', 'filterDropDownControl_title' - The title of the @FilterDropDownControl@.
--
-- 'sourceFilterId', 'filterDropDownControl_sourceFilterId' - The source filter ID of the @FilterDropDownControl@.
newFilterDropDownControl ::
  -- | 'filterControlId'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'sourceFilterId'
  Prelude.Text ->
  FilterDropDownControl
newFilterDropDownControl
  pFilterControlId_
  pTitle_
  pSourceFilterId_ =
    FilterDropDownControl'
      { cascadingControlConfiguration =
          Prelude.Nothing,
        displayOptions = Prelude.Nothing,
        selectableValues = Prelude.Nothing,
        type' = Prelude.Nothing,
        filterControlId = pFilterControlId_,
        title = pTitle_,
        sourceFilterId = pSourceFilterId_
      }

-- | The values that are displayed in a control can be configured to only
-- show values that are valid based on what\'s selected in other controls.
filterDropDownControl_cascadingControlConfiguration :: Lens.Lens' FilterDropDownControl (Prelude.Maybe CascadingControlConfiguration)
filterDropDownControl_cascadingControlConfiguration = Lens.lens (\FilterDropDownControl' {cascadingControlConfiguration} -> cascadingControlConfiguration) (\s@FilterDropDownControl' {} a -> s {cascadingControlConfiguration = a} :: FilterDropDownControl)

-- | The display options of the @FilterDropDownControl@.
filterDropDownControl_displayOptions :: Lens.Lens' FilterDropDownControl (Prelude.Maybe DropDownControlDisplayOptions)
filterDropDownControl_displayOptions = Lens.lens (\FilterDropDownControl' {displayOptions} -> displayOptions) (\s@FilterDropDownControl' {} a -> s {displayOptions = a} :: FilterDropDownControl)

-- | A list of selectable values that are used in a control.
filterDropDownControl_selectableValues :: Lens.Lens' FilterDropDownControl (Prelude.Maybe FilterSelectableValues)
filterDropDownControl_selectableValues = Lens.lens (\FilterDropDownControl' {selectableValues} -> selectableValues) (\s@FilterDropDownControl' {} a -> s {selectableValues = a} :: FilterDropDownControl)

-- | The type of the @FilterDropDownControl@. Choose one of the following
-- options:
--
-- -   @MULTI_SELECT@: The user can select multiple entries from a dropdown
--     menu.
--
-- -   @SINGLE_SELECT@: The user can select a single entry from a dropdown
--     menu.
filterDropDownControl_type :: Lens.Lens' FilterDropDownControl (Prelude.Maybe SheetControlListType)
filterDropDownControl_type = Lens.lens (\FilterDropDownControl' {type'} -> type') (\s@FilterDropDownControl' {} a -> s {type' = a} :: FilterDropDownControl)

-- | The ID of the @FilterDropDownControl@.
filterDropDownControl_filterControlId :: Lens.Lens' FilterDropDownControl Prelude.Text
filterDropDownControl_filterControlId = Lens.lens (\FilterDropDownControl' {filterControlId} -> filterControlId) (\s@FilterDropDownControl' {} a -> s {filterControlId = a} :: FilterDropDownControl)

-- | The title of the @FilterDropDownControl@.
filterDropDownControl_title :: Lens.Lens' FilterDropDownControl Prelude.Text
filterDropDownControl_title = Lens.lens (\FilterDropDownControl' {title} -> title) (\s@FilterDropDownControl' {} a -> s {title = a} :: FilterDropDownControl)

-- | The source filter ID of the @FilterDropDownControl@.
filterDropDownControl_sourceFilterId :: Lens.Lens' FilterDropDownControl Prelude.Text
filterDropDownControl_sourceFilterId = Lens.lens (\FilterDropDownControl' {sourceFilterId} -> sourceFilterId) (\s@FilterDropDownControl' {} a -> s {sourceFilterId = a} :: FilterDropDownControl)

instance Data.FromJSON FilterDropDownControl where
  parseJSON =
    Data.withObject
      "FilterDropDownControl"
      ( \x ->
          FilterDropDownControl'
            Prelude.<$> (x Data..:? "CascadingControlConfiguration")
            Prelude.<*> (x Data..:? "DisplayOptions")
            Prelude.<*> (x Data..:? "SelectableValues")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..: "FilterControlId")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "SourceFilterId")
      )

instance Prelude.Hashable FilterDropDownControl where
  hashWithSalt _salt FilterDropDownControl' {..} =
    _salt
      `Prelude.hashWithSalt` cascadingControlConfiguration
      `Prelude.hashWithSalt` displayOptions
      `Prelude.hashWithSalt` selectableValues
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` filterControlId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` sourceFilterId

instance Prelude.NFData FilterDropDownControl where
  rnf FilterDropDownControl' {..} =
    Prelude.rnf cascadingControlConfiguration
      `Prelude.seq` Prelude.rnf displayOptions
      `Prelude.seq` Prelude.rnf selectableValues
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf filterControlId
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf sourceFilterId

instance Data.ToJSON FilterDropDownControl where
  toJSON FilterDropDownControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CascadingControlConfiguration" Data..=)
              Prelude.<$> cascadingControlConfiguration,
            ("DisplayOptions" Data..=)
              Prelude.<$> displayOptions,
            ("SelectableValues" Data..=)
              Prelude.<$> selectableValues,
            ("Type" Data..=) Prelude.<$> type',
            Prelude.Just
              ("FilterControlId" Data..= filterControlId),
            Prelude.Just ("Title" Data..= title),
            Prelude.Just
              ("SourceFilterId" Data..= sourceFilterId)
          ]
      )
