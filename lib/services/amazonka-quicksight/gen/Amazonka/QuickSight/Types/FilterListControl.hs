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
-- Module      : Amazonka.QuickSight.Types.FilterListControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterListControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CascadingControlConfiguration
import Amazonka.QuickSight.Types.FilterSelectableValues
import Amazonka.QuickSight.Types.ListControlDisplayOptions
import Amazonka.QuickSight.Types.SheetControlListType

-- | A control to display a list of buttons or boxes. This is used to select
-- either a single value or multiple values.
--
-- /See:/ 'newFilterListControl' smart constructor.
data FilterListControl = FilterListControl'
  { -- | The values that are displayed in a control can be configured to only
    -- show values that are valid based on what\'s selected in other controls.
    cascadingControlConfiguration :: Prelude.Maybe CascadingControlConfiguration,
    -- | The display options of a control.
    displayOptions :: Prelude.Maybe ListControlDisplayOptions,
    -- | A list of selectable values that are used in a control.
    selectableValues :: Prelude.Maybe FilterSelectableValues,
    -- | The type of @FilterListControl@. Choose one of the following options:
    --
    -- -   @MULTI_SELECT@: The user can select multiple entries from the list.
    --
    -- -   @SINGLE_SELECT@: The user can select a single entry from the list.
    type' :: Prelude.Maybe SheetControlListType,
    -- | The ID of the @FilterListControl@.
    filterControlId :: Prelude.Text,
    -- | The title of the @FilterListControl@.
    title :: Prelude.Text,
    -- | The source filter ID of the @FilterListControl@.
    sourceFilterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterListControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cascadingControlConfiguration', 'filterListControl_cascadingControlConfiguration' - The values that are displayed in a control can be configured to only
-- show values that are valid based on what\'s selected in other controls.
--
-- 'displayOptions', 'filterListControl_displayOptions' - The display options of a control.
--
-- 'selectableValues', 'filterListControl_selectableValues' - A list of selectable values that are used in a control.
--
-- 'type'', 'filterListControl_type' - The type of @FilterListControl@. Choose one of the following options:
--
-- -   @MULTI_SELECT@: The user can select multiple entries from the list.
--
-- -   @SINGLE_SELECT@: The user can select a single entry from the list.
--
-- 'filterControlId', 'filterListControl_filterControlId' - The ID of the @FilterListControl@.
--
-- 'title', 'filterListControl_title' - The title of the @FilterListControl@.
--
-- 'sourceFilterId', 'filterListControl_sourceFilterId' - The source filter ID of the @FilterListControl@.
newFilterListControl ::
  -- | 'filterControlId'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'sourceFilterId'
  Prelude.Text ->
  FilterListControl
newFilterListControl
  pFilterControlId_
  pTitle_
  pSourceFilterId_ =
    FilterListControl'
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
filterListControl_cascadingControlConfiguration :: Lens.Lens' FilterListControl (Prelude.Maybe CascadingControlConfiguration)
filterListControl_cascadingControlConfiguration = Lens.lens (\FilterListControl' {cascadingControlConfiguration} -> cascadingControlConfiguration) (\s@FilterListControl' {} a -> s {cascadingControlConfiguration = a} :: FilterListControl)

-- | The display options of a control.
filterListControl_displayOptions :: Lens.Lens' FilterListControl (Prelude.Maybe ListControlDisplayOptions)
filterListControl_displayOptions = Lens.lens (\FilterListControl' {displayOptions} -> displayOptions) (\s@FilterListControl' {} a -> s {displayOptions = a} :: FilterListControl)

-- | A list of selectable values that are used in a control.
filterListControl_selectableValues :: Lens.Lens' FilterListControl (Prelude.Maybe FilterSelectableValues)
filterListControl_selectableValues = Lens.lens (\FilterListControl' {selectableValues} -> selectableValues) (\s@FilterListControl' {} a -> s {selectableValues = a} :: FilterListControl)

-- | The type of @FilterListControl@. Choose one of the following options:
--
-- -   @MULTI_SELECT@: The user can select multiple entries from the list.
--
-- -   @SINGLE_SELECT@: The user can select a single entry from the list.
filterListControl_type :: Lens.Lens' FilterListControl (Prelude.Maybe SheetControlListType)
filterListControl_type = Lens.lens (\FilterListControl' {type'} -> type') (\s@FilterListControl' {} a -> s {type' = a} :: FilterListControl)

-- | The ID of the @FilterListControl@.
filterListControl_filterControlId :: Lens.Lens' FilterListControl Prelude.Text
filterListControl_filterControlId = Lens.lens (\FilterListControl' {filterControlId} -> filterControlId) (\s@FilterListControl' {} a -> s {filterControlId = a} :: FilterListControl)

-- | The title of the @FilterListControl@.
filterListControl_title :: Lens.Lens' FilterListControl Prelude.Text
filterListControl_title = Lens.lens (\FilterListControl' {title} -> title) (\s@FilterListControl' {} a -> s {title = a} :: FilterListControl)

-- | The source filter ID of the @FilterListControl@.
filterListControl_sourceFilterId :: Lens.Lens' FilterListControl Prelude.Text
filterListControl_sourceFilterId = Lens.lens (\FilterListControl' {sourceFilterId} -> sourceFilterId) (\s@FilterListControl' {} a -> s {sourceFilterId = a} :: FilterListControl)

instance Data.FromJSON FilterListControl where
  parseJSON =
    Data.withObject
      "FilterListControl"
      ( \x ->
          FilterListControl'
            Prelude.<$> (x Data..:? "CascadingControlConfiguration")
            Prelude.<*> (x Data..:? "DisplayOptions")
            Prelude.<*> (x Data..:? "SelectableValues")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..: "FilterControlId")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "SourceFilterId")
      )

instance Prelude.Hashable FilterListControl where
  hashWithSalt _salt FilterListControl' {..} =
    _salt
      `Prelude.hashWithSalt` cascadingControlConfiguration
      `Prelude.hashWithSalt` displayOptions
      `Prelude.hashWithSalt` selectableValues
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` filterControlId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` sourceFilterId

instance Prelude.NFData FilterListControl where
  rnf FilterListControl' {..} =
    Prelude.rnf cascadingControlConfiguration
      `Prelude.seq` Prelude.rnf displayOptions
      `Prelude.seq` Prelude.rnf selectableValues
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf filterControlId
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf sourceFilterId

instance Data.ToJSON FilterListControl where
  toJSON FilterListControl' {..} =
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
