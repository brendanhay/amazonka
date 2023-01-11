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
-- Module      : Amazonka.QuickSight.Types.FilterDateTimePickerControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterDateTimePickerControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DateTimePickerControlDisplayOptions
import Amazonka.QuickSight.Types.SheetControlDateTimePickerType

-- | A control from a date filter that is used to specify date and time.
--
-- /See:/ 'newFilterDateTimePickerControl' smart constructor.
data FilterDateTimePickerControl = FilterDateTimePickerControl'
  { -- | The display options of a control.
    displayOptions :: Prelude.Maybe DateTimePickerControlDisplayOptions,
    -- | The date time picker type of a @FilterDateTimePickerControl@. Choose one
    -- of the following options:
    --
    -- -   @SINGLE_VALUED@: The filter condition is a fixed date.
    --
    -- -   @DATE_RANGE@: The filter condition is a date time range.
    type' :: Prelude.Maybe SheetControlDateTimePickerType,
    -- | The ID of the @FilterDateTimePickerControl@.
    filterControlId :: Prelude.Text,
    -- | The title of the @FilterDateTimePickerControl@.
    title :: Prelude.Text,
    -- | The source filter ID of the @FilterDateTimePickerControl@.
    sourceFilterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterDateTimePickerControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayOptions', 'filterDateTimePickerControl_displayOptions' - The display options of a control.
--
-- 'type'', 'filterDateTimePickerControl_type' - The date time picker type of a @FilterDateTimePickerControl@. Choose one
-- of the following options:
--
-- -   @SINGLE_VALUED@: The filter condition is a fixed date.
--
-- -   @DATE_RANGE@: The filter condition is a date time range.
--
-- 'filterControlId', 'filterDateTimePickerControl_filterControlId' - The ID of the @FilterDateTimePickerControl@.
--
-- 'title', 'filterDateTimePickerControl_title' - The title of the @FilterDateTimePickerControl@.
--
-- 'sourceFilterId', 'filterDateTimePickerControl_sourceFilterId' - The source filter ID of the @FilterDateTimePickerControl@.
newFilterDateTimePickerControl ::
  -- | 'filterControlId'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'sourceFilterId'
  Prelude.Text ->
  FilterDateTimePickerControl
newFilterDateTimePickerControl
  pFilterControlId_
  pTitle_
  pSourceFilterId_ =
    FilterDateTimePickerControl'
      { displayOptions =
          Prelude.Nothing,
        type' = Prelude.Nothing,
        filterControlId = pFilterControlId_,
        title = pTitle_,
        sourceFilterId = pSourceFilterId_
      }

-- | The display options of a control.
filterDateTimePickerControl_displayOptions :: Lens.Lens' FilterDateTimePickerControl (Prelude.Maybe DateTimePickerControlDisplayOptions)
filterDateTimePickerControl_displayOptions = Lens.lens (\FilterDateTimePickerControl' {displayOptions} -> displayOptions) (\s@FilterDateTimePickerControl' {} a -> s {displayOptions = a} :: FilterDateTimePickerControl)

-- | The date time picker type of a @FilterDateTimePickerControl@. Choose one
-- of the following options:
--
-- -   @SINGLE_VALUED@: The filter condition is a fixed date.
--
-- -   @DATE_RANGE@: The filter condition is a date time range.
filterDateTimePickerControl_type :: Lens.Lens' FilterDateTimePickerControl (Prelude.Maybe SheetControlDateTimePickerType)
filterDateTimePickerControl_type = Lens.lens (\FilterDateTimePickerControl' {type'} -> type') (\s@FilterDateTimePickerControl' {} a -> s {type' = a} :: FilterDateTimePickerControl)

-- | The ID of the @FilterDateTimePickerControl@.
filterDateTimePickerControl_filterControlId :: Lens.Lens' FilterDateTimePickerControl Prelude.Text
filterDateTimePickerControl_filterControlId = Lens.lens (\FilterDateTimePickerControl' {filterControlId} -> filterControlId) (\s@FilterDateTimePickerControl' {} a -> s {filterControlId = a} :: FilterDateTimePickerControl)

-- | The title of the @FilterDateTimePickerControl@.
filterDateTimePickerControl_title :: Lens.Lens' FilterDateTimePickerControl Prelude.Text
filterDateTimePickerControl_title = Lens.lens (\FilterDateTimePickerControl' {title} -> title) (\s@FilterDateTimePickerControl' {} a -> s {title = a} :: FilterDateTimePickerControl)

-- | The source filter ID of the @FilterDateTimePickerControl@.
filterDateTimePickerControl_sourceFilterId :: Lens.Lens' FilterDateTimePickerControl Prelude.Text
filterDateTimePickerControl_sourceFilterId = Lens.lens (\FilterDateTimePickerControl' {sourceFilterId} -> sourceFilterId) (\s@FilterDateTimePickerControl' {} a -> s {sourceFilterId = a} :: FilterDateTimePickerControl)

instance Data.FromJSON FilterDateTimePickerControl where
  parseJSON =
    Data.withObject
      "FilterDateTimePickerControl"
      ( \x ->
          FilterDateTimePickerControl'
            Prelude.<$> (x Data..:? "DisplayOptions")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..: "FilterControlId")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "SourceFilterId")
      )

instance Prelude.Hashable FilterDateTimePickerControl where
  hashWithSalt _salt FilterDateTimePickerControl' {..} =
    _salt `Prelude.hashWithSalt` displayOptions
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` filterControlId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` sourceFilterId

instance Prelude.NFData FilterDateTimePickerControl where
  rnf FilterDateTimePickerControl' {..} =
    Prelude.rnf displayOptions
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf filterControlId
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf sourceFilterId

instance Data.ToJSON FilterDateTimePickerControl where
  toJSON FilterDateTimePickerControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisplayOptions" Data..=)
              Prelude.<$> displayOptions,
            ("Type" Data..=) Prelude.<$> type',
            Prelude.Just
              ("FilterControlId" Data..= filterControlId),
            Prelude.Just ("Title" Data..= title),
            Prelude.Just
              ("SourceFilterId" Data..= sourceFilterId)
          ]
      )
