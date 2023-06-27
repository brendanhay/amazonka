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
-- Module      : Amazonka.QuickSight.Types.FilterTextFieldControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterTextFieldControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TextFieldControlDisplayOptions

-- | A control to display a text box that is used to enter a single entry.
--
-- /See:/ 'newFilterTextFieldControl' smart constructor.
data FilterTextFieldControl = FilterTextFieldControl'
  { -- | The display options of a control.
    displayOptions :: Prelude.Maybe TextFieldControlDisplayOptions,
    -- | The ID of the @FilterTextFieldControl@.
    filterControlId :: Prelude.Text,
    -- | The title of the @FilterTextFieldControl@.
    title :: Prelude.Text,
    -- | The source filter ID of the @FilterTextFieldControl@.
    sourceFilterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterTextFieldControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayOptions', 'filterTextFieldControl_displayOptions' - The display options of a control.
--
-- 'filterControlId', 'filterTextFieldControl_filterControlId' - The ID of the @FilterTextFieldControl@.
--
-- 'title', 'filterTextFieldControl_title' - The title of the @FilterTextFieldControl@.
--
-- 'sourceFilterId', 'filterTextFieldControl_sourceFilterId' - The source filter ID of the @FilterTextFieldControl@.
newFilterTextFieldControl ::
  -- | 'filterControlId'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'sourceFilterId'
  Prelude.Text ->
  FilterTextFieldControl
newFilterTextFieldControl
  pFilterControlId_
  pTitle_
  pSourceFilterId_ =
    FilterTextFieldControl'
      { displayOptions =
          Prelude.Nothing,
        filterControlId = pFilterControlId_,
        title = pTitle_,
        sourceFilterId = pSourceFilterId_
      }

-- | The display options of a control.
filterTextFieldControl_displayOptions :: Lens.Lens' FilterTextFieldControl (Prelude.Maybe TextFieldControlDisplayOptions)
filterTextFieldControl_displayOptions = Lens.lens (\FilterTextFieldControl' {displayOptions} -> displayOptions) (\s@FilterTextFieldControl' {} a -> s {displayOptions = a} :: FilterTextFieldControl)

-- | The ID of the @FilterTextFieldControl@.
filterTextFieldControl_filterControlId :: Lens.Lens' FilterTextFieldControl Prelude.Text
filterTextFieldControl_filterControlId = Lens.lens (\FilterTextFieldControl' {filterControlId} -> filterControlId) (\s@FilterTextFieldControl' {} a -> s {filterControlId = a} :: FilterTextFieldControl)

-- | The title of the @FilterTextFieldControl@.
filterTextFieldControl_title :: Lens.Lens' FilterTextFieldControl Prelude.Text
filterTextFieldControl_title = Lens.lens (\FilterTextFieldControl' {title} -> title) (\s@FilterTextFieldControl' {} a -> s {title = a} :: FilterTextFieldControl)

-- | The source filter ID of the @FilterTextFieldControl@.
filterTextFieldControl_sourceFilterId :: Lens.Lens' FilterTextFieldControl Prelude.Text
filterTextFieldControl_sourceFilterId = Lens.lens (\FilterTextFieldControl' {sourceFilterId} -> sourceFilterId) (\s@FilterTextFieldControl' {} a -> s {sourceFilterId = a} :: FilterTextFieldControl)

instance Data.FromJSON FilterTextFieldControl where
  parseJSON =
    Data.withObject
      "FilterTextFieldControl"
      ( \x ->
          FilterTextFieldControl'
            Prelude.<$> (x Data..:? "DisplayOptions")
            Prelude.<*> (x Data..: "FilterControlId")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "SourceFilterId")
      )

instance Prelude.Hashable FilterTextFieldControl where
  hashWithSalt _salt FilterTextFieldControl' {..} =
    _salt
      `Prelude.hashWithSalt` displayOptions
      `Prelude.hashWithSalt` filterControlId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` sourceFilterId

instance Prelude.NFData FilterTextFieldControl where
  rnf FilterTextFieldControl' {..} =
    Prelude.rnf displayOptions
      `Prelude.seq` Prelude.rnf filterControlId
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf sourceFilterId

instance Data.ToJSON FilterTextFieldControl where
  toJSON FilterTextFieldControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisplayOptions" Data..=)
              Prelude.<$> displayOptions,
            Prelude.Just
              ("FilterControlId" Data..= filterControlId),
            Prelude.Just ("Title" Data..= title),
            Prelude.Just
              ("SourceFilterId" Data..= sourceFilterId)
          ]
      )
