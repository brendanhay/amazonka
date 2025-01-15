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
-- Module      : Amazonka.QuickSight.Types.FilterTextAreaControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterTextAreaControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TextAreaControlDisplayOptions

-- | A control to display a text box that is used to enter multiple entries.
--
-- /See:/ 'newFilterTextAreaControl' smart constructor.
data FilterTextAreaControl = FilterTextAreaControl'
  { -- | The delimiter that is used to separate the lines in text.
    delimiter :: Prelude.Maybe Prelude.Text,
    -- | The display options of a control.
    displayOptions :: Prelude.Maybe TextAreaControlDisplayOptions,
    -- | The ID of the @FilterTextAreaControl@.
    filterControlId :: Prelude.Text,
    -- | The title of the @FilterTextAreaControl@.
    title :: Prelude.Text,
    -- | The source filter ID of the @FilterTextAreaControl@.
    sourceFilterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterTextAreaControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delimiter', 'filterTextAreaControl_delimiter' - The delimiter that is used to separate the lines in text.
--
-- 'displayOptions', 'filterTextAreaControl_displayOptions' - The display options of a control.
--
-- 'filterControlId', 'filterTextAreaControl_filterControlId' - The ID of the @FilterTextAreaControl@.
--
-- 'title', 'filterTextAreaControl_title' - The title of the @FilterTextAreaControl@.
--
-- 'sourceFilterId', 'filterTextAreaControl_sourceFilterId' - The source filter ID of the @FilterTextAreaControl@.
newFilterTextAreaControl ::
  -- | 'filterControlId'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'sourceFilterId'
  Prelude.Text ->
  FilterTextAreaControl
newFilterTextAreaControl
  pFilterControlId_
  pTitle_
  pSourceFilterId_ =
    FilterTextAreaControl'
      { delimiter = Prelude.Nothing,
        displayOptions = Prelude.Nothing,
        filterControlId = pFilterControlId_,
        title = pTitle_,
        sourceFilterId = pSourceFilterId_
      }

-- | The delimiter that is used to separate the lines in text.
filterTextAreaControl_delimiter :: Lens.Lens' FilterTextAreaControl (Prelude.Maybe Prelude.Text)
filterTextAreaControl_delimiter = Lens.lens (\FilterTextAreaControl' {delimiter} -> delimiter) (\s@FilterTextAreaControl' {} a -> s {delimiter = a} :: FilterTextAreaControl)

-- | The display options of a control.
filterTextAreaControl_displayOptions :: Lens.Lens' FilterTextAreaControl (Prelude.Maybe TextAreaControlDisplayOptions)
filterTextAreaControl_displayOptions = Lens.lens (\FilterTextAreaControl' {displayOptions} -> displayOptions) (\s@FilterTextAreaControl' {} a -> s {displayOptions = a} :: FilterTextAreaControl)

-- | The ID of the @FilterTextAreaControl@.
filterTextAreaControl_filterControlId :: Lens.Lens' FilterTextAreaControl Prelude.Text
filterTextAreaControl_filterControlId = Lens.lens (\FilterTextAreaControl' {filterControlId} -> filterControlId) (\s@FilterTextAreaControl' {} a -> s {filterControlId = a} :: FilterTextAreaControl)

-- | The title of the @FilterTextAreaControl@.
filterTextAreaControl_title :: Lens.Lens' FilterTextAreaControl Prelude.Text
filterTextAreaControl_title = Lens.lens (\FilterTextAreaControl' {title} -> title) (\s@FilterTextAreaControl' {} a -> s {title = a} :: FilterTextAreaControl)

-- | The source filter ID of the @FilterTextAreaControl@.
filterTextAreaControl_sourceFilterId :: Lens.Lens' FilterTextAreaControl Prelude.Text
filterTextAreaControl_sourceFilterId = Lens.lens (\FilterTextAreaControl' {sourceFilterId} -> sourceFilterId) (\s@FilterTextAreaControl' {} a -> s {sourceFilterId = a} :: FilterTextAreaControl)

instance Data.FromJSON FilterTextAreaControl where
  parseJSON =
    Data.withObject
      "FilterTextAreaControl"
      ( \x ->
          FilterTextAreaControl'
            Prelude.<$> (x Data..:? "Delimiter")
            Prelude.<*> (x Data..:? "DisplayOptions")
            Prelude.<*> (x Data..: "FilterControlId")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "SourceFilterId")
      )

instance Prelude.Hashable FilterTextAreaControl where
  hashWithSalt _salt FilterTextAreaControl' {..} =
    _salt
      `Prelude.hashWithSalt` delimiter
      `Prelude.hashWithSalt` displayOptions
      `Prelude.hashWithSalt` filterControlId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` sourceFilterId

instance Prelude.NFData FilterTextAreaControl where
  rnf FilterTextAreaControl' {..} =
    Prelude.rnf delimiter `Prelude.seq`
      Prelude.rnf displayOptions `Prelude.seq`
        Prelude.rnf filterControlId `Prelude.seq`
          Prelude.rnf title `Prelude.seq`
            Prelude.rnf sourceFilterId

instance Data.ToJSON FilterTextAreaControl where
  toJSON FilterTextAreaControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Delimiter" Data..=) Prelude.<$> delimiter,
            ("DisplayOptions" Data..=)
              Prelude.<$> displayOptions,
            Prelude.Just
              ("FilterControlId" Data..= filterControlId),
            Prelude.Just ("Title" Data..= title),
            Prelude.Just
              ("SourceFilterId" Data..= sourceFilterId)
          ]
      )
