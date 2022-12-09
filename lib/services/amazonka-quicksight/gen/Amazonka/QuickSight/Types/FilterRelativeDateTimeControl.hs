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
-- Module      : Amazonka.QuickSight.Types.FilterRelativeDateTimeControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterRelativeDateTimeControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.RelativeDateTimeControlDisplayOptions

-- | A control from a date filter that is used to specify the relative date.
--
-- /See:/ 'newFilterRelativeDateTimeControl' smart constructor.
data FilterRelativeDateTimeControl = FilterRelativeDateTimeControl'
  { -- | The display options of a control.
    displayOptions :: Prelude.Maybe RelativeDateTimeControlDisplayOptions,
    -- | The ID of the @FilterTextAreaControl@.
    filterControlId :: Prelude.Text,
    -- | The title of the @FilterTextAreaControl@.
    title :: Prelude.Text,
    -- | The source filter ID of the @FilterTextAreaControl@.
    sourceFilterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterRelativeDateTimeControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayOptions', 'filterRelativeDateTimeControl_displayOptions' - The display options of a control.
--
-- 'filterControlId', 'filterRelativeDateTimeControl_filterControlId' - The ID of the @FilterTextAreaControl@.
--
-- 'title', 'filterRelativeDateTimeControl_title' - The title of the @FilterTextAreaControl@.
--
-- 'sourceFilterId', 'filterRelativeDateTimeControl_sourceFilterId' - The source filter ID of the @FilterTextAreaControl@.
newFilterRelativeDateTimeControl ::
  -- | 'filterControlId'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'sourceFilterId'
  Prelude.Text ->
  FilterRelativeDateTimeControl
newFilterRelativeDateTimeControl
  pFilterControlId_
  pTitle_
  pSourceFilterId_ =
    FilterRelativeDateTimeControl'
      { displayOptions =
          Prelude.Nothing,
        filterControlId = pFilterControlId_,
        title = pTitle_,
        sourceFilterId = pSourceFilterId_
      }

-- | The display options of a control.
filterRelativeDateTimeControl_displayOptions :: Lens.Lens' FilterRelativeDateTimeControl (Prelude.Maybe RelativeDateTimeControlDisplayOptions)
filterRelativeDateTimeControl_displayOptions = Lens.lens (\FilterRelativeDateTimeControl' {displayOptions} -> displayOptions) (\s@FilterRelativeDateTimeControl' {} a -> s {displayOptions = a} :: FilterRelativeDateTimeControl)

-- | The ID of the @FilterTextAreaControl@.
filterRelativeDateTimeControl_filterControlId :: Lens.Lens' FilterRelativeDateTimeControl Prelude.Text
filterRelativeDateTimeControl_filterControlId = Lens.lens (\FilterRelativeDateTimeControl' {filterControlId} -> filterControlId) (\s@FilterRelativeDateTimeControl' {} a -> s {filterControlId = a} :: FilterRelativeDateTimeControl)

-- | The title of the @FilterTextAreaControl@.
filterRelativeDateTimeControl_title :: Lens.Lens' FilterRelativeDateTimeControl Prelude.Text
filterRelativeDateTimeControl_title = Lens.lens (\FilterRelativeDateTimeControl' {title} -> title) (\s@FilterRelativeDateTimeControl' {} a -> s {title = a} :: FilterRelativeDateTimeControl)

-- | The source filter ID of the @FilterTextAreaControl@.
filterRelativeDateTimeControl_sourceFilterId :: Lens.Lens' FilterRelativeDateTimeControl Prelude.Text
filterRelativeDateTimeControl_sourceFilterId = Lens.lens (\FilterRelativeDateTimeControl' {sourceFilterId} -> sourceFilterId) (\s@FilterRelativeDateTimeControl' {} a -> s {sourceFilterId = a} :: FilterRelativeDateTimeControl)

instance Data.FromJSON FilterRelativeDateTimeControl where
  parseJSON =
    Data.withObject
      "FilterRelativeDateTimeControl"
      ( \x ->
          FilterRelativeDateTimeControl'
            Prelude.<$> (x Data..:? "DisplayOptions")
            Prelude.<*> (x Data..: "FilterControlId")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "SourceFilterId")
      )

instance
  Prelude.Hashable
    FilterRelativeDateTimeControl
  where
  hashWithSalt _salt FilterRelativeDateTimeControl' {..} =
    _salt `Prelude.hashWithSalt` displayOptions
      `Prelude.hashWithSalt` filterControlId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` sourceFilterId

instance Prelude.NFData FilterRelativeDateTimeControl where
  rnf FilterRelativeDateTimeControl' {..} =
    Prelude.rnf displayOptions
      `Prelude.seq` Prelude.rnf filterControlId
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf sourceFilterId

instance Data.ToJSON FilterRelativeDateTimeControl where
  toJSON FilterRelativeDateTimeControl' {..} =
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
