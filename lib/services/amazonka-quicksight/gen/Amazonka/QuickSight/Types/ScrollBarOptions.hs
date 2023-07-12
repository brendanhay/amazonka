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
-- Module      : Amazonka.QuickSight.Types.ScrollBarOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ScrollBarOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility
import Amazonka.QuickSight.Types.VisibleRangeOptions

-- | The visual display options for a data zoom scroll bar.
--
-- /See:/ 'newScrollBarOptions' smart constructor.
data ScrollBarOptions = ScrollBarOptions'
  { -- | The visibility of the data zoom scroll bar.
    visibility :: Prelude.Maybe Visibility,
    -- | The visibility range for the data zoom scroll bar.
    visibleRange :: Prelude.Maybe VisibleRangeOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScrollBarOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'visibility', 'scrollBarOptions_visibility' - The visibility of the data zoom scroll bar.
--
-- 'visibleRange', 'scrollBarOptions_visibleRange' - The visibility range for the data zoom scroll bar.
newScrollBarOptions ::
  ScrollBarOptions
newScrollBarOptions =
  ScrollBarOptions'
    { visibility = Prelude.Nothing,
      visibleRange = Prelude.Nothing
    }

-- | The visibility of the data zoom scroll bar.
scrollBarOptions_visibility :: Lens.Lens' ScrollBarOptions (Prelude.Maybe Visibility)
scrollBarOptions_visibility = Lens.lens (\ScrollBarOptions' {visibility} -> visibility) (\s@ScrollBarOptions' {} a -> s {visibility = a} :: ScrollBarOptions)

-- | The visibility range for the data zoom scroll bar.
scrollBarOptions_visibleRange :: Lens.Lens' ScrollBarOptions (Prelude.Maybe VisibleRangeOptions)
scrollBarOptions_visibleRange = Lens.lens (\ScrollBarOptions' {visibleRange} -> visibleRange) (\s@ScrollBarOptions' {} a -> s {visibleRange = a} :: ScrollBarOptions)

instance Data.FromJSON ScrollBarOptions where
  parseJSON =
    Data.withObject
      "ScrollBarOptions"
      ( \x ->
          ScrollBarOptions'
            Prelude.<$> (x Data..:? "Visibility")
            Prelude.<*> (x Data..:? "VisibleRange")
      )

instance Prelude.Hashable ScrollBarOptions where
  hashWithSalt _salt ScrollBarOptions' {..} =
    _salt
      `Prelude.hashWithSalt` visibility
      `Prelude.hashWithSalt` visibleRange

instance Prelude.NFData ScrollBarOptions where
  rnf ScrollBarOptions' {..} =
    Prelude.rnf visibility
      `Prelude.seq` Prelude.rnf visibleRange

instance Data.ToJSON ScrollBarOptions where
  toJSON ScrollBarOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Visibility" Data..=) Prelude.<$> visibility,
            ("VisibleRange" Data..=) Prelude.<$> visibleRange
          ]
      )
