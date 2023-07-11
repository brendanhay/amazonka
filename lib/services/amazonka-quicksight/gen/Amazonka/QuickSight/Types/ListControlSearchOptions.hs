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
-- Module      : Amazonka.QuickSight.Types.ListControlSearchOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ListControlSearchOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The configuration of the search options in a list control.
--
-- /See:/ 'newListControlSearchOptions' smart constructor.
data ListControlSearchOptions = ListControlSearchOptions'
  { -- | The visibility configuration of the search options in a list control.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListControlSearchOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'visibility', 'listControlSearchOptions_visibility' - The visibility configuration of the search options in a list control.
newListControlSearchOptions ::
  ListControlSearchOptions
newListControlSearchOptions =
  ListControlSearchOptions'
    { visibility =
        Prelude.Nothing
    }

-- | The visibility configuration of the search options in a list control.
listControlSearchOptions_visibility :: Lens.Lens' ListControlSearchOptions (Prelude.Maybe Visibility)
listControlSearchOptions_visibility = Lens.lens (\ListControlSearchOptions' {visibility} -> visibility) (\s@ListControlSearchOptions' {} a -> s {visibility = a} :: ListControlSearchOptions)

instance Data.FromJSON ListControlSearchOptions where
  parseJSON =
    Data.withObject
      "ListControlSearchOptions"
      ( \x ->
          ListControlSearchOptions'
            Prelude.<$> (x Data..:? "Visibility")
      )

instance Prelude.Hashable ListControlSearchOptions where
  hashWithSalt _salt ListControlSearchOptions' {..} =
    _salt `Prelude.hashWithSalt` visibility

instance Prelude.NFData ListControlSearchOptions where
  rnf ListControlSearchOptions' {..} =
    Prelude.rnf visibility

instance Data.ToJSON ListControlSearchOptions where
  toJSON ListControlSearchOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Visibility" Data..=) Prelude.<$> visibility]
      )
