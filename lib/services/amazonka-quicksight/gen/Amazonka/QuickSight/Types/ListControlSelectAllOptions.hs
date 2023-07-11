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
-- Module      : Amazonka.QuickSight.Types.ListControlSelectAllOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ListControlSelectAllOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The configuration of the @Select all@ options in a list control.
--
-- /See:/ 'newListControlSelectAllOptions' smart constructor.
data ListControlSelectAllOptions = ListControlSelectAllOptions'
  { -- | The visibility configuration of the @Select all@ options in a list
    -- control.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListControlSelectAllOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'visibility', 'listControlSelectAllOptions_visibility' - The visibility configuration of the @Select all@ options in a list
-- control.
newListControlSelectAllOptions ::
  ListControlSelectAllOptions
newListControlSelectAllOptions =
  ListControlSelectAllOptions'
    { visibility =
        Prelude.Nothing
    }

-- | The visibility configuration of the @Select all@ options in a list
-- control.
listControlSelectAllOptions_visibility :: Lens.Lens' ListControlSelectAllOptions (Prelude.Maybe Visibility)
listControlSelectAllOptions_visibility = Lens.lens (\ListControlSelectAllOptions' {visibility} -> visibility) (\s@ListControlSelectAllOptions' {} a -> s {visibility = a} :: ListControlSelectAllOptions)

instance Data.FromJSON ListControlSelectAllOptions where
  parseJSON =
    Data.withObject
      "ListControlSelectAllOptions"
      ( \x ->
          ListControlSelectAllOptions'
            Prelude.<$> (x Data..:? "Visibility")
      )

instance Prelude.Hashable ListControlSelectAllOptions where
  hashWithSalt _salt ListControlSelectAllOptions' {..} =
    _salt `Prelude.hashWithSalt` visibility

instance Prelude.NFData ListControlSelectAllOptions where
  rnf ListControlSelectAllOptions' {..} =
    Prelude.rnf visibility

instance Data.ToJSON ListControlSelectAllOptions where
  toJSON ListControlSelectAllOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Visibility" Data..=) Prelude.<$> visibility]
      )
