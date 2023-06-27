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
-- Module      : Amazonka.MGN.Types.ListExportsRequestFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ListExportsRequestFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | List exports request filters.
--
-- /See:/ 'newListExportsRequestFilters' smart constructor.
data ListExportsRequestFilters = ListExportsRequestFilters'
  { -- | List exports request filters export ids.
    exportIDs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExportsRequestFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportIDs', 'listExportsRequestFilters_exportIDs' - List exports request filters export ids.
newListExportsRequestFilters ::
  ListExportsRequestFilters
newListExportsRequestFilters =
  ListExportsRequestFilters'
    { exportIDs =
        Prelude.Nothing
    }

-- | List exports request filters export ids.
listExportsRequestFilters_exportIDs :: Lens.Lens' ListExportsRequestFilters (Prelude.Maybe [Prelude.Text])
listExportsRequestFilters_exportIDs = Lens.lens (\ListExportsRequestFilters' {exportIDs} -> exportIDs) (\s@ListExportsRequestFilters' {} a -> s {exportIDs = a} :: ListExportsRequestFilters) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ListExportsRequestFilters where
  hashWithSalt _salt ListExportsRequestFilters' {..} =
    _salt `Prelude.hashWithSalt` exportIDs

instance Prelude.NFData ListExportsRequestFilters where
  rnf ListExportsRequestFilters' {..} =
    Prelude.rnf exportIDs

instance Data.ToJSON ListExportsRequestFilters where
  toJSON ListExportsRequestFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [("exportIDs" Data..=) Prelude.<$> exportIDs]
      )
