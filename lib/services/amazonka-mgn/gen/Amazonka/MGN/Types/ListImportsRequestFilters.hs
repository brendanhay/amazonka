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
-- Module      : Amazonka.MGN.Types.ListImportsRequestFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ListImportsRequestFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | List imports request filters.
--
-- /See:/ 'newListImportsRequestFilters' smart constructor.
data ListImportsRequestFilters = ListImportsRequestFilters'
  { -- | List imports request filters import IDs.
    importIDs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImportsRequestFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importIDs', 'listImportsRequestFilters_importIDs' - List imports request filters import IDs.
newListImportsRequestFilters ::
  ListImportsRequestFilters
newListImportsRequestFilters =
  ListImportsRequestFilters'
    { importIDs =
        Prelude.Nothing
    }

-- | List imports request filters import IDs.
listImportsRequestFilters_importIDs :: Lens.Lens' ListImportsRequestFilters (Prelude.Maybe [Prelude.Text])
listImportsRequestFilters_importIDs = Lens.lens (\ListImportsRequestFilters' {importIDs} -> importIDs) (\s@ListImportsRequestFilters' {} a -> s {importIDs = a} :: ListImportsRequestFilters) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ListImportsRequestFilters where
  hashWithSalt _salt ListImportsRequestFilters' {..} =
    _salt `Prelude.hashWithSalt` importIDs

instance Prelude.NFData ListImportsRequestFilters where
  rnf ListImportsRequestFilters' {..} =
    Prelude.rnf importIDs

instance Data.ToJSON ListImportsRequestFilters where
  toJSON ListImportsRequestFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [("importIDs" Data..=) Prelude.<$> importIDs]
      )
