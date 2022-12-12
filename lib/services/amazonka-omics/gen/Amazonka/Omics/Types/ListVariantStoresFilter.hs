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
-- Module      : Amazonka.Omics.Types.ListVariantStoresFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ListVariantStoresFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.StoreStatus
import qualified Amazonka.Prelude as Prelude

-- | A filter for variant stores.
--
-- /See:/ 'newListVariantStoresFilter' smart constructor.
data ListVariantStoresFilter = ListVariantStoresFilter'
  { -- | A status to filter on.
    status :: Prelude.Maybe StoreStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVariantStoresFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listVariantStoresFilter_status' - A status to filter on.
newListVariantStoresFilter ::
  ListVariantStoresFilter
newListVariantStoresFilter =
  ListVariantStoresFilter' {status = Prelude.Nothing}

-- | A status to filter on.
listVariantStoresFilter_status :: Lens.Lens' ListVariantStoresFilter (Prelude.Maybe StoreStatus)
listVariantStoresFilter_status = Lens.lens (\ListVariantStoresFilter' {status} -> status) (\s@ListVariantStoresFilter' {} a -> s {status = a} :: ListVariantStoresFilter)

instance Prelude.Hashable ListVariantStoresFilter where
  hashWithSalt _salt ListVariantStoresFilter' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData ListVariantStoresFilter where
  rnf ListVariantStoresFilter' {..} = Prelude.rnf status

instance Data.ToJSON ListVariantStoresFilter where
  toJSON ListVariantStoresFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [("status" Data..=) Prelude.<$> status]
      )
