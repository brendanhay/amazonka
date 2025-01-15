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
-- Module      : Amazonka.Inspector2.Types.AmiAggregation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AmiAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.AmiSortBy
import Amazonka.Inspector2.Types.SortOrder
import Amazonka.Inspector2.Types.StringFilter
import qualified Amazonka.Prelude as Prelude

-- | The details that define an aggregation based on Amazon machine images
-- (AMIs).
--
-- /See:/ 'newAmiAggregation' smart constructor.
data AmiAggregation = AmiAggregation'
  { -- | The IDs of AMIs to aggregate findings for.
    amis :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The value to sort results by.
    sortBy :: Prelude.Maybe AmiSortBy,
    -- | The order to sort results by.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmiAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amis', 'amiAggregation_amis' - The IDs of AMIs to aggregate findings for.
--
-- 'sortBy', 'amiAggregation_sortBy' - The value to sort results by.
--
-- 'sortOrder', 'amiAggregation_sortOrder' - The order to sort results by.
newAmiAggregation ::
  AmiAggregation
newAmiAggregation =
  AmiAggregation'
    { amis = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | The IDs of AMIs to aggregate findings for.
amiAggregation_amis :: Lens.Lens' AmiAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
amiAggregation_amis = Lens.lens (\AmiAggregation' {amis} -> amis) (\s@AmiAggregation' {} a -> s {amis = a} :: AmiAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The value to sort results by.
amiAggregation_sortBy :: Lens.Lens' AmiAggregation (Prelude.Maybe AmiSortBy)
amiAggregation_sortBy = Lens.lens (\AmiAggregation' {sortBy} -> sortBy) (\s@AmiAggregation' {} a -> s {sortBy = a} :: AmiAggregation)

-- | The order to sort results by.
amiAggregation_sortOrder :: Lens.Lens' AmiAggregation (Prelude.Maybe SortOrder)
amiAggregation_sortOrder = Lens.lens (\AmiAggregation' {sortOrder} -> sortOrder) (\s@AmiAggregation' {} a -> s {sortOrder = a} :: AmiAggregation)

instance Prelude.Hashable AmiAggregation where
  hashWithSalt _salt AmiAggregation' {..} =
    _salt
      `Prelude.hashWithSalt` amis
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData AmiAggregation where
  rnf AmiAggregation' {..} =
    Prelude.rnf amis `Prelude.seq`
      Prelude.rnf sortBy `Prelude.seq`
        Prelude.rnf sortOrder

instance Data.ToJSON AmiAggregation where
  toJSON AmiAggregation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("amis" Data..=) Prelude.<$> amis,
            ("sortBy" Data..=) Prelude.<$> sortBy,
            ("sortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )
