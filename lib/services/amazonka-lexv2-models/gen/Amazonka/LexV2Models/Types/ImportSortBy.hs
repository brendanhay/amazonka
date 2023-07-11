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
-- Module      : Amazonka.LexV2Models.Types.ImportSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ImportSortBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ImportSortAttribute
import Amazonka.LexV2Models.Types.SortOrder
import qualified Amazonka.Prelude as Prelude

-- | Provides information for sorting a list of imports.
--
-- /See:/ 'newImportSortBy' smart constructor.
data ImportSortBy = ImportSortBy'
  { -- | The export field to use for sorting.
    attribute :: ImportSortAttribute,
    -- | The order to sort the list.
    order :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportSortBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'importSortBy_attribute' - The export field to use for sorting.
--
-- 'order', 'importSortBy_order' - The order to sort the list.
newImportSortBy ::
  -- | 'attribute'
  ImportSortAttribute ->
  -- | 'order'
  SortOrder ->
  ImportSortBy
newImportSortBy pAttribute_ pOrder_ =
  ImportSortBy'
    { attribute = pAttribute_,
      order = pOrder_
    }

-- | The export field to use for sorting.
importSortBy_attribute :: Lens.Lens' ImportSortBy ImportSortAttribute
importSortBy_attribute = Lens.lens (\ImportSortBy' {attribute} -> attribute) (\s@ImportSortBy' {} a -> s {attribute = a} :: ImportSortBy)

-- | The order to sort the list.
importSortBy_order :: Lens.Lens' ImportSortBy SortOrder
importSortBy_order = Lens.lens (\ImportSortBy' {order} -> order) (\s@ImportSortBy' {} a -> s {order = a} :: ImportSortBy)

instance Prelude.Hashable ImportSortBy where
  hashWithSalt _salt ImportSortBy' {..} =
    _salt
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` order

instance Prelude.NFData ImportSortBy where
  rnf ImportSortBy' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf order

instance Data.ToJSON ImportSortBy where
  toJSON ImportSortBy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("attribute" Data..= attribute),
            Prelude.Just ("order" Data..= order)
          ]
      )
