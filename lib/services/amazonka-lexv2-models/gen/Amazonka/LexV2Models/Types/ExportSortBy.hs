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
-- Module      : Amazonka.LexV2Models.Types.ExportSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ExportSortBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ExportSortAttribute
import Amazonka.LexV2Models.Types.SortOrder
import qualified Amazonka.Prelude as Prelude

-- | Provides information about sorting a list of exports.
--
-- /See:/ 'newExportSortBy' smart constructor.
data ExportSortBy = ExportSortBy'
  { -- | The export field to use for sorting.
    attribute :: ExportSortAttribute,
    -- | The order to sort the list.
    order :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportSortBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'exportSortBy_attribute' - The export field to use for sorting.
--
-- 'order', 'exportSortBy_order' - The order to sort the list.
newExportSortBy ::
  -- | 'attribute'
  ExportSortAttribute ->
  -- | 'order'
  SortOrder ->
  ExportSortBy
newExportSortBy pAttribute_ pOrder_ =
  ExportSortBy'
    { attribute = pAttribute_,
      order = pOrder_
    }

-- | The export field to use for sorting.
exportSortBy_attribute :: Lens.Lens' ExportSortBy ExportSortAttribute
exportSortBy_attribute = Lens.lens (\ExportSortBy' {attribute} -> attribute) (\s@ExportSortBy' {} a -> s {attribute = a} :: ExportSortBy)

-- | The order to sort the list.
exportSortBy_order :: Lens.Lens' ExportSortBy SortOrder
exportSortBy_order = Lens.lens (\ExportSortBy' {order} -> order) (\s@ExportSortBy' {} a -> s {order = a} :: ExportSortBy)

instance Prelude.Hashable ExportSortBy where
  hashWithSalt _salt ExportSortBy' {..} =
    _salt
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` order

instance Prelude.NFData ExportSortBy where
  rnf ExportSortBy' {..} =
    Prelude.rnf attribute `Prelude.seq`
      Prelude.rnf order

instance Data.ToJSON ExportSortBy where
  toJSON ExportSortBy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("attribute" Data..= attribute),
            Prelude.Just ("order" Data..= order)
          ]
      )
