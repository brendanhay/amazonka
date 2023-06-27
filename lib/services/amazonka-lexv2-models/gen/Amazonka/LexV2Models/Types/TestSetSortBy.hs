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
-- Module      : Amazonka.LexV2Models.Types.TestSetSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestSetSortBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.SortOrder
import Amazonka.LexV2Models.Types.TestSetSortAttribute
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the methods by which to sort the test set.
--
-- /See:/ 'newTestSetSortBy' smart constructor.
data TestSetSortBy = TestSetSortBy'
  { -- | Specifies whether to sort the test sets by name or by the time they were
    -- last updated.
    attribute :: TestSetSortAttribute,
    -- | Specifies whether to sort in ascending or descending order.
    order :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestSetSortBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'testSetSortBy_attribute' - Specifies whether to sort the test sets by name or by the time they were
-- last updated.
--
-- 'order', 'testSetSortBy_order' - Specifies whether to sort in ascending or descending order.
newTestSetSortBy ::
  -- | 'attribute'
  TestSetSortAttribute ->
  -- | 'order'
  SortOrder ->
  TestSetSortBy
newTestSetSortBy pAttribute_ pOrder_ =
  TestSetSortBy'
    { attribute = pAttribute_,
      order = pOrder_
    }

-- | Specifies whether to sort the test sets by name or by the time they were
-- last updated.
testSetSortBy_attribute :: Lens.Lens' TestSetSortBy TestSetSortAttribute
testSetSortBy_attribute = Lens.lens (\TestSetSortBy' {attribute} -> attribute) (\s@TestSetSortBy' {} a -> s {attribute = a} :: TestSetSortBy)

-- | Specifies whether to sort in ascending or descending order.
testSetSortBy_order :: Lens.Lens' TestSetSortBy SortOrder
testSetSortBy_order = Lens.lens (\TestSetSortBy' {order} -> order) (\s@TestSetSortBy' {} a -> s {order = a} :: TestSetSortBy)

instance Prelude.Hashable TestSetSortBy where
  hashWithSalt _salt TestSetSortBy' {..} =
    _salt
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` order

instance Prelude.NFData TestSetSortBy where
  rnf TestSetSortBy' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf order

instance Data.ToJSON TestSetSortBy where
  toJSON TestSetSortBy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("attribute" Data..= attribute),
            Prelude.Just ("order" Data..= order)
          ]
      )
