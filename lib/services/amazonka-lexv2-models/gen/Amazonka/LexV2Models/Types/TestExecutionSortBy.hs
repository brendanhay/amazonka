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
-- Module      : Amazonka.LexV2Models.Types.TestExecutionSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestExecutionSortBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.SortOrder
import Amazonka.LexV2Models.Types.TestExecutionSortAttribute
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the method by which to sort the instances of
-- test executions you have carried out.
--
-- /See:/ 'newTestExecutionSortBy' smart constructor.
data TestExecutionSortBy = TestExecutionSortBy'
  { -- | Specifies whether to sort the test set executions by the date and time
    -- at which the test sets were created.
    attribute :: TestExecutionSortAttribute,
    -- | Specifies whether to sort in ascending or descending order.
    order :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestExecutionSortBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'testExecutionSortBy_attribute' - Specifies whether to sort the test set executions by the date and time
-- at which the test sets were created.
--
-- 'order', 'testExecutionSortBy_order' - Specifies whether to sort in ascending or descending order.
newTestExecutionSortBy ::
  -- | 'attribute'
  TestExecutionSortAttribute ->
  -- | 'order'
  SortOrder ->
  TestExecutionSortBy
newTestExecutionSortBy pAttribute_ pOrder_ =
  TestExecutionSortBy'
    { attribute = pAttribute_,
      order = pOrder_
    }

-- | Specifies whether to sort the test set executions by the date and time
-- at which the test sets were created.
testExecutionSortBy_attribute :: Lens.Lens' TestExecutionSortBy TestExecutionSortAttribute
testExecutionSortBy_attribute = Lens.lens (\TestExecutionSortBy' {attribute} -> attribute) (\s@TestExecutionSortBy' {} a -> s {attribute = a} :: TestExecutionSortBy)

-- | Specifies whether to sort in ascending or descending order.
testExecutionSortBy_order :: Lens.Lens' TestExecutionSortBy SortOrder
testExecutionSortBy_order = Lens.lens (\TestExecutionSortBy' {order} -> order) (\s@TestExecutionSortBy' {} a -> s {order = a} :: TestExecutionSortBy)

instance Prelude.Hashable TestExecutionSortBy where
  hashWithSalt _salt TestExecutionSortBy' {..} =
    _salt
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` order

instance Prelude.NFData TestExecutionSortBy where
  rnf TestExecutionSortBy' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf order

instance Data.ToJSON TestExecutionSortBy where
  toJSON TestExecutionSortBy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("attribute" Data..= attribute),
            Prelude.Just ("order" Data..= order)
          ]
      )
