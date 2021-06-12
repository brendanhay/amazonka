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
-- Module      : Network.AWS.DynamoDB.Types.TransactWriteItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TransactWriteItem where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.ConditionCheck
import Network.AWS.DynamoDB.Types.Delete
import Network.AWS.DynamoDB.Types.Put
import Network.AWS.DynamoDB.Types.Update
import qualified Network.AWS.Lens as Lens

-- | A list of requests that can perform update, put, delete, or check
-- operations on multiple items in one or more tables atomically.
--
-- /See:/ 'newTransactWriteItem' smart constructor.
data TransactWriteItem = TransactWriteItem'
  { -- | A request to perform a @PutItem@ operation.
    put :: Core.Maybe Put,
    -- | A request to perform a check item operation.
    conditionCheck :: Core.Maybe ConditionCheck,
    -- | A request to perform an @UpdateItem@ operation.
    update :: Core.Maybe Update,
    -- | A request to perform a @DeleteItem@ operation.
    delete' :: Core.Maybe Delete
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransactWriteItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'put', 'transactWriteItem_put' - A request to perform a @PutItem@ operation.
--
-- 'conditionCheck', 'transactWriteItem_conditionCheck' - A request to perform a check item operation.
--
-- 'update', 'transactWriteItem_update' - A request to perform an @UpdateItem@ operation.
--
-- 'delete'', 'transactWriteItem_delete' - A request to perform a @DeleteItem@ operation.
newTransactWriteItem ::
  TransactWriteItem
newTransactWriteItem =
  TransactWriteItem'
    { put = Core.Nothing,
      conditionCheck = Core.Nothing,
      update = Core.Nothing,
      delete' = Core.Nothing
    }

-- | A request to perform a @PutItem@ operation.
transactWriteItem_put :: Lens.Lens' TransactWriteItem (Core.Maybe Put)
transactWriteItem_put = Lens.lens (\TransactWriteItem' {put} -> put) (\s@TransactWriteItem' {} a -> s {put = a} :: TransactWriteItem)

-- | A request to perform a check item operation.
transactWriteItem_conditionCheck :: Lens.Lens' TransactWriteItem (Core.Maybe ConditionCheck)
transactWriteItem_conditionCheck = Lens.lens (\TransactWriteItem' {conditionCheck} -> conditionCheck) (\s@TransactWriteItem' {} a -> s {conditionCheck = a} :: TransactWriteItem)

-- | A request to perform an @UpdateItem@ operation.
transactWriteItem_update :: Lens.Lens' TransactWriteItem (Core.Maybe Update)
transactWriteItem_update = Lens.lens (\TransactWriteItem' {update} -> update) (\s@TransactWriteItem' {} a -> s {update = a} :: TransactWriteItem)

-- | A request to perform a @DeleteItem@ operation.
transactWriteItem_delete :: Lens.Lens' TransactWriteItem (Core.Maybe Delete)
transactWriteItem_delete = Lens.lens (\TransactWriteItem' {delete'} -> delete') (\s@TransactWriteItem' {} a -> s {delete' = a} :: TransactWriteItem)

instance Core.Hashable TransactWriteItem

instance Core.NFData TransactWriteItem

instance Core.ToJSON TransactWriteItem where
  toJSON TransactWriteItem' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Put" Core..=) Core.<$> put,
            ("ConditionCheck" Core..=) Core.<$> conditionCheck,
            ("Update" Core..=) Core.<$> update,
            ("Delete" Core..=) Core.<$> delete'
          ]
      )
