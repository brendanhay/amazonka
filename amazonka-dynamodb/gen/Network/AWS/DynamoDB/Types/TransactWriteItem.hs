{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DynamoDB.Types.ConditionCheck
import Network.AWS.DynamoDB.Types.Delete
import Network.AWS.DynamoDB.Types.Put
import Network.AWS.DynamoDB.Types.Update
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of requests that can perform update, put, delete, or check
-- operations on multiple items in one or more tables atomically.
--
-- /See:/ 'newTransactWriteItem' smart constructor.
data TransactWriteItem = TransactWriteItem'
  { -- | A request to perform a @PutItem@ operation.
    put :: Prelude.Maybe Put,
    -- | A request to perform a check item operation.
    conditionCheck :: Prelude.Maybe ConditionCheck,
    -- | A request to perform an @UpdateItem@ operation.
    update :: Prelude.Maybe Update,
    -- | A request to perform a @DeleteItem@ operation.
    delete' :: Prelude.Maybe Delete
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { put = Prelude.Nothing,
      conditionCheck = Prelude.Nothing,
      update = Prelude.Nothing,
      delete' = Prelude.Nothing
    }

-- | A request to perform a @PutItem@ operation.
transactWriteItem_put :: Lens.Lens' TransactWriteItem (Prelude.Maybe Put)
transactWriteItem_put = Lens.lens (\TransactWriteItem' {put} -> put) (\s@TransactWriteItem' {} a -> s {put = a} :: TransactWriteItem)

-- | A request to perform a check item operation.
transactWriteItem_conditionCheck :: Lens.Lens' TransactWriteItem (Prelude.Maybe ConditionCheck)
transactWriteItem_conditionCheck = Lens.lens (\TransactWriteItem' {conditionCheck} -> conditionCheck) (\s@TransactWriteItem' {} a -> s {conditionCheck = a} :: TransactWriteItem)

-- | A request to perform an @UpdateItem@ operation.
transactWriteItem_update :: Lens.Lens' TransactWriteItem (Prelude.Maybe Update)
transactWriteItem_update = Lens.lens (\TransactWriteItem' {update} -> update) (\s@TransactWriteItem' {} a -> s {update = a} :: TransactWriteItem)

-- | A request to perform a @DeleteItem@ operation.
transactWriteItem_delete :: Lens.Lens' TransactWriteItem (Prelude.Maybe Delete)
transactWriteItem_delete = Lens.lens (\TransactWriteItem' {delete'} -> delete') (\s@TransactWriteItem' {} a -> s {delete' = a} :: TransactWriteItem)

instance Prelude.Hashable TransactWriteItem

instance Prelude.NFData TransactWriteItem

instance Prelude.ToJSON TransactWriteItem where
  toJSON TransactWriteItem' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Put" Prelude..=) Prelude.<$> put,
            ("ConditionCheck" Prelude..=)
              Prelude.<$> conditionCheck,
            ("Update" Prelude..=) Prelude.<$> update,
            ("Delete" Prelude..=) Prelude.<$> delete'
          ]
      )
