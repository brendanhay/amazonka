{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TransactWriteItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TransactWriteItem
  ( TransactWriteItem (..),

    -- * Smart constructor
    mkTransactWriteItem,

    -- * Lenses
    twiConditionCheck,
    twiPut,
    twiDelete,
    twiUpdate,
  )
where

import Network.AWS.DynamoDB.Types.ConditionCheck
import Network.AWS.DynamoDB.Types.Delete
import Network.AWS.DynamoDB.Types.Put
import Network.AWS.DynamoDB.Types.Update
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of requests that can perform update, put, delete, or check operations on multiple items in one or more tables atomically.
--
-- /See:/ 'mkTransactWriteItem' smart constructor.
data TransactWriteItem = TransactWriteItem'
  { -- | A request to perform a check item operation.
    conditionCheck :: Lude.Maybe ConditionCheck,
    -- | A request to perform a @PutItem@ operation.
    put :: Lude.Maybe Put,
    -- | A request to perform a @DeleteItem@ operation.
    delete :: Lude.Maybe Delete,
    -- | A request to perform an @UpdateItem@ operation.
    update :: Lude.Maybe Update
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransactWriteItem' with the minimum fields required to make a request.
--
-- * 'conditionCheck' - A request to perform a check item operation.
-- * 'put' - A request to perform a @PutItem@ operation.
-- * 'delete' - A request to perform a @DeleteItem@ operation.
-- * 'update' - A request to perform an @UpdateItem@ operation.
mkTransactWriteItem ::
  TransactWriteItem
mkTransactWriteItem =
  TransactWriteItem'
    { conditionCheck = Lude.Nothing,
      put = Lude.Nothing,
      delete = Lude.Nothing,
      update = Lude.Nothing
    }

-- | A request to perform a check item operation.
--
-- /Note:/ Consider using 'conditionCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twiConditionCheck :: Lens.Lens' TransactWriteItem (Lude.Maybe ConditionCheck)
twiConditionCheck = Lens.lens (conditionCheck :: TransactWriteItem -> Lude.Maybe ConditionCheck) (\s a -> s {conditionCheck = a} :: TransactWriteItem)
{-# DEPRECATED twiConditionCheck "Use generic-lens or generic-optics with 'conditionCheck' instead." #-}

-- | A request to perform a @PutItem@ operation.
--
-- /Note:/ Consider using 'put' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twiPut :: Lens.Lens' TransactWriteItem (Lude.Maybe Put)
twiPut = Lens.lens (put :: TransactWriteItem -> Lude.Maybe Put) (\s a -> s {put = a} :: TransactWriteItem)
{-# DEPRECATED twiPut "Use generic-lens or generic-optics with 'put' instead." #-}

-- | A request to perform a @DeleteItem@ operation.
--
-- /Note:/ Consider using 'delete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twiDelete :: Lens.Lens' TransactWriteItem (Lude.Maybe Delete)
twiDelete = Lens.lens (delete :: TransactWriteItem -> Lude.Maybe Delete) (\s a -> s {delete = a} :: TransactWriteItem)
{-# DEPRECATED twiDelete "Use generic-lens or generic-optics with 'delete' instead." #-}

-- | A request to perform an @UpdateItem@ operation.
--
-- /Note:/ Consider using 'update' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twiUpdate :: Lens.Lens' TransactWriteItem (Lude.Maybe Update)
twiUpdate = Lens.lens (update :: TransactWriteItem -> Lude.Maybe Update) (\s a -> s {update = a} :: TransactWriteItem)
{-# DEPRECATED twiUpdate "Use generic-lens or generic-optics with 'update' instead." #-}

instance Lude.ToJSON TransactWriteItem where
  toJSON TransactWriteItem' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConditionCheck" Lude..=) Lude.<$> conditionCheck,
            ("Put" Lude..=) Lude.<$> put,
            ("Delete" Lude..=) Lude.<$> delete,
            ("Update" Lude..=) Lude.<$> update
          ]
      )
