{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TransactWriteItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.TransactWriteItem
  ( TransactWriteItem (..)
  -- * Smart constructor
  , mkTransactWriteItem
  -- * Lenses
  , twiConditionCheck
  , twiDelete
  , twiPut
  , twiUpdate
  ) where

import qualified Network.AWS.DynamoDB.Types.ConditionCheck as Types
import qualified Network.AWS.DynamoDB.Types.Delete as Types
import qualified Network.AWS.DynamoDB.Types.Put as Types
import qualified Network.AWS.DynamoDB.Types.Update as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of requests that can perform update, put, delete, or check operations on multiple items in one or more tables atomically.
--
-- /See:/ 'mkTransactWriteItem' smart constructor.
data TransactWriteItem = TransactWriteItem'
  { conditionCheck :: Core.Maybe Types.ConditionCheck
    -- ^ A request to perform a check item operation.
  , delete :: Core.Maybe Types.Delete
    -- ^ A request to perform a @DeleteItem@ operation.
  , put :: Core.Maybe Types.Put
    -- ^ A request to perform a @PutItem@ operation.
  , update :: Core.Maybe Types.Update
    -- ^ A request to perform an @UpdateItem@ operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransactWriteItem' value with any optional fields omitted.
mkTransactWriteItem
    :: TransactWriteItem
mkTransactWriteItem
  = TransactWriteItem'{conditionCheck = Core.Nothing,
                       delete = Core.Nothing, put = Core.Nothing, update = Core.Nothing}

-- | A request to perform a check item operation.
--
-- /Note:/ Consider using 'conditionCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twiConditionCheck :: Lens.Lens' TransactWriteItem (Core.Maybe Types.ConditionCheck)
twiConditionCheck = Lens.field @"conditionCheck"
{-# INLINEABLE twiConditionCheck #-}
{-# DEPRECATED conditionCheck "Use generic-lens or generic-optics with 'conditionCheck' instead"  #-}

-- | A request to perform a @DeleteItem@ operation.
--
-- /Note:/ Consider using 'delete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twiDelete :: Lens.Lens' TransactWriteItem (Core.Maybe Types.Delete)
twiDelete = Lens.field @"delete"
{-# INLINEABLE twiDelete #-}
{-# DEPRECATED delete "Use generic-lens or generic-optics with 'delete' instead"  #-}

-- | A request to perform a @PutItem@ operation.
--
-- /Note:/ Consider using 'put' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twiPut :: Lens.Lens' TransactWriteItem (Core.Maybe Types.Put)
twiPut = Lens.field @"put"
{-# INLINEABLE twiPut #-}
{-# DEPRECATED put "Use generic-lens or generic-optics with 'put' instead"  #-}

-- | A request to perform an @UpdateItem@ operation.
--
-- /Note:/ Consider using 'update' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twiUpdate :: Lens.Lens' TransactWriteItem (Core.Maybe Types.Update)
twiUpdate = Lens.field @"update"
{-# INLINEABLE twiUpdate #-}
{-# DEPRECATED update "Use generic-lens or generic-optics with 'update' instead"  #-}

instance Core.FromJSON TransactWriteItem where
        toJSON TransactWriteItem{..}
          = Core.object
              (Core.catMaybes
                 [("ConditionCheck" Core..=) Core.<$> conditionCheck,
                  ("Delete" Core..=) Core.<$> delete, ("Put" Core..=) Core.<$> put,
                  ("Update" Core..=) Core.<$> update])
