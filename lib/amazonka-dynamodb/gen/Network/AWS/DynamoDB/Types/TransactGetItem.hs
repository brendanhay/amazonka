{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TransactGetItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TransactGetItem
  ( TransactGetItem (..),

    -- * Smart constructor
    mkTransactGetItem,

    -- * Lenses
    tgiGet,
  )
where

import qualified Network.AWS.DynamoDB.Types.Get as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies an item to be retrieved as part of the transaction.
--
-- /See:/ 'mkTransactGetItem' smart constructor.
newtype TransactGetItem = TransactGetItem'
  { -- | Contains the primary key that identifies the item to get, together with the name of the table that contains the item, and optionally the specific attributes of the item to retrieve.
    get :: Types.Get
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TransactGetItem' value with any optional fields omitted.
mkTransactGetItem ::
  -- | 'get'
  Types.Get ->
  TransactGetItem
mkTransactGetItem get = TransactGetItem' {get}

-- | Contains the primary key that identifies the item to get, together with the name of the table that contains the item, and optionally the specific attributes of the item to retrieve.
--
-- /Note:/ Consider using 'get' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgiGet :: Lens.Lens' TransactGetItem Types.Get
tgiGet = Lens.field @"get"
{-# DEPRECATED tgiGet "Use generic-lens or generic-optics with 'get' instead." #-}

instance Core.FromJSON TransactGetItem where
  toJSON TransactGetItem {..} =
    Core.object (Core.catMaybes [Core.Just ("Get" Core..= get)])
