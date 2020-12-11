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

import Network.AWS.DynamoDB.Types.Get
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies an item to be retrieved as part of the transaction.
--
-- /See:/ 'mkTransactGetItem' smart constructor.
newtype TransactGetItem = TransactGetItem' {get :: Get}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransactGetItem' with the minimum fields required to make a request.
--
-- * 'get' - Contains the primary key that identifies the item to get, together with the name of the table that contains the item, and optionally the specific attributes of the item to retrieve.
mkTransactGetItem ::
  -- | 'get'
  Get ->
  TransactGetItem
mkTransactGetItem pGet_ = TransactGetItem' {get = pGet_}

-- | Contains the primary key that identifies the item to get, together with the name of the table that contains the item, and optionally the specific attributes of the item to retrieve.
--
-- /Note:/ Consider using 'get' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgiGet :: Lens.Lens' TransactGetItem Get
tgiGet = Lens.lens (get :: TransactGetItem -> Get) (\s a -> s {get = a} :: TransactGetItem)
{-# DEPRECATED tgiGet "Use generic-lens or generic-optics with 'get' instead." #-}

instance Lude.ToJSON TransactGetItem where
  toJSON TransactGetItem' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Get" Lude..= get)])
