{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ItemResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ItemResponse
  ( ItemResponse (..),

    -- * Smart constructor
    mkItemResponse,

    -- * Lenses
    iItem,
  )
where

import Network.AWS.DynamoDB.Types.AttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details for the requested item.
--
-- /See:/ 'mkItemResponse' smart constructor.
newtype ItemResponse = ItemResponse'
  { -- | Map of attribute data consisting of the data type and attribute value.
    item :: Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ItemResponse' with the minimum fields required to make a request.
--
-- * 'item' - Map of attribute data consisting of the data type and attribute value.
mkItemResponse ::
  ItemResponse
mkItemResponse = ItemResponse' {item = Lude.Nothing}

-- | Map of attribute data consisting of the data type and attribute value.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iItem :: Lens.Lens' ItemResponse (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
iItem = Lens.lens (item :: ItemResponse -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {item = a} :: ItemResponse)
{-# DEPRECATED iItem "Use generic-lens or generic-optics with 'item' instead." #-}

instance Lude.FromJSON ItemResponse where
  parseJSON =
    Lude.withObject
      "ItemResponse"
      ( \x ->
          ItemResponse' Lude.<$> (x Lude..:? "Item" Lude..!= Lude.mempty)
      )
