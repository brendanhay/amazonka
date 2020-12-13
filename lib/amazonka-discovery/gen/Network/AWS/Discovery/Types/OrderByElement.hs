{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.OrderByElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.OrderByElement
  ( OrderByElement (..),

    -- * Smart constructor
    mkOrderByElement,

    -- * Lenses
    obeSortOrder,
    obeFieldName,
  )
where

import Network.AWS.Discovery.Types.OrderString
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A field and direction for ordered output.
--
-- /See:/ 'mkOrderByElement' smart constructor.
data OrderByElement = OrderByElement'
  { -- | Ordering direction.
    sortOrder :: Lude.Maybe OrderString,
    -- | The field on which to order.
    fieldName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrderByElement' with the minimum fields required to make a request.
--
-- * 'sortOrder' - Ordering direction.
-- * 'fieldName' - The field on which to order.
mkOrderByElement ::
  -- | 'fieldName'
  Lude.Text ->
  OrderByElement
mkOrderByElement pFieldName_ =
  OrderByElement'
    { sortOrder = Lude.Nothing,
      fieldName = pFieldName_
    }

-- | Ordering direction.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
obeSortOrder :: Lens.Lens' OrderByElement (Lude.Maybe OrderString)
obeSortOrder = Lens.lens (sortOrder :: OrderByElement -> Lude.Maybe OrderString) (\s a -> s {sortOrder = a} :: OrderByElement)
{-# DEPRECATED obeSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The field on which to order.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
obeFieldName :: Lens.Lens' OrderByElement Lude.Text
obeFieldName = Lens.lens (fieldName :: OrderByElement -> Lude.Text) (\s a -> s {fieldName = a} :: OrderByElement)
{-# DEPRECATED obeFieldName "Use generic-lens or generic-optics with 'fieldName' instead." #-}

instance Lude.ToJSON OrderByElement where
  toJSON OrderByElement' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sortOrder" Lude..=) Lude.<$> sortOrder,
            Lude.Just ("fieldName" Lude..= fieldName)
          ]
      )
