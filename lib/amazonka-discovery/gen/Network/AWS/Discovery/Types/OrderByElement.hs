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
    obeFieldName,
    obeSortOrder,
  )
where

import qualified Network.AWS.Discovery.Types.OrderString as Types
import qualified Network.AWS.Discovery.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A field and direction for ordered output.
--
-- /See:/ 'mkOrderByElement' smart constructor.
data OrderByElement = OrderByElement'
  { -- | The field on which to order.
    fieldName :: Types.String,
    -- | Ordering direction.
    sortOrder :: Core.Maybe Types.OrderString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrderByElement' value with any optional fields omitted.
mkOrderByElement ::
  -- | 'fieldName'
  Types.String ->
  OrderByElement
mkOrderByElement fieldName =
  OrderByElement' {fieldName, sortOrder = Core.Nothing}

-- | The field on which to order.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
obeFieldName :: Lens.Lens' OrderByElement Types.String
obeFieldName = Lens.field @"fieldName"
{-# DEPRECATED obeFieldName "Use generic-lens or generic-optics with 'fieldName' instead." #-}

-- | Ordering direction.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
obeSortOrder :: Lens.Lens' OrderByElement (Core.Maybe Types.OrderString)
obeSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED obeSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON OrderByElement where
  toJSON OrderByElement {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("fieldName" Core..= fieldName),
            ("sortOrder" Core..=) Core.<$> sortOrder
          ]
      )
