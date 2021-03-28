{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.OrderByElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.OrderByElement
  ( OrderByElement (..)
  -- * Smart constructor
  , mkOrderByElement
  -- * Lenses
  , obeFieldName
  , obeSortOrder
  ) where

import qualified Network.AWS.Discovery.Types.OrderString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A field and direction for ordered output.
--
-- /See:/ 'mkOrderByElement' smart constructor.
data OrderByElement = OrderByElement'
  { fieldName :: Core.Text
    -- ^ The field on which to order.
  , sortOrder :: Core.Maybe Types.OrderString
    -- ^ Ordering direction.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrderByElement' value with any optional fields omitted.
mkOrderByElement
    :: Core.Text -- ^ 'fieldName'
    -> OrderByElement
mkOrderByElement fieldName
  = OrderByElement'{fieldName, sortOrder = Core.Nothing}

-- | The field on which to order.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
obeFieldName :: Lens.Lens' OrderByElement Core.Text
obeFieldName = Lens.field @"fieldName"
{-# INLINEABLE obeFieldName #-}
{-# DEPRECATED fieldName "Use generic-lens or generic-optics with 'fieldName' instead"  #-}

-- | Ordering direction.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
obeSortOrder :: Lens.Lens' OrderByElement (Core.Maybe Types.OrderString)
obeSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE obeSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.FromJSON OrderByElement where
        toJSON OrderByElement{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("fieldName" Core..= fieldName),
                  ("sortOrder" Core..=) Core.<$> sortOrder])
