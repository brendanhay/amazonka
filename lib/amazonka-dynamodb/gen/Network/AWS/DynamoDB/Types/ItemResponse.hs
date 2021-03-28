{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ItemResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ItemResponse
  ( ItemResponse (..)
  -- * Smart constructor
  , mkItemResponse
  -- * Lenses
  , irItem
  ) where

import qualified Network.AWS.DynamoDB.Types.AttributeName as Types
import qualified Network.AWS.DynamoDB.Types.AttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details for the requested item.
--
-- /See:/ 'mkItemResponse' smart constructor.
newtype ItemResponse = ItemResponse'
  { item :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue)
    -- ^ Map of attribute data consisting of the data type and attribute value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ItemResponse' value with any optional fields omitted.
mkItemResponse
    :: ItemResponse
mkItemResponse = ItemResponse'{item = Core.Nothing}

-- | Map of attribute data consisting of the data type and attribute value.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irItem :: Lens.Lens' ItemResponse (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
irItem = Lens.field @"item"
{-# INLINEABLE irItem #-}
{-# DEPRECATED item "Use generic-lens or generic-optics with 'item' instead"  #-}

instance Core.FromJSON ItemResponse where
        parseJSON
          = Core.withObject "ItemResponse" Core.$
              \ x -> ItemResponse' Core.<$> (x Core..:? "Item")
