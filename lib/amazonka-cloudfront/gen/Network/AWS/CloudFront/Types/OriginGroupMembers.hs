{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginGroupMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.OriginGroupMembers
  ( OriginGroupMembers (..)
  -- * Smart constructor
  , mkOriginGroupMembers
  -- * Lenses
  , ogmQuantity
  , ogmItems
  ) where

import qualified Network.AWS.CloudFront.Types.OriginGroupMember as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex data type for the origins included in an origin group.
--
-- /See:/ 'mkOriginGroupMembers' smart constructor.
data OriginGroupMembers = OriginGroupMembers'
  { quantity :: Core.Int
    -- ^ The number of origins in an origin group.
  , items :: Core.NonEmpty Types.OriginGroupMember
    -- ^ Items (origins) in an origin group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OriginGroupMembers' value with any optional fields omitted.
mkOriginGroupMembers
    :: Core.Int -- ^ 'quantity'
    -> Core.NonEmpty Types.OriginGroupMember -- ^ 'items'
    -> OriginGroupMembers
mkOriginGroupMembers quantity items
  = OriginGroupMembers'{quantity, items}

-- | The number of origins in an origin group.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogmQuantity :: Lens.Lens' OriginGroupMembers Core.Int
ogmQuantity = Lens.field @"quantity"
{-# INLINEABLE ogmQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | Items (origins) in an origin group.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogmItems :: Lens.Lens' OriginGroupMembers (Core.NonEmpty Types.OriginGroupMember)
ogmItems = Lens.field @"items"
{-# INLINEABLE ogmItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.ToXML OriginGroupMembers where
        toXML OriginGroupMembers{..}
          = Core.toXMLElement "Quantity" quantity Core.<>
              Core.toXMLElement "Items"
                (Core.toXMLList "OriginGroupMember" items)

instance Core.FromXML OriginGroupMembers where
        parseXML x
          = OriginGroupMembers' Core.<$>
              (x Core..@ "Quantity") Core.<*>
                x Core..@ "Items" Core..<@>
                  Core.parseXMLNonEmpty "OriginGroupMember"
