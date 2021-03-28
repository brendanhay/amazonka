{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginSslProtocols
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.OriginSslProtocols
  ( OriginSslProtocols (..)
  -- * Smart constructor
  , mkOriginSslProtocols
  -- * Lenses
  , ospQuantity
  , ospItems
  ) where

import qualified Network.AWS.CloudFront.Types.SslProtocol as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that contains information about the SSL/TLS protocols that CloudFront can use when establishing an HTTPS connection with your origin. 
--
-- /See:/ 'mkOriginSslProtocols' smart constructor.
data OriginSslProtocols = OriginSslProtocols'
  { quantity :: Core.Int
    -- ^ The number of SSL/TLS protocols that you want to allow CloudFront to use when establishing an HTTPS connection with this origin. 
  , items :: [Types.SslProtocol]
    -- ^ A list that contains allowed SSL/TLS protocols for this distribution.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OriginSslProtocols' value with any optional fields omitted.
mkOriginSslProtocols
    :: Core.Int -- ^ 'quantity'
    -> OriginSslProtocols
mkOriginSslProtocols quantity
  = OriginSslProtocols'{quantity, items = Core.mempty}

-- | The number of SSL/TLS protocols that you want to allow CloudFront to use when establishing an HTTPS connection with this origin. 
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ospQuantity :: Lens.Lens' OriginSslProtocols Core.Int
ospQuantity = Lens.field @"quantity"
{-# INLINEABLE ospQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | A list that contains allowed SSL/TLS protocols for this distribution.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ospItems :: Lens.Lens' OriginSslProtocols [Types.SslProtocol]
ospItems = Lens.field @"items"
{-# INLINEABLE ospItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.ToXML OriginSslProtocols where
        toXML OriginSslProtocols{..}
          = Core.toXMLElement "Quantity" quantity Core.<>
              Core.toXMLElement "Items" (Core.toXMLList "SslProtocol" items)

instance Core.FromXML OriginSslProtocols where
        parseXML x
          = OriginSslProtocols' Core.<$>
              (x Core..@ "Quantity") Core.<*>
                x Core..@ "Items" Core..@! Core.mempty Core..<@>
                  Core.parseXMLList "SslProtocol"
