{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginSSLProtocols
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginSSLProtocols
  ( OriginSSLProtocols (..),

    -- * Smart constructor
    mkOriginSSLProtocols,

    -- * Lenses
    ospQuantity,
    ospItems,
  )
where

import Network.AWS.CloudFront.Types.SSLProtocol
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that contains information about the SSL/TLS protocols that CloudFront can use when establishing an HTTPS connection with your origin.
--
-- /See:/ 'mkOriginSSLProtocols' smart constructor.
data OriginSSLProtocols = OriginSSLProtocols'
  { quantity :: Lude.Int,
    items :: [SSLProtocol]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OriginSSLProtocols' with the minimum fields required to make a request.
--
-- * 'items' - A list that contains allowed SSL/TLS protocols for this distribution.
-- * 'quantity' - The number of SSL/TLS protocols that you want to allow CloudFront to use when establishing an HTTPS connection with this origin.
mkOriginSSLProtocols ::
  -- | 'quantity'
  Lude.Int ->
  OriginSSLProtocols
mkOriginSSLProtocols pQuantity_ =
  OriginSSLProtocols' {quantity = pQuantity_, items = Lude.mempty}

-- | The number of SSL/TLS protocols that you want to allow CloudFront to use when establishing an HTTPS connection with this origin.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ospQuantity :: Lens.Lens' OriginSSLProtocols Lude.Int
ospQuantity = Lens.lens (quantity :: OriginSSLProtocols -> Lude.Int) (\s a -> s {quantity = a} :: OriginSSLProtocols)
{-# DEPRECATED ospQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A list that contains allowed SSL/TLS protocols for this distribution.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ospItems :: Lens.Lens' OriginSSLProtocols [SSLProtocol]
ospItems = Lens.lens (items :: OriginSSLProtocols -> [SSLProtocol]) (\s a -> s {items = a} :: OriginSSLProtocols)
{-# DEPRECATED ospItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML OriginSSLProtocols where
  parseXML x =
    OriginSSLProtocols'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.parseXMLList "SslProtocol"
               )

instance Lude.ToXML OriginSSLProtocols where
  toXML OriginSSLProtocols' {..} =
    Lude.mconcat
      [ "Quantity" Lude.@= quantity,
        "Items" Lude.@= Lude.toXMLList "SslProtocol" items
      ]
