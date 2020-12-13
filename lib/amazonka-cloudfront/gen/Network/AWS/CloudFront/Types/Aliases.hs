{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Aliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Aliases
  ( Aliases (..),

    -- * Smart constructor
    mkAliases,

    -- * Lenses
    aQuantity,
    aItems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
--
-- /See:/ 'mkAliases' smart constructor.
data Aliases = Aliases'
  { -- | The number of CNAME aliases, if any, that you want to associate with this distribution.
    quantity :: Lude.Int,
    -- | A complex type that contains the CNAME aliases, if any, that you want to associate with this distribution.
    items :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Aliases' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of CNAME aliases, if any, that you want to associate with this distribution.
-- * 'items' - A complex type that contains the CNAME aliases, if any, that you want to associate with this distribution.
mkAliases ::
  -- | 'quantity'
  Lude.Int ->
  Aliases
mkAliases pQuantity_ =
  Aliases' {quantity = pQuantity_, items = Lude.Nothing}

-- | The number of CNAME aliases, if any, that you want to associate with this distribution.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aQuantity :: Lens.Lens' Aliases Lude.Int
aQuantity = Lens.lens (quantity :: Aliases -> Lude.Int) (\s a -> s {quantity = a} :: Aliases)
{-# DEPRECATED aQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A complex type that contains the CNAME aliases, if any, that you want to associate with this distribution.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aItems :: Lens.Lens' Aliases (Lude.Maybe [Lude.Text])
aItems = Lens.lens (items :: Aliases -> Lude.Maybe [Lude.Text]) (\s a -> s {items = a} :: Aliases)
{-# DEPRECATED aItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML Aliases where
  parseXML x =
    Aliases'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "CNAME")
               )

instance Lude.ToXML Aliases where
  toXML Aliases' {..} =
    Lude.mconcat
      [ "Quantity" Lude.@= quantity,
        "Items" Lude.@= Lude.toXML (Lude.toXMLList "CNAME" Lude.<$> items)
      ]
