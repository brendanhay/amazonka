{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.RouteFilterPrefix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.RouteFilterPrefix
  ( RouteFilterPrefix (..),

    -- * Smart constructor
    mkRouteFilterPrefix,

    -- * Lenses
    rfpCidr,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a route filter prefix that a customer can advertise through Border Gateway Protocol (BGP) over a public virtual interface.
--
-- /See:/ 'mkRouteFilterPrefix' smart constructor.
newtype RouteFilterPrefix = RouteFilterPrefix'
  { cidr ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RouteFilterPrefix' with the minimum fields required to make a request.
--
-- * 'cidr' - The CIDR block for the advertised route. Separate multiple routes using commas. An IPv6 CIDR must use /64 or shorter.
mkRouteFilterPrefix ::
  RouteFilterPrefix
mkRouteFilterPrefix = RouteFilterPrefix' {cidr = Lude.Nothing}

-- | The CIDR block for the advertised route. Separate multiple routes using commas. An IPv6 CIDR must use /64 or shorter.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfpCidr :: Lens.Lens' RouteFilterPrefix (Lude.Maybe Lude.Text)
rfpCidr = Lens.lens (cidr :: RouteFilterPrefix -> Lude.Maybe Lude.Text) (\s a -> s {cidr = a} :: RouteFilterPrefix)
{-# DEPRECATED rfpCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

instance Lude.FromJSON RouteFilterPrefix where
  parseJSON =
    Lude.withObject
      "RouteFilterPrefix"
      (\x -> RouteFilterPrefix' Lude.<$> (x Lude..:? "cidr"))

instance Lude.ToJSON RouteFilterPrefix where
  toJSON RouteFilterPrefix' {..} =
    Lude.object (Lude.catMaybes [("cidr" Lude..=) Lude.<$> cidr])
