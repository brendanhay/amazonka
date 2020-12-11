-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.LocalIPDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.LocalIPDetails
  ( LocalIPDetails (..),

    -- * Smart constructor
    mkLocalIPDetails,

    -- * Lenses
    lidIPAddressV4,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the local IP address of the connection.
--
-- /See:/ 'mkLocalIPDetails' smart constructor.
newtype LocalIPDetails = LocalIPDetails'
  { ipAddressV4 ::
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

-- | Creates a value of 'LocalIPDetails' with the minimum fields required to make a request.
--
-- * 'ipAddressV4' - The IPv4 local address of the connection.
mkLocalIPDetails ::
  LocalIPDetails
mkLocalIPDetails = LocalIPDetails' {ipAddressV4 = Lude.Nothing}

-- | The IPv4 local address of the connection.
--
-- /Note:/ Consider using 'ipAddressV4' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidIPAddressV4 :: Lens.Lens' LocalIPDetails (Lude.Maybe Lude.Text)
lidIPAddressV4 = Lens.lens (ipAddressV4 :: LocalIPDetails -> Lude.Maybe Lude.Text) (\s a -> s {ipAddressV4 = a} :: LocalIPDetails)
{-# DEPRECATED lidIPAddressV4 "Use generic-lens or generic-optics with 'ipAddressV4' instead." #-}

instance Lude.FromJSON LocalIPDetails where
  parseJSON =
    Lude.withObject
      "LocalIPDetails"
      (\x -> LocalIPDetails' Lude.<$> (x Lude..:? "ipAddressV4"))
