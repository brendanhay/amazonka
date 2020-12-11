-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceIPv6AddressRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceIPv6AddressRequest
  ( InstanceIPv6AddressRequest (..),

    -- * Smart constructor
    mkInstanceIPv6AddressRequest,

    -- * Lenses
    iiarIPv6Address,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an IPv6 address.
--
-- /See:/ 'mkInstanceIPv6AddressRequest' smart constructor.
newtype InstanceIPv6AddressRequest = InstanceIPv6AddressRequest'
  { ipv6Address ::
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

-- | Creates a value of 'InstanceIPv6AddressRequest' with the minimum fields required to make a request.
--
-- * 'ipv6Address' - The IPv6 address.
mkInstanceIPv6AddressRequest ::
  InstanceIPv6AddressRequest
mkInstanceIPv6AddressRequest =
  InstanceIPv6AddressRequest' {ipv6Address = Lude.Nothing}

-- | The IPv6 address.
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiarIPv6Address :: Lens.Lens' InstanceIPv6AddressRequest (Lude.Maybe Lude.Text)
iiarIPv6Address = Lens.lens (ipv6Address :: InstanceIPv6AddressRequest -> Lude.Maybe Lude.Text) (\s a -> s {ipv6Address = a} :: InstanceIPv6AddressRequest)
{-# DEPRECATED iiarIPv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead." #-}

instance Lude.ToQuery InstanceIPv6AddressRequest where
  toQuery InstanceIPv6AddressRequest' {..} =
    Lude.mconcat ["Ipv6Address" Lude.=: ipv6Address]
