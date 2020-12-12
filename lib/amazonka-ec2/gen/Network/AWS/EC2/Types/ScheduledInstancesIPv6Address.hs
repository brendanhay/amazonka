{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesIPv6Address
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesIPv6Address
  ( ScheduledInstancesIPv6Address (..),

    -- * Smart constructor
    mkScheduledInstancesIPv6Address,

    -- * Lenses
    siiaIPv6Address,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an IPv6 address.
--
-- /See:/ 'mkScheduledInstancesIPv6Address' smart constructor.
newtype ScheduledInstancesIPv6Address = ScheduledInstancesIPv6Address'
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

-- | Creates a value of 'ScheduledInstancesIPv6Address' with the minimum fields required to make a request.
--
-- * 'ipv6Address' - The IPv6 address.
mkScheduledInstancesIPv6Address ::
  ScheduledInstancesIPv6Address
mkScheduledInstancesIPv6Address =
  ScheduledInstancesIPv6Address' {ipv6Address = Lude.Nothing}

-- | The IPv6 address.
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siiaIPv6Address :: Lens.Lens' ScheduledInstancesIPv6Address (Lude.Maybe Lude.Text)
siiaIPv6Address = Lens.lens (ipv6Address :: ScheduledInstancesIPv6Address -> Lude.Maybe Lude.Text) (\s a -> s {ipv6Address = a} :: ScheduledInstancesIPv6Address)
{-# DEPRECATED siiaIPv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead." #-}

instance Lude.ToQuery ScheduledInstancesIPv6Address where
  toQuery ScheduledInstancesIPv6Address' {..} =
    Lude.mconcat ["Ipv6Address" Lude.=: ipv6Address]
