{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesPrivateIPAddressConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesPrivateIPAddressConfig
  ( ScheduledInstancesPrivateIPAddressConfig (..),

    -- * Smart constructor
    mkScheduledInstancesPrivateIPAddressConfig,

    -- * Lenses
    sipiacPrimary,
    sipiacPrivateIPAddress,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a private IPv4 address for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstancesPrivateIPAddressConfig' smart constructor.
data ScheduledInstancesPrivateIPAddressConfig = ScheduledInstancesPrivateIPAddressConfig'
  { -- | Indicates whether this is a primary IPv4 address. Otherwise, this is a secondary IPv4 address.
    primary :: Lude.Maybe Lude.Bool,
    -- | The IPv4 address.
    privateIPAddress :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduledInstancesPrivateIPAddressConfig' with the minimum fields required to make a request.
--
-- * 'primary' - Indicates whether this is a primary IPv4 address. Otherwise, this is a secondary IPv4 address.
-- * 'privateIPAddress' - The IPv4 address.
mkScheduledInstancesPrivateIPAddressConfig ::
  ScheduledInstancesPrivateIPAddressConfig
mkScheduledInstancesPrivateIPAddressConfig =
  ScheduledInstancesPrivateIPAddressConfig'
    { primary = Lude.Nothing,
      privateIPAddress = Lude.Nothing
    }

-- | Indicates whether this is a primary IPv4 address. Otherwise, this is a secondary IPv4 address.
--
-- /Note:/ Consider using 'primary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipiacPrimary :: Lens.Lens' ScheduledInstancesPrivateIPAddressConfig (Lude.Maybe Lude.Bool)
sipiacPrimary = Lens.lens (primary :: ScheduledInstancesPrivateIPAddressConfig -> Lude.Maybe Lude.Bool) (\s a -> s {primary = a} :: ScheduledInstancesPrivateIPAddressConfig)
{-# DEPRECATED sipiacPrimary "Use generic-lens or generic-optics with 'primary' instead." #-}

-- | The IPv4 address.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipiacPrivateIPAddress :: Lens.Lens' ScheduledInstancesPrivateIPAddressConfig (Lude.Maybe Lude.Text)
sipiacPrivateIPAddress = Lens.lens (privateIPAddress :: ScheduledInstancesPrivateIPAddressConfig -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: ScheduledInstancesPrivateIPAddressConfig)
{-# DEPRECATED sipiacPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

instance Lude.ToQuery ScheduledInstancesPrivateIPAddressConfig where
  toQuery ScheduledInstancesPrivateIPAddressConfig' {..} =
    Lude.mconcat
      [ "Primary" Lude.=: primary,
        "PrivateIpAddress" Lude.=: privateIPAddress
      ]
