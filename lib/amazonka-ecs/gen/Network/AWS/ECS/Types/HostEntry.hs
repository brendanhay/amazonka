{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.HostEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.HostEntry
  ( HostEntry (..),

    -- * Smart constructor
    mkHostEntry,

    -- * Lenses
    heIpAddress,
    heHostname,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Hostnames and IP address entries that are added to the @/etc/hosts@ file of a container via the @extraHosts@ parameter of its 'ContainerDefinition' .
--
-- /See:/ 'mkHostEntry' smart constructor.
data HostEntry = HostEntry'
  { -- | The IP address to use in the @/etc/hosts@ entry.
    ipAddress :: Lude.Text,
    -- | The hostname to use in the @/etc/hosts@ entry.
    hostname :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HostEntry' with the minimum fields required to make a request.
--
-- * 'ipAddress' - The IP address to use in the @/etc/hosts@ entry.
-- * 'hostname' - The hostname to use in the @/etc/hosts@ entry.
mkHostEntry ::
  -- | 'ipAddress'
  Lude.Text ->
  -- | 'hostname'
  Lude.Text ->
  HostEntry
mkHostEntry pIpAddress_ pHostname_ =
  HostEntry' {ipAddress = pIpAddress_, hostname = pHostname_}

-- | The IP address to use in the @/etc/hosts@ entry.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heIpAddress :: Lens.Lens' HostEntry Lude.Text
heIpAddress = Lens.lens (ipAddress :: HostEntry -> Lude.Text) (\s a -> s {ipAddress = a} :: HostEntry)
{-# DEPRECATED heIpAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | The hostname to use in the @/etc/hosts@ entry.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heHostname :: Lens.Lens' HostEntry Lude.Text
heHostname = Lens.lens (hostname :: HostEntry -> Lude.Text) (\s a -> s {hostname = a} :: HostEntry)
{-# DEPRECATED heHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

instance Lude.FromJSON HostEntry where
  parseJSON =
    Lude.withObject
      "HostEntry"
      ( \x ->
          HostEntry'
            Lude.<$> (x Lude..: "ipAddress") Lude.<*> (x Lude..: "hostname")
      )

instance Lude.ToJSON HostEntry where
  toJSON HostEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ipAddress" Lude..= ipAddress),
            Lude.Just ("hostname" Lude..= hostname)
          ]
      )
