{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DNSServersOptionsModifyStructure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DNSServersOptionsModifyStructure
  ( DNSServersOptionsModifyStructure (..),

    -- * Smart constructor
    mkDNSServersOptionsModifyStructure,

    -- * Lenses
    dsomsEnabled,
    dsomsCustomDNSServers,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the DNS server to be used.
--
-- /See:/ 'mkDNSServersOptionsModifyStructure' smart constructor.
data DNSServersOptionsModifyStructure = DNSServersOptionsModifyStructure'
  { -- | Indicates whether DNS servers should be used. Specify @False@ to delete the existing DNS servers.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The IPv4 address range, in CIDR notation, of the DNS servers to be used. You can specify up to two DNS servers. Ensure that the DNS servers can be reached by the clients. The specified values overwrite the existing values.
    customDNSServers :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DNSServersOptionsModifyStructure' with the minimum fields required to make a request.
--
-- * 'enabled' - Indicates whether DNS servers should be used. Specify @False@ to delete the existing DNS servers.
-- * 'customDNSServers' - The IPv4 address range, in CIDR notation, of the DNS servers to be used. You can specify up to two DNS servers. Ensure that the DNS servers can be reached by the clients. The specified values overwrite the existing values.
mkDNSServersOptionsModifyStructure ::
  DNSServersOptionsModifyStructure
mkDNSServersOptionsModifyStructure =
  DNSServersOptionsModifyStructure'
    { enabled = Lude.Nothing,
      customDNSServers = Lude.Nothing
    }

-- | Indicates whether DNS servers should be used. Specify @False@ to delete the existing DNS servers.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsomsEnabled :: Lens.Lens' DNSServersOptionsModifyStructure (Lude.Maybe Lude.Bool)
dsomsEnabled = Lens.lens (enabled :: DNSServersOptionsModifyStructure -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: DNSServersOptionsModifyStructure)
{-# DEPRECATED dsomsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The IPv4 address range, in CIDR notation, of the DNS servers to be used. You can specify up to two DNS servers. Ensure that the DNS servers can be reached by the clients. The specified values overwrite the existing values.
--
-- /Note:/ Consider using 'customDNSServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsomsCustomDNSServers :: Lens.Lens' DNSServersOptionsModifyStructure (Lude.Maybe [Lude.Text])
dsomsCustomDNSServers = Lens.lens (customDNSServers :: DNSServersOptionsModifyStructure -> Lude.Maybe [Lude.Text]) (\s a -> s {customDNSServers = a} :: DNSServersOptionsModifyStructure)
{-# DEPRECATED dsomsCustomDNSServers "Use generic-lens or generic-optics with 'customDNSServers' instead." #-}

instance Lude.ToQuery DNSServersOptionsModifyStructure where
  toQuery DNSServersOptionsModifyStructure' {..} =
    Lude.mconcat
      [ "Enabled" Lude.=: enabled,
        Lude.toQuery
          (Lude.toQueryList "CustomDnsServers" Lude.<$> customDNSServers)
      ]
