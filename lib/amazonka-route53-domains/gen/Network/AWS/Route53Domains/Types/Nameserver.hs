{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.Nameserver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.Nameserver
  ( Nameserver (..),

    -- * Smart constructor
    mkNameserver,

    -- * Lenses
    nName,
    nGlueIps,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53Domains.Types.GlueIp as Types
import qualified Network.AWS.Route53Domains.Types.HostName as Types

-- | Nameserver includes the following elements.
--
-- /See:/ 'mkNameserver' smart constructor.
data Nameserver = Nameserver'
  { -- | The fully qualified host name of the name server.
    --
    -- Constraint: Maximum 255 characters
    name :: Types.HostName,
    -- | Glue IP address of a name server entry. Glue IP addresses are required only when the name of the name server is a subdomain of the domain. For example, if your domain is example.com and the name server for the domain is ns.example.com, you need to specify the IP address for ns.example.com.
    --
    -- Constraints: The list can contain only one IPv4 and one IPv6 address.
    glueIps :: Core.Maybe [Types.GlueIp]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Nameserver' value with any optional fields omitted.
mkNameserver ::
  -- | 'name'
  Types.HostName ->
  Nameserver
mkNameserver name = Nameserver' {name, glueIps = Core.Nothing}

-- | The fully qualified host name of the name server.
--
-- Constraint: Maximum 255 characters
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nName :: Lens.Lens' Nameserver Types.HostName
nName = Lens.field @"name"
{-# DEPRECATED nName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Glue IP address of a name server entry. Glue IP addresses are required only when the name of the name server is a subdomain of the domain. For example, if your domain is example.com and the name server for the domain is ns.example.com, you need to specify the IP address for ns.example.com.
--
-- Constraints: The list can contain only one IPv4 and one IPv6 address.
--
-- /Note:/ Consider using 'glueIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nGlueIps :: Lens.Lens' Nameserver (Core.Maybe [Types.GlueIp])
nGlueIps = Lens.field @"glueIps"
{-# DEPRECATED nGlueIps "Use generic-lens or generic-optics with 'glueIps' instead." #-}

instance Core.FromJSON Nameserver where
  toJSON Nameserver {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("GlueIps" Core..=) Core.<$> glueIps
          ]
      )

instance Core.FromJSON Nameserver where
  parseJSON =
    Core.withObject "Nameserver" Core.$
      \x ->
        Nameserver'
          Core.<$> (x Core..: "Name") Core.<*> (x Core..:? "GlueIps")
