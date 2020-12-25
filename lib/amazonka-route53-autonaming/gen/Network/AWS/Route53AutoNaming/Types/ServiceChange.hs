{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.ServiceChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.ServiceChange
  ( ServiceChange (..),

    -- * Smart constructor
    mkServiceChange,

    -- * Lenses
    scDescription,
    scDnsConfig,
    scHealthCheckConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.DnsConfigChange as Types
import qualified Network.AWS.Route53AutoNaming.Types.HealthCheckConfig as Types
import qualified Network.AWS.Route53AutoNaming.Types.ResourceDescription as Types

-- | A complex type that contains changes to an existing service.
--
-- /See:/ 'mkServiceChange' smart constructor.
data ServiceChange = ServiceChange'
  { -- | A description for the service.
    description :: Core.Maybe Types.ResourceDescription,
    -- | A complex type that contains information about the Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
    dnsConfig :: Core.Maybe Types.DnsConfigChange,
    healthCheckConfig :: Core.Maybe Types.HealthCheckConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceChange' value with any optional fields omitted.
mkServiceChange ::
  ServiceChange
mkServiceChange =
  ServiceChange'
    { description = Core.Nothing,
      dnsConfig = Core.Nothing,
      healthCheckConfig = Core.Nothing
    }

-- | A description for the service.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDescription :: Lens.Lens' ServiceChange (Core.Maybe Types.ResourceDescription)
scDescription = Lens.field @"description"
{-# DEPRECATED scDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A complex type that contains information about the Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
--
-- /Note:/ Consider using 'dnsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDnsConfig :: Lens.Lens' ServiceChange (Core.Maybe Types.DnsConfigChange)
scDnsConfig = Lens.field @"dnsConfig"
{-# DEPRECATED scDnsConfig "Use generic-lens or generic-optics with 'dnsConfig' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'healthCheckConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scHealthCheckConfig :: Lens.Lens' ServiceChange (Core.Maybe Types.HealthCheckConfig)
scHealthCheckConfig = Lens.field @"healthCheckConfig"
{-# DEPRECATED scHealthCheckConfig "Use generic-lens or generic-optics with 'healthCheckConfig' instead." #-}

instance Core.FromJSON ServiceChange where
  toJSON ServiceChange {..} =
    Core.object
      ( Core.catMaybes
          [ ("Description" Core..=) Core.<$> description,
            ("DnsConfig" Core..=) Core.<$> dnsConfig,
            ("HealthCheckConfig" Core..=) Core.<$> healthCheckConfig
          ]
      )
