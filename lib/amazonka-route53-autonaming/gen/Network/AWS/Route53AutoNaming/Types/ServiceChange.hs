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
    scHealthCheckConfig,
    scDNSConfig,
    scDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53AutoNaming.Types.DNSConfigChange
import Network.AWS.Route53AutoNaming.Types.HealthCheckConfig

-- | A complex type that contains changes to an existing service.
--
-- /See:/ 'mkServiceChange' smart constructor.
data ServiceChange = ServiceChange'
  { healthCheckConfig ::
      Lude.Maybe HealthCheckConfig,
    dnsConfig :: Lude.Maybe DNSConfigChange,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceChange' with the minimum fields required to make a request.
--
-- * 'description' - A description for the service.
-- * 'dnsConfig' - A complex type that contains information about the Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
-- * 'healthCheckConfig' - Undocumented field.
mkServiceChange ::
  ServiceChange
mkServiceChange =
  ServiceChange'
    { healthCheckConfig = Lude.Nothing,
      dnsConfig = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'healthCheckConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scHealthCheckConfig :: Lens.Lens' ServiceChange (Lude.Maybe HealthCheckConfig)
scHealthCheckConfig = Lens.lens (healthCheckConfig :: ServiceChange -> Lude.Maybe HealthCheckConfig) (\s a -> s {healthCheckConfig = a} :: ServiceChange)
{-# DEPRECATED scHealthCheckConfig "Use generic-lens or generic-optics with 'healthCheckConfig' instead." #-}

-- | A complex type that contains information about the Route 53 DNS records that you want AWS Cloud Map to create when you register an instance.
--
-- /Note:/ Consider using 'dnsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDNSConfig :: Lens.Lens' ServiceChange (Lude.Maybe DNSConfigChange)
scDNSConfig = Lens.lens (dnsConfig :: ServiceChange -> Lude.Maybe DNSConfigChange) (\s a -> s {dnsConfig = a} :: ServiceChange)
{-# DEPRECATED scDNSConfig "Use generic-lens or generic-optics with 'dnsConfig' instead." #-}

-- | A description for the service.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDescription :: Lens.Lens' ServiceChange (Lude.Maybe Lude.Text)
scDescription = Lens.lens (description :: ServiceChange -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ServiceChange)
{-# DEPRECATED scDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.ToJSON ServiceChange where
  toJSON ServiceChange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("HealthCheckConfig" Lude..=) Lude.<$> healthCheckConfig,
            ("DnsConfig" Lude..=) Lude.<$> dnsConfig,
            ("Description" Lude..=) Lude.<$> description
          ]
      )
