{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.NamespaceProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.NamespaceProperties
  ( NamespaceProperties (..),

    -- * Smart constructor
    mkNamespaceProperties,

    -- * Lenses
    npDnsProperties,
    npHttpProperties,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.DnsProperties as Types
import qualified Network.AWS.Route53AutoNaming.Types.HttpProperties as Types

-- | A complex type that contains information that is specific to the namespace type.
--
-- /See:/ 'mkNamespaceProperties' smart constructor.
data NamespaceProperties = NamespaceProperties'
  { -- | A complex type that contains the ID for the Route 53 hosted zone that AWS Cloud Map creates when you create a namespace.
    dnsProperties :: Core.Maybe Types.DnsProperties,
    -- | A complex type that contains the name of an HTTP namespace.
    httpProperties :: Core.Maybe Types.HttpProperties
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NamespaceProperties' value with any optional fields omitted.
mkNamespaceProperties ::
  NamespaceProperties
mkNamespaceProperties =
  NamespaceProperties'
    { dnsProperties = Core.Nothing,
      httpProperties = Core.Nothing
    }

-- | A complex type that contains the ID for the Route 53 hosted zone that AWS Cloud Map creates when you create a namespace.
--
-- /Note:/ Consider using 'dnsProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npDnsProperties :: Lens.Lens' NamespaceProperties (Core.Maybe Types.DnsProperties)
npDnsProperties = Lens.field @"dnsProperties"
{-# DEPRECATED npDnsProperties "Use generic-lens or generic-optics with 'dnsProperties' instead." #-}

-- | A complex type that contains the name of an HTTP namespace.
--
-- /Note:/ Consider using 'httpProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npHttpProperties :: Lens.Lens' NamespaceProperties (Core.Maybe Types.HttpProperties)
npHttpProperties = Lens.field @"httpProperties"
{-# DEPRECATED npHttpProperties "Use generic-lens or generic-optics with 'httpProperties' instead." #-}

instance Core.FromJSON NamespaceProperties where
  parseJSON =
    Core.withObject "NamespaceProperties" Core.$
      \x ->
        NamespaceProperties'
          Core.<$> (x Core..:? "DnsProperties") Core.<*> (x Core..:? "HttpProperties")
