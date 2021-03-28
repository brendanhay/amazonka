{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.DnsProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.DnsProperties
  ( DnsProperties (..)
  -- * Smart constructor
  , mkDnsProperties
  -- * Lenses
  , dpHostedZoneId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.ResourceId as Types

-- | A complex type that contains the ID for the Route 53 hosted zone that AWS Cloud Map creates when you create a namespace.
--
-- /See:/ 'mkDnsProperties' smart constructor.
newtype DnsProperties = DnsProperties'
  { hostedZoneId :: Core.Maybe Types.ResourceId
    -- ^ The ID for the Route 53 hosted zone that AWS Cloud Map creates when you create a namespace.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DnsProperties' value with any optional fields omitted.
mkDnsProperties
    :: DnsProperties
mkDnsProperties = DnsProperties'{hostedZoneId = Core.Nothing}

-- | The ID for the Route 53 hosted zone that AWS Cloud Map creates when you create a namespace.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpHostedZoneId :: Lens.Lens' DnsProperties (Core.Maybe Types.ResourceId)
dpHostedZoneId = Lens.field @"hostedZoneId"
{-# INLINEABLE dpHostedZoneId #-}
{-# DEPRECATED hostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead"  #-}

instance Core.FromJSON DnsProperties where
        parseJSON
          = Core.withObject "DnsProperties" Core.$
              \ x -> DnsProperties' Core.<$> (x Core..:? "HostedZoneId")
