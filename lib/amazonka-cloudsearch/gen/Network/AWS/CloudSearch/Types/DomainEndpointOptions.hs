{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DomainEndpointOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DomainEndpointOptions
  ( DomainEndpointOptions (..),

    -- * Smart constructor
    mkDomainEndpointOptions,

    -- * Lenses
    deoEnforceHTTPS,
    deoTLSSecurityPolicy,
  )
where

import qualified Network.AWS.CloudSearch.Types.TLSSecurityPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The domain's endpoint options.
--
-- /See:/ 'mkDomainEndpointOptions' smart constructor.
data DomainEndpointOptions = DomainEndpointOptions'
  { -- | Whether the domain is HTTPS only enabled.
    enforceHTTPS :: Core.Maybe Core.Bool,
    -- | The minimum required TLS version
    tLSSecurityPolicy :: Core.Maybe Types.TLSSecurityPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DomainEndpointOptions' value with any optional fields omitted.
mkDomainEndpointOptions ::
  DomainEndpointOptions
mkDomainEndpointOptions =
  DomainEndpointOptions'
    { enforceHTTPS = Core.Nothing,
      tLSSecurityPolicy = Core.Nothing
    }

-- | Whether the domain is HTTPS only enabled.
--
-- /Note:/ Consider using 'enforceHTTPS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoEnforceHTTPS :: Lens.Lens' DomainEndpointOptions (Core.Maybe Core.Bool)
deoEnforceHTTPS = Lens.field @"enforceHTTPS"
{-# DEPRECATED deoEnforceHTTPS "Use generic-lens or generic-optics with 'enforceHTTPS' instead." #-}

-- | The minimum required TLS version
--
-- /Note:/ Consider using 'tLSSecurityPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoTLSSecurityPolicy :: Lens.Lens' DomainEndpointOptions (Core.Maybe Types.TLSSecurityPolicy)
deoTLSSecurityPolicy = Lens.field @"tLSSecurityPolicy"
{-# DEPRECATED deoTLSSecurityPolicy "Use generic-lens or generic-optics with 'tLSSecurityPolicy' instead." #-}

instance Core.FromXML DomainEndpointOptions where
  parseXML x =
    DomainEndpointOptions'
      Core.<$> (x Core..@? "EnforceHTTPS")
      Core.<*> (x Core..@? "TLSSecurityPolicy")
