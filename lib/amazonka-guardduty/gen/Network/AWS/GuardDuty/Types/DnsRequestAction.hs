{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DnsRequestAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.DnsRequestAction
  ( DnsRequestAction (..)
  -- * Smart constructor
  , mkDnsRequestAction
  -- * Lenses
  , draDomain
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the DNS_REQUEST action described in this finding.
--
-- /See:/ 'mkDnsRequestAction' smart constructor.
newtype DnsRequestAction = DnsRequestAction'
  { domain :: Core.Maybe Core.Text
    -- ^ The domain information for the API request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DnsRequestAction' value with any optional fields omitted.
mkDnsRequestAction
    :: DnsRequestAction
mkDnsRequestAction = DnsRequestAction'{domain = Core.Nothing}

-- | The domain information for the API request.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draDomain :: Lens.Lens' DnsRequestAction (Core.Maybe Core.Text)
draDomain = Lens.field @"domain"
{-# INLINEABLE draDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

instance Core.FromJSON DnsRequestAction where
        parseJSON
          = Core.withObject "DnsRequestAction" Core.$
              \ x -> DnsRequestAction' Core.<$> (x Core..:? "domain")
