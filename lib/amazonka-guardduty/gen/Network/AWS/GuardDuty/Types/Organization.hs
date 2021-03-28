{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Organization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.Organization
  ( Organization (..)
  -- * Smart constructor
  , mkOrganization
  -- * Lenses
  , oAsn
  , oAsnOrg
  , oIsp
  , oOrg
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the ISP organization of the remote IP address.
--
-- /See:/ 'mkOrganization' smart constructor.
data Organization = Organization'
  { asn :: Core.Maybe Core.Text
    -- ^ The Autonomous System Number (ASN) of the internet provider of the remote IP address.
  , asnOrg :: Core.Maybe Core.Text
    -- ^ The organization that registered this ASN.
  , isp :: Core.Maybe Core.Text
    -- ^ The ISP information for the internet provider.
  , org :: Core.Maybe Core.Text
    -- ^ The name of the internet provider.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Organization' value with any optional fields omitted.
mkOrganization
    :: Organization
mkOrganization
  = Organization'{asn = Core.Nothing, asnOrg = Core.Nothing,
                  isp = Core.Nothing, org = Core.Nothing}

-- | The Autonomous System Number (ASN) of the internet provider of the remote IP address.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oAsn :: Lens.Lens' Organization (Core.Maybe Core.Text)
oAsn = Lens.field @"asn"
{-# INLINEABLE oAsn #-}
{-# DEPRECATED asn "Use generic-lens or generic-optics with 'asn' instead"  #-}

-- | The organization that registered this ASN.
--
-- /Note:/ Consider using 'asnOrg' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oAsnOrg :: Lens.Lens' Organization (Core.Maybe Core.Text)
oAsnOrg = Lens.field @"asnOrg"
{-# INLINEABLE oAsnOrg #-}
{-# DEPRECATED asnOrg "Use generic-lens or generic-optics with 'asnOrg' instead"  #-}

-- | The ISP information for the internet provider.
--
-- /Note:/ Consider using 'isp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oIsp :: Lens.Lens' Organization (Core.Maybe Core.Text)
oIsp = Lens.field @"isp"
{-# INLINEABLE oIsp #-}
{-# DEPRECATED isp "Use generic-lens or generic-optics with 'isp' instead"  #-}

-- | The name of the internet provider.
--
-- /Note:/ Consider using 'org' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOrg :: Lens.Lens' Organization (Core.Maybe Core.Text)
oOrg = Lens.field @"org"
{-# INLINEABLE oOrg #-}
{-# DEPRECATED org "Use generic-lens or generic-optics with 'org' instead"  #-}

instance Core.FromJSON Organization where
        parseJSON
          = Core.withObject "Organization" Core.$
              \ x ->
                Organization' Core.<$>
                  (x Core..:? "asn") Core.<*> x Core..:? "asnOrg" Core.<*>
                    x Core..:? "isp"
                    Core.<*> x Core..:? "org"
