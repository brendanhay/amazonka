{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Organization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Organization
  ( Organization (..),

    -- * Smart constructor
    mkOrganization,

    -- * Lenses
    oAsn,
    oAsnOrg,
    oIsp,
    oOrg,
  )
where

import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the ISP organization of the remote IP address.
--
-- /See:/ 'mkOrganization' smart constructor.
data Organization = Organization'
  { -- | The Autonomous System Number (ASN) of the internet provider of the remote IP address.
    asn :: Core.Maybe Types.String,
    -- | The organization that registered this ASN.
    asnOrg :: Core.Maybe Types.String,
    -- | The ISP information for the internet provider.
    isp :: Core.Maybe Types.String,
    -- | The name of the internet provider.
    org :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Organization' value with any optional fields omitted.
mkOrganization ::
  Organization
mkOrganization =
  Organization'
    { asn = Core.Nothing,
      asnOrg = Core.Nothing,
      isp = Core.Nothing,
      org = Core.Nothing
    }

-- | The Autonomous System Number (ASN) of the internet provider of the remote IP address.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oAsn :: Lens.Lens' Organization (Core.Maybe Types.String)
oAsn = Lens.field @"asn"
{-# DEPRECATED oAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

-- | The organization that registered this ASN.
--
-- /Note:/ Consider using 'asnOrg' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oAsnOrg :: Lens.Lens' Organization (Core.Maybe Types.String)
oAsnOrg = Lens.field @"asnOrg"
{-# DEPRECATED oAsnOrg "Use generic-lens or generic-optics with 'asnOrg' instead." #-}

-- | The ISP information for the internet provider.
--
-- /Note:/ Consider using 'isp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oIsp :: Lens.Lens' Organization (Core.Maybe Types.String)
oIsp = Lens.field @"isp"
{-# DEPRECATED oIsp "Use generic-lens or generic-optics with 'isp' instead." #-}

-- | The name of the internet provider.
--
-- /Note:/ Consider using 'org' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOrg :: Lens.Lens' Organization (Core.Maybe Types.String)
oOrg = Lens.field @"org"
{-# DEPRECATED oOrg "Use generic-lens or generic-optics with 'org' instead." #-}

instance Core.FromJSON Organization where
  parseJSON =
    Core.withObject "Organization" Core.$
      \x ->
        Organization'
          Core.<$> (x Core..:? "asn")
          Core.<*> (x Core..:? "asnOrg")
          Core.<*> (x Core..:? "isp")
          Core.<*> (x Core..:? "org")
