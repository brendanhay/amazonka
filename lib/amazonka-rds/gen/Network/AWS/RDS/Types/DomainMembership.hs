{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DomainMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DomainMembership
  ( DomainMembership (..),

    -- * Smart constructor
    mkDomainMembership,

    -- * Lenses
    dmDomain,
    dmFQDN,
    dmIAMRoleName,
    dmStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.Domain as Types
import qualified Network.AWS.RDS.Types.FQDN as Types
import qualified Network.AWS.RDS.Types.IAMRoleName as Types
import qualified Network.AWS.RDS.Types.Status as Types

-- | An Active Directory Domain membership record associated with the DB instance or cluster.
--
-- /See:/ 'mkDomainMembership' smart constructor.
data DomainMembership = DomainMembership'
  { -- | The identifier of the Active Directory Domain.
    domain :: Core.Maybe Types.Domain,
    -- | The fully qualified domain name of the Active Directory Domain.
    fqdn :: Core.Maybe Types.FQDN,
    -- | The name of the IAM role to be used when making API calls to the Directory Service.
    iAMRoleName :: Core.Maybe Types.IAMRoleName,
    -- | The status of the Active Directory Domain membership for the DB instance or cluster. Values include joined, pending-join, failed, and so on.
    status :: Core.Maybe Types.Status
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DomainMembership' value with any optional fields omitted.
mkDomainMembership ::
  DomainMembership
mkDomainMembership =
  DomainMembership'
    { domain = Core.Nothing,
      fqdn = Core.Nothing,
      iAMRoleName = Core.Nothing,
      status = Core.Nothing
    }

-- | The identifier of the Active Directory Domain.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmDomain :: Lens.Lens' DomainMembership (Core.Maybe Types.Domain)
dmDomain = Lens.field @"domain"
{-# DEPRECATED dmDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The fully qualified domain name of the Active Directory Domain.
--
-- /Note:/ Consider using 'fqdn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmFQDN :: Lens.Lens' DomainMembership (Core.Maybe Types.FQDN)
dmFQDN = Lens.field @"fqdn"
{-# DEPRECATED dmFQDN "Use generic-lens or generic-optics with 'fqdn' instead." #-}

-- | The name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'iAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmIAMRoleName :: Lens.Lens' DomainMembership (Core.Maybe Types.IAMRoleName)
dmIAMRoleName = Lens.field @"iAMRoleName"
{-# DEPRECATED dmIAMRoleName "Use generic-lens or generic-optics with 'iAMRoleName' instead." #-}

-- | The status of the Active Directory Domain membership for the DB instance or cluster. Values include joined, pending-join, failed, and so on.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmStatus :: Lens.Lens' DomainMembership (Core.Maybe Types.Status)
dmStatus = Lens.field @"status"
{-# DEPRECATED dmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML DomainMembership where
  parseXML x =
    DomainMembership'
      Core.<$> (x Core..@? "Domain")
      Core.<*> (x Core..@? "FQDN")
      Core.<*> (x Core..@? "IAMRoleName")
      Core.<*> (x Core..@? "Status")
