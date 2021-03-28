{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DomainMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DomainMembership
  ( DomainMembership (..)
  -- * Smart constructor
  , mkDomainMembership
  -- * Lenses
  , dmDomain
  , dmFQDN
  , dmIAMRoleName
  , dmStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An Active Directory Domain membership record associated with the DB instance or cluster.
--
-- /See:/ 'mkDomainMembership' smart constructor.
data DomainMembership = DomainMembership'
  { domain :: Core.Maybe Core.Text
    -- ^ The identifier of the Active Directory Domain.
  , fqdn :: Core.Maybe Core.Text
    -- ^ The fully qualified domain name of the Active Directory Domain.
  , iAMRoleName :: Core.Maybe Core.Text
    -- ^ The name of the IAM role to be used when making API calls to the Directory Service.
  , status :: Core.Maybe Core.Text
    -- ^ The status of the Active Directory Domain membership for the DB instance or cluster. Values include joined, pending-join, failed, and so on.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DomainMembership' value with any optional fields omitted.
mkDomainMembership
    :: DomainMembership
mkDomainMembership
  = DomainMembership'{domain = Core.Nothing, fqdn = Core.Nothing,
                      iAMRoleName = Core.Nothing, status = Core.Nothing}

-- | The identifier of the Active Directory Domain.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmDomain :: Lens.Lens' DomainMembership (Core.Maybe Core.Text)
dmDomain = Lens.field @"domain"
{-# INLINEABLE dmDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | The fully qualified domain name of the Active Directory Domain.
--
-- /Note:/ Consider using 'fqdn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmFQDN :: Lens.Lens' DomainMembership (Core.Maybe Core.Text)
dmFQDN = Lens.field @"fqdn"
{-# INLINEABLE dmFQDN #-}
{-# DEPRECATED fqdn "Use generic-lens or generic-optics with 'fqdn' instead"  #-}

-- | The name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'iAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmIAMRoleName :: Lens.Lens' DomainMembership (Core.Maybe Core.Text)
dmIAMRoleName = Lens.field @"iAMRoleName"
{-# INLINEABLE dmIAMRoleName #-}
{-# DEPRECATED iAMRoleName "Use generic-lens or generic-optics with 'iAMRoleName' instead"  #-}

-- | The status of the Active Directory Domain membership for the DB instance or cluster. Values include joined, pending-join, failed, and so on.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmStatus :: Lens.Lens' DomainMembership (Core.Maybe Core.Text)
dmStatus = Lens.field @"status"
{-# INLINEABLE dmStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML DomainMembership where
        parseXML x
          = DomainMembership' Core.<$>
              (x Core..@? "Domain") Core.<*> x Core..@? "FQDN" Core.<*>
                x Core..@? "IAMRoleName"
                Core.<*> x Core..@? "Status"
