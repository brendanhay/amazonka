{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.DomainSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types.DomainSummary
  ( DomainSummary (..)
  -- * Smart constructor
  , mkDomainSummary
  -- * Lenses
  , dsDomainName
  , dsAutoRenew
  , dsExpiry
  , dsTransferLock
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53Domains.Types.DomainName as Types

-- | Summary information about one domain.
--
-- /See:/ 'mkDomainSummary' smart constructor.
data DomainSummary = DomainSummary'
  { domainName :: Types.DomainName
    -- ^ The name of the domain that the summary information applies to.
  , autoRenew :: Core.Maybe Core.Bool
    -- ^ Indicates whether the domain is automatically renewed upon expiration.
  , expiry :: Core.Maybe Core.NominalDiffTime
    -- ^ Expiration date of the domain in Unix time format and Coordinated Universal Time (UTC).
  , transferLock :: Core.Maybe Core.Bool
    -- ^ Indicates whether a domain is locked from unauthorized transfer to another party.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DomainSummary' value with any optional fields omitted.
mkDomainSummary
    :: Types.DomainName -- ^ 'domainName'
    -> DomainSummary
mkDomainSummary domainName
  = DomainSummary'{domainName, autoRenew = Core.Nothing,
                   expiry = Core.Nothing, transferLock = Core.Nothing}

-- | The name of the domain that the summary information applies to.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDomainName :: Lens.Lens' DomainSummary Types.DomainName
dsDomainName = Lens.field @"domainName"
{-# INLINEABLE dsDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | Indicates whether the domain is automatically renewed upon expiration.
--
-- /Note:/ Consider using 'autoRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAutoRenew :: Lens.Lens' DomainSummary (Core.Maybe Core.Bool)
dsAutoRenew = Lens.field @"autoRenew"
{-# INLINEABLE dsAutoRenew #-}
{-# DEPRECATED autoRenew "Use generic-lens or generic-optics with 'autoRenew' instead"  #-}

-- | Expiration date of the domain in Unix time format and Coordinated Universal Time (UTC).
--
-- /Note:/ Consider using 'expiry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsExpiry :: Lens.Lens' DomainSummary (Core.Maybe Core.NominalDiffTime)
dsExpiry = Lens.field @"expiry"
{-# INLINEABLE dsExpiry #-}
{-# DEPRECATED expiry "Use generic-lens or generic-optics with 'expiry' instead"  #-}

-- | Indicates whether a domain is locked from unauthorized transfer to another party.
--
-- /Note:/ Consider using 'transferLock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsTransferLock :: Lens.Lens' DomainSummary (Core.Maybe Core.Bool)
dsTransferLock = Lens.field @"transferLock"
{-# INLINEABLE dsTransferLock #-}
{-# DEPRECATED transferLock "Use generic-lens or generic-optics with 'transferLock' instead"  #-}

instance Core.FromJSON DomainSummary where
        parseJSON
          = Core.withObject "DomainSummary" Core.$
              \ x ->
                DomainSummary' Core.<$>
                  (x Core..: "DomainName") Core.<*> x Core..:? "AutoRenew" Core.<*>
                    x Core..:? "Expiry"
                    Core.<*> x Core..:? "TransferLock"
