{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.DomainSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.DomainSummary
  ( DomainSummary (..),

    -- * Smart constructor
    mkDomainSummary,

    -- * Lenses
    dsDomainName,
    dsAutoRenew,
    dsExpiry,
    dsTransferLock,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53Domains.Types.DomainName as Types

-- | Summary information about one domain.
--
-- /See:/ 'mkDomainSummary' smart constructor.
data DomainSummary = DomainSummary'
  { -- | The name of the domain that the summary information applies to.
    domainName :: Types.DomainName,
    -- | Indicates whether the domain is automatically renewed upon expiration.
    autoRenew :: Core.Maybe Core.Bool,
    -- | Expiration date of the domain in Unix time format and Coordinated Universal Time (UTC).
    expiry :: Core.Maybe Core.NominalDiffTime,
    -- | Indicates whether a domain is locked from unauthorized transfer to another party.
    transferLock :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DomainSummary' value with any optional fields omitted.
mkDomainSummary ::
  -- | 'domainName'
  Types.DomainName ->
  DomainSummary
mkDomainSummary domainName =
  DomainSummary'
    { domainName,
      autoRenew = Core.Nothing,
      expiry = Core.Nothing,
      transferLock = Core.Nothing
    }

-- | The name of the domain that the summary information applies to.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDomainName :: Lens.Lens' DomainSummary Types.DomainName
dsDomainName = Lens.field @"domainName"
{-# DEPRECATED dsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Indicates whether the domain is automatically renewed upon expiration.
--
-- /Note:/ Consider using 'autoRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAutoRenew :: Lens.Lens' DomainSummary (Core.Maybe Core.Bool)
dsAutoRenew = Lens.field @"autoRenew"
{-# DEPRECATED dsAutoRenew "Use generic-lens or generic-optics with 'autoRenew' instead." #-}

-- | Expiration date of the domain in Unix time format and Coordinated Universal Time (UTC).
--
-- /Note:/ Consider using 'expiry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsExpiry :: Lens.Lens' DomainSummary (Core.Maybe Core.NominalDiffTime)
dsExpiry = Lens.field @"expiry"
{-# DEPRECATED dsExpiry "Use generic-lens or generic-optics with 'expiry' instead." #-}

-- | Indicates whether a domain is locked from unauthorized transfer to another party.
--
-- /Note:/ Consider using 'transferLock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsTransferLock :: Lens.Lens' DomainSummary (Core.Maybe Core.Bool)
dsTransferLock = Lens.field @"transferLock"
{-# DEPRECATED dsTransferLock "Use generic-lens or generic-optics with 'transferLock' instead." #-}

instance Core.FromJSON DomainSummary where
  parseJSON =
    Core.withObject "DomainSummary" Core.$
      \x ->
        DomainSummary'
          Core.<$> (x Core..: "DomainName")
          Core.<*> (x Core..:? "AutoRenew")
          Core.<*> (x Core..:? "Expiry")
          Core.<*> (x Core..:? "TransferLock")
