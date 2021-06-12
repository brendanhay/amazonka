{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.DomainSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.DomainSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Summary information about one domain.
--
-- /See:/ 'newDomainSummary' smart constructor.
data DomainSummary = DomainSummary'
  { -- | Expiration date of the domain in Unix time format and Coordinated
    -- Universal Time (UTC).
    expiry :: Core.Maybe Core.POSIX,
    -- | Indicates whether the domain is automatically renewed upon expiration.
    autoRenew :: Core.Maybe Core.Bool,
    -- | Indicates whether a domain is locked from unauthorized transfer to
    -- another party.
    transferLock :: Core.Maybe Core.Bool,
    -- | The name of the domain that the summary information applies to.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DomainSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiry', 'domainSummary_expiry' - Expiration date of the domain in Unix time format and Coordinated
-- Universal Time (UTC).
--
-- 'autoRenew', 'domainSummary_autoRenew' - Indicates whether the domain is automatically renewed upon expiration.
--
-- 'transferLock', 'domainSummary_transferLock' - Indicates whether a domain is locked from unauthorized transfer to
-- another party.
--
-- 'domainName', 'domainSummary_domainName' - The name of the domain that the summary information applies to.
newDomainSummary ::
  -- | 'domainName'
  Core.Text ->
  DomainSummary
newDomainSummary pDomainName_ =
  DomainSummary'
    { expiry = Core.Nothing,
      autoRenew = Core.Nothing,
      transferLock = Core.Nothing,
      domainName = pDomainName_
    }

-- | Expiration date of the domain in Unix time format and Coordinated
-- Universal Time (UTC).
domainSummary_expiry :: Lens.Lens' DomainSummary (Core.Maybe Core.UTCTime)
domainSummary_expiry = Lens.lens (\DomainSummary' {expiry} -> expiry) (\s@DomainSummary' {} a -> s {expiry = a} :: DomainSummary) Core.. Lens.mapping Core._Time

-- | Indicates whether the domain is automatically renewed upon expiration.
domainSummary_autoRenew :: Lens.Lens' DomainSummary (Core.Maybe Core.Bool)
domainSummary_autoRenew = Lens.lens (\DomainSummary' {autoRenew} -> autoRenew) (\s@DomainSummary' {} a -> s {autoRenew = a} :: DomainSummary)

-- | Indicates whether a domain is locked from unauthorized transfer to
-- another party.
domainSummary_transferLock :: Lens.Lens' DomainSummary (Core.Maybe Core.Bool)
domainSummary_transferLock = Lens.lens (\DomainSummary' {transferLock} -> transferLock) (\s@DomainSummary' {} a -> s {transferLock = a} :: DomainSummary)

-- | The name of the domain that the summary information applies to.
domainSummary_domainName :: Lens.Lens' DomainSummary Core.Text
domainSummary_domainName = Lens.lens (\DomainSummary' {domainName} -> domainName) (\s@DomainSummary' {} a -> s {domainName = a} :: DomainSummary)

instance Core.FromJSON DomainSummary where
  parseJSON =
    Core.withObject
      "DomainSummary"
      ( \x ->
          DomainSummary'
            Core.<$> (x Core..:? "Expiry")
            Core.<*> (x Core..:? "AutoRenew")
            Core.<*> (x Core..:? "TransferLock")
            Core.<*> (x Core..: "DomainName")
      )

instance Core.Hashable DomainSummary

instance Core.NFData DomainSummary
