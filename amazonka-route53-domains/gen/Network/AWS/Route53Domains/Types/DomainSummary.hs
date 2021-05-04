{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Summary information about one domain.
--
-- /See:/ 'newDomainSummary' smart constructor.
data DomainSummary = DomainSummary'
  { -- | Expiration date of the domain in Unix time format and Coordinated
    -- Universal Time (UTC).
    expiry :: Prelude.Maybe Prelude.POSIX,
    -- | Indicates whether the domain is automatically renewed upon expiration.
    autoRenew :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a domain is locked from unauthorized transfer to
    -- another party.
    transferLock :: Prelude.Maybe Prelude.Bool,
    -- | The name of the domain that the summary information applies to.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DomainSummary
newDomainSummary pDomainName_ =
  DomainSummary'
    { expiry = Prelude.Nothing,
      autoRenew = Prelude.Nothing,
      transferLock = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Expiration date of the domain in Unix time format and Coordinated
-- Universal Time (UTC).
domainSummary_expiry :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.UTCTime)
domainSummary_expiry = Lens.lens (\DomainSummary' {expiry} -> expiry) (\s@DomainSummary' {} a -> s {expiry = a} :: DomainSummary) Prelude.. Lens.mapping Prelude._Time

-- | Indicates whether the domain is automatically renewed upon expiration.
domainSummary_autoRenew :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Bool)
domainSummary_autoRenew = Lens.lens (\DomainSummary' {autoRenew} -> autoRenew) (\s@DomainSummary' {} a -> s {autoRenew = a} :: DomainSummary)

-- | Indicates whether a domain is locked from unauthorized transfer to
-- another party.
domainSummary_transferLock :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Bool)
domainSummary_transferLock = Lens.lens (\DomainSummary' {transferLock} -> transferLock) (\s@DomainSummary' {} a -> s {transferLock = a} :: DomainSummary)

-- | The name of the domain that the summary information applies to.
domainSummary_domainName :: Lens.Lens' DomainSummary Prelude.Text
domainSummary_domainName = Lens.lens (\DomainSummary' {domainName} -> domainName) (\s@DomainSummary' {} a -> s {domainName = a} :: DomainSummary)

instance Prelude.FromJSON DomainSummary where
  parseJSON =
    Prelude.withObject
      "DomainSummary"
      ( \x ->
          DomainSummary'
            Prelude.<$> (x Prelude..:? "Expiry")
            Prelude.<*> (x Prelude..:? "AutoRenew")
            Prelude.<*> (x Prelude..:? "TransferLock")
            Prelude.<*> (x Prelude..: "DomainName")
      )

instance Prelude.Hashable DomainSummary

instance Prelude.NFData DomainSummary
