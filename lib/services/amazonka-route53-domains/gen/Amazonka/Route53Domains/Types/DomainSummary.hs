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
-- Module      : Amazonka.Route53Domains.Types.DomainSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.DomainSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information about one domain.
--
-- /See:/ 'newDomainSummary' smart constructor.
data DomainSummary = DomainSummary'
  { -- | Indicates whether the domain is automatically renewed upon expiration.
    autoRenew :: Prelude.Maybe Prelude.Bool,
    -- | Expiration date of the domain in Unix time format and Coordinated
    -- Universal Time (UTC).
    expiry :: Prelude.Maybe Data.POSIX,
    -- | Indicates whether a domain is locked from unauthorized transfer to
    -- another party.
    transferLock :: Prelude.Maybe Prelude.Bool,
    -- | The name of the domain that the summary information applies to.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoRenew', 'domainSummary_autoRenew' - Indicates whether the domain is automatically renewed upon expiration.
--
-- 'expiry', 'domainSummary_expiry' - Expiration date of the domain in Unix time format and Coordinated
-- Universal Time (UTC).
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
    { autoRenew = Prelude.Nothing,
      expiry = Prelude.Nothing,
      transferLock = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Indicates whether the domain is automatically renewed upon expiration.
domainSummary_autoRenew :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Bool)
domainSummary_autoRenew = Lens.lens (\DomainSummary' {autoRenew} -> autoRenew) (\s@DomainSummary' {} a -> s {autoRenew = a} :: DomainSummary)

-- | Expiration date of the domain in Unix time format and Coordinated
-- Universal Time (UTC).
domainSummary_expiry :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.UTCTime)
domainSummary_expiry = Lens.lens (\DomainSummary' {expiry} -> expiry) (\s@DomainSummary' {} a -> s {expiry = a} :: DomainSummary) Prelude.. Lens.mapping Data._Time

-- | Indicates whether a domain is locked from unauthorized transfer to
-- another party.
domainSummary_transferLock :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Bool)
domainSummary_transferLock = Lens.lens (\DomainSummary' {transferLock} -> transferLock) (\s@DomainSummary' {} a -> s {transferLock = a} :: DomainSummary)

-- | The name of the domain that the summary information applies to.
domainSummary_domainName :: Lens.Lens' DomainSummary Prelude.Text
domainSummary_domainName = Lens.lens (\DomainSummary' {domainName} -> domainName) (\s@DomainSummary' {} a -> s {domainName = a} :: DomainSummary)

instance Data.FromJSON DomainSummary where
  parseJSON =
    Data.withObject
      "DomainSummary"
      ( \x ->
          DomainSummary'
            Prelude.<$> (x Data..:? "AutoRenew")
            Prelude.<*> (x Data..:? "Expiry")
            Prelude.<*> (x Data..:? "TransferLock")
            Prelude.<*> (x Data..: "DomainName")
      )

instance Prelude.Hashable DomainSummary where
  hashWithSalt _salt DomainSummary' {..} =
    _salt `Prelude.hashWithSalt` autoRenew
      `Prelude.hashWithSalt` expiry
      `Prelude.hashWithSalt` transferLock
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DomainSummary where
  rnf DomainSummary' {..} =
    Prelude.rnf autoRenew
      `Prelude.seq` Prelude.rnf expiry
      `Prelude.seq` Prelude.rnf transferLock
      `Prelude.seq` Prelude.rnf domainName
