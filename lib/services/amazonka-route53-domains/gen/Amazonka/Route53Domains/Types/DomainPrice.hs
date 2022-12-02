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
-- Module      : Amazonka.Route53Domains.Types.DomainPrice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.DomainPrice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Domains.Types.PriceWithCurrency

-- | Information about the domain price associated with a TLD.
--
-- /See:/ 'newDomainPrice' smart constructor.
data DomainPrice = DomainPrice'
  { -- | The name of the TLD for which the prices apply.
    name :: Prelude.Maybe Prelude.Text,
    -- | The price for transferring the domain registration to Route 53.
    transferPrice :: Prelude.Maybe PriceWithCurrency,
    -- | The price for domain registration with Route 53.
    registrationPrice :: Prelude.Maybe PriceWithCurrency,
    -- | The price for changing domain ownership.
    changeOwnershipPrice :: Prelude.Maybe PriceWithCurrency,
    -- | The price for restoring the domain with Route 53.
    restorationPrice :: Prelude.Maybe PriceWithCurrency,
    -- | The price for renewing domain registration with Route 53.
    renewalPrice :: Prelude.Maybe PriceWithCurrency
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainPrice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'domainPrice_name' - The name of the TLD for which the prices apply.
--
-- 'transferPrice', 'domainPrice_transferPrice' - The price for transferring the domain registration to Route 53.
--
-- 'registrationPrice', 'domainPrice_registrationPrice' - The price for domain registration with Route 53.
--
-- 'changeOwnershipPrice', 'domainPrice_changeOwnershipPrice' - The price for changing domain ownership.
--
-- 'restorationPrice', 'domainPrice_restorationPrice' - The price for restoring the domain with Route 53.
--
-- 'renewalPrice', 'domainPrice_renewalPrice' - The price for renewing domain registration with Route 53.
newDomainPrice ::
  DomainPrice
newDomainPrice =
  DomainPrice'
    { name = Prelude.Nothing,
      transferPrice = Prelude.Nothing,
      registrationPrice = Prelude.Nothing,
      changeOwnershipPrice = Prelude.Nothing,
      restorationPrice = Prelude.Nothing,
      renewalPrice = Prelude.Nothing
    }

-- | The name of the TLD for which the prices apply.
domainPrice_name :: Lens.Lens' DomainPrice (Prelude.Maybe Prelude.Text)
domainPrice_name = Lens.lens (\DomainPrice' {name} -> name) (\s@DomainPrice' {} a -> s {name = a} :: DomainPrice)

-- | The price for transferring the domain registration to Route 53.
domainPrice_transferPrice :: Lens.Lens' DomainPrice (Prelude.Maybe PriceWithCurrency)
domainPrice_transferPrice = Lens.lens (\DomainPrice' {transferPrice} -> transferPrice) (\s@DomainPrice' {} a -> s {transferPrice = a} :: DomainPrice)

-- | The price for domain registration with Route 53.
domainPrice_registrationPrice :: Lens.Lens' DomainPrice (Prelude.Maybe PriceWithCurrency)
domainPrice_registrationPrice = Lens.lens (\DomainPrice' {registrationPrice} -> registrationPrice) (\s@DomainPrice' {} a -> s {registrationPrice = a} :: DomainPrice)

-- | The price for changing domain ownership.
domainPrice_changeOwnershipPrice :: Lens.Lens' DomainPrice (Prelude.Maybe PriceWithCurrency)
domainPrice_changeOwnershipPrice = Lens.lens (\DomainPrice' {changeOwnershipPrice} -> changeOwnershipPrice) (\s@DomainPrice' {} a -> s {changeOwnershipPrice = a} :: DomainPrice)

-- | The price for restoring the domain with Route 53.
domainPrice_restorationPrice :: Lens.Lens' DomainPrice (Prelude.Maybe PriceWithCurrency)
domainPrice_restorationPrice = Lens.lens (\DomainPrice' {restorationPrice} -> restorationPrice) (\s@DomainPrice' {} a -> s {restorationPrice = a} :: DomainPrice)

-- | The price for renewing domain registration with Route 53.
domainPrice_renewalPrice :: Lens.Lens' DomainPrice (Prelude.Maybe PriceWithCurrency)
domainPrice_renewalPrice = Lens.lens (\DomainPrice' {renewalPrice} -> renewalPrice) (\s@DomainPrice' {} a -> s {renewalPrice = a} :: DomainPrice)

instance Data.FromJSON DomainPrice where
  parseJSON =
    Data.withObject
      "DomainPrice"
      ( \x ->
          DomainPrice'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "TransferPrice")
            Prelude.<*> (x Data..:? "RegistrationPrice")
            Prelude.<*> (x Data..:? "ChangeOwnershipPrice")
            Prelude.<*> (x Data..:? "RestorationPrice")
            Prelude.<*> (x Data..:? "RenewalPrice")
      )

instance Prelude.Hashable DomainPrice where
  hashWithSalt _salt DomainPrice' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` transferPrice
      `Prelude.hashWithSalt` registrationPrice
      `Prelude.hashWithSalt` changeOwnershipPrice
      `Prelude.hashWithSalt` restorationPrice
      `Prelude.hashWithSalt` renewalPrice

instance Prelude.NFData DomainPrice where
  rnf DomainPrice' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf transferPrice
      `Prelude.seq` Prelude.rnf registrationPrice
      `Prelude.seq` Prelude.rnf changeOwnershipPrice
      `Prelude.seq` Prelude.rnf restorationPrice
      `Prelude.seq` Prelude.rnf renewalPrice
