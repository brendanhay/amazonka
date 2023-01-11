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
-- Module      : Amazonka.Route53Domains.Types.Consent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.Consent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Customer\'s consent for the owner change request.
--
-- /See:/ 'newConsent' smart constructor.
data Consent = Consent'
  { -- | Maximum amount the customer agreed to accept.
    maxPrice :: Prelude.Double,
    -- | Currency for the @MaxPrice@.
    currency :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Consent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxPrice', 'consent_maxPrice' - Maximum amount the customer agreed to accept.
--
-- 'currency', 'consent_currency' - Currency for the @MaxPrice@.
newConsent ::
  -- | 'maxPrice'
  Prelude.Double ->
  -- | 'currency'
  Prelude.Text ->
  Consent
newConsent pMaxPrice_ pCurrency_ =
  Consent'
    { maxPrice = pMaxPrice_,
      currency = pCurrency_
    }

-- | Maximum amount the customer agreed to accept.
consent_maxPrice :: Lens.Lens' Consent Prelude.Double
consent_maxPrice = Lens.lens (\Consent' {maxPrice} -> maxPrice) (\s@Consent' {} a -> s {maxPrice = a} :: Consent)

-- | Currency for the @MaxPrice@.
consent_currency :: Lens.Lens' Consent Prelude.Text
consent_currency = Lens.lens (\Consent' {currency} -> currency) (\s@Consent' {} a -> s {currency = a} :: Consent)

instance Prelude.Hashable Consent where
  hashWithSalt _salt Consent' {..} =
    _salt `Prelude.hashWithSalt` maxPrice
      `Prelude.hashWithSalt` currency

instance Prelude.NFData Consent where
  rnf Consent' {..} =
    Prelude.rnf maxPrice
      `Prelude.seq` Prelude.rnf currency

instance Data.ToJSON Consent where
  toJSON Consent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("MaxPrice" Data..= maxPrice),
            Prelude.Just ("Currency" Data..= currency)
          ]
      )
