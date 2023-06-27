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
-- Module      : Amazonka.PaymentCryptographyData.Types.CardVerificationValue1
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.CardVerificationValue1 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Card data parameters that are required to verify CVV (Card Verification
-- Value) for the payment card.
--
-- /See:/ 'newCardVerificationValue1' smart constructor.
data CardVerificationValue1 = CardVerificationValue1'
  { -- | The expiry date of a payment card.
    cardExpiryDate :: Prelude.Text,
    -- | The service code of the payment card. This is different from Card
    -- Security Code (CSC).
    serviceCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CardVerificationValue1' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cardExpiryDate', 'cardVerificationValue1_cardExpiryDate' - The expiry date of a payment card.
--
-- 'serviceCode', 'cardVerificationValue1_serviceCode' - The service code of the payment card. This is different from Card
-- Security Code (CSC).
newCardVerificationValue1 ::
  -- | 'cardExpiryDate'
  Prelude.Text ->
  -- | 'serviceCode'
  Prelude.Text ->
  CardVerificationValue1
newCardVerificationValue1
  pCardExpiryDate_
  pServiceCode_ =
    CardVerificationValue1'
      { cardExpiryDate =
          pCardExpiryDate_,
        serviceCode = pServiceCode_
      }

-- | The expiry date of a payment card.
cardVerificationValue1_cardExpiryDate :: Lens.Lens' CardVerificationValue1 Prelude.Text
cardVerificationValue1_cardExpiryDate = Lens.lens (\CardVerificationValue1' {cardExpiryDate} -> cardExpiryDate) (\s@CardVerificationValue1' {} a -> s {cardExpiryDate = a} :: CardVerificationValue1)

-- | The service code of the payment card. This is different from Card
-- Security Code (CSC).
cardVerificationValue1_serviceCode :: Lens.Lens' CardVerificationValue1 Prelude.Text
cardVerificationValue1_serviceCode = Lens.lens (\CardVerificationValue1' {serviceCode} -> serviceCode) (\s@CardVerificationValue1' {} a -> s {serviceCode = a} :: CardVerificationValue1)

instance Prelude.Hashable CardVerificationValue1 where
  hashWithSalt _salt CardVerificationValue1' {..} =
    _salt
      `Prelude.hashWithSalt` cardExpiryDate
      `Prelude.hashWithSalt` serviceCode

instance Prelude.NFData CardVerificationValue1 where
  rnf CardVerificationValue1' {..} =
    Prelude.rnf cardExpiryDate
      `Prelude.seq` Prelude.rnf serviceCode

instance Data.ToJSON CardVerificationValue1 where
  toJSON CardVerificationValue1' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CardExpiryDate" Data..= cardExpiryDate),
            Prelude.Just ("ServiceCode" Data..= serviceCode)
          ]
      )
