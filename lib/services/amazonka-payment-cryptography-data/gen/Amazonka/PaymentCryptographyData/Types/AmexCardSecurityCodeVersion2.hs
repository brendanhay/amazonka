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
-- Module      : Amazonka.PaymentCryptographyData.Types.AmexCardSecurityCodeVersion2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.AmexCardSecurityCodeVersion2 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Card data parameters that are required to generate a Card Security Code
-- (CSC2) for an AMEX payment card.
--
-- /See:/ 'newAmexCardSecurityCodeVersion2' smart constructor.
data AmexCardSecurityCodeVersion2 = AmexCardSecurityCodeVersion2'
  { -- | The expiry date of a payment card.
    cardExpiryDate :: Prelude.Text,
    -- | The service code of the AMEX payment card. This is different from the
    -- Card Security Code (CSC).
    serviceCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmexCardSecurityCodeVersion2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cardExpiryDate', 'amexCardSecurityCodeVersion2_cardExpiryDate' - The expiry date of a payment card.
--
-- 'serviceCode', 'amexCardSecurityCodeVersion2_serviceCode' - The service code of the AMEX payment card. This is different from the
-- Card Security Code (CSC).
newAmexCardSecurityCodeVersion2 ::
  -- | 'cardExpiryDate'
  Prelude.Text ->
  -- | 'serviceCode'
  Prelude.Text ->
  AmexCardSecurityCodeVersion2
newAmexCardSecurityCodeVersion2
  pCardExpiryDate_
  pServiceCode_ =
    AmexCardSecurityCodeVersion2'
      { cardExpiryDate =
          pCardExpiryDate_,
        serviceCode = pServiceCode_
      }

-- | The expiry date of a payment card.
amexCardSecurityCodeVersion2_cardExpiryDate :: Lens.Lens' AmexCardSecurityCodeVersion2 Prelude.Text
amexCardSecurityCodeVersion2_cardExpiryDate = Lens.lens (\AmexCardSecurityCodeVersion2' {cardExpiryDate} -> cardExpiryDate) (\s@AmexCardSecurityCodeVersion2' {} a -> s {cardExpiryDate = a} :: AmexCardSecurityCodeVersion2)

-- | The service code of the AMEX payment card. This is different from the
-- Card Security Code (CSC).
amexCardSecurityCodeVersion2_serviceCode :: Lens.Lens' AmexCardSecurityCodeVersion2 Prelude.Text
amexCardSecurityCodeVersion2_serviceCode = Lens.lens (\AmexCardSecurityCodeVersion2' {serviceCode} -> serviceCode) (\s@AmexCardSecurityCodeVersion2' {} a -> s {serviceCode = a} :: AmexCardSecurityCodeVersion2)

instance
  Prelude.Hashable
    AmexCardSecurityCodeVersion2
  where
  hashWithSalt _salt AmexCardSecurityCodeVersion2' {..} =
    _salt
      `Prelude.hashWithSalt` cardExpiryDate
      `Prelude.hashWithSalt` serviceCode

instance Prelude.NFData AmexCardSecurityCodeVersion2 where
  rnf AmexCardSecurityCodeVersion2' {..} =
    Prelude.rnf cardExpiryDate
      `Prelude.seq` Prelude.rnf serviceCode

instance Data.ToJSON AmexCardSecurityCodeVersion2 where
  toJSON AmexCardSecurityCodeVersion2' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CardExpiryDate" Data..= cardExpiryDate),
            Prelude.Just ("ServiceCode" Data..= serviceCode)
          ]
      )
