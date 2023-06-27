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
-- Module      : Amazonka.PaymentCryptographyData.Types.CardGenerationAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.CardGenerationAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.AmexCardSecurityCodeVersion1
import Amazonka.PaymentCryptographyData.Types.AmexCardSecurityCodeVersion2
import Amazonka.PaymentCryptographyData.Types.CardHolderVerificationValue
import Amazonka.PaymentCryptographyData.Types.CardVerificationValue1
import Amazonka.PaymentCryptographyData.Types.CardVerificationValue2
import Amazonka.PaymentCryptographyData.Types.DynamicCardVerificationCode
import Amazonka.PaymentCryptographyData.Types.DynamicCardVerificationValue
import qualified Amazonka.Prelude as Prelude

-- | Card data parameters that are required to generate Card Verification
-- Values (CVV\/CVV2), Dynamic Card Verification Values (dCVV\/dCVV2), or
-- Card Security Codes (CSC).
--
-- /See:/ 'newCardGenerationAttributes' smart constructor.
data CardGenerationAttributes = CardGenerationAttributes'
  { amexCardSecurityCodeVersion1 :: Prelude.Maybe AmexCardSecurityCodeVersion1,
    -- | Card data parameters that are required to generate a Card Security Code
    -- (CSC2) for an AMEX payment card.
    amexCardSecurityCodeVersion2 :: Prelude.Maybe AmexCardSecurityCodeVersion2,
    -- | Card data parameters that are required to generate a cardholder
    -- verification value for the payment card.
    cardHolderVerificationValue :: Prelude.Maybe CardHolderVerificationValue,
    -- | Card data parameters that are required to generate Card Verification
    -- Value (CVV) for the payment card.
    cardVerificationValue1 :: Prelude.Maybe CardVerificationValue1,
    -- | Card data parameters that are required to generate Card Verification
    -- Value (CVV2) for the payment card.
    cardVerificationValue2 :: Prelude.Maybe CardVerificationValue2,
    -- | Card data parameters that are required to generate CDynamic Card
    -- Verification Code (dCVC) for the payment card.
    dynamicCardVerificationCode :: Prelude.Maybe DynamicCardVerificationCode,
    -- | Card data parameters that are required to generate CDynamic Card
    -- Verification Value (dCVV) for the payment card.
    dynamicCardVerificationValue :: Prelude.Maybe DynamicCardVerificationValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CardGenerationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amexCardSecurityCodeVersion1', 'cardGenerationAttributes_amexCardSecurityCodeVersion1' - Undocumented member.
--
-- 'amexCardSecurityCodeVersion2', 'cardGenerationAttributes_amexCardSecurityCodeVersion2' - Card data parameters that are required to generate a Card Security Code
-- (CSC2) for an AMEX payment card.
--
-- 'cardHolderVerificationValue', 'cardGenerationAttributes_cardHolderVerificationValue' - Card data parameters that are required to generate a cardholder
-- verification value for the payment card.
--
-- 'cardVerificationValue1', 'cardGenerationAttributes_cardVerificationValue1' - Card data parameters that are required to generate Card Verification
-- Value (CVV) for the payment card.
--
-- 'cardVerificationValue2', 'cardGenerationAttributes_cardVerificationValue2' - Card data parameters that are required to generate Card Verification
-- Value (CVV2) for the payment card.
--
-- 'dynamicCardVerificationCode', 'cardGenerationAttributes_dynamicCardVerificationCode' - Card data parameters that are required to generate CDynamic Card
-- Verification Code (dCVC) for the payment card.
--
-- 'dynamicCardVerificationValue', 'cardGenerationAttributes_dynamicCardVerificationValue' - Card data parameters that are required to generate CDynamic Card
-- Verification Value (dCVV) for the payment card.
newCardGenerationAttributes ::
  CardGenerationAttributes
newCardGenerationAttributes =
  CardGenerationAttributes'
    { amexCardSecurityCodeVersion1 =
        Prelude.Nothing,
      amexCardSecurityCodeVersion2 = Prelude.Nothing,
      cardHolderVerificationValue = Prelude.Nothing,
      cardVerificationValue1 = Prelude.Nothing,
      cardVerificationValue2 = Prelude.Nothing,
      dynamicCardVerificationCode = Prelude.Nothing,
      dynamicCardVerificationValue = Prelude.Nothing
    }

-- | Undocumented member.
cardGenerationAttributes_amexCardSecurityCodeVersion1 :: Lens.Lens' CardGenerationAttributes (Prelude.Maybe AmexCardSecurityCodeVersion1)
cardGenerationAttributes_amexCardSecurityCodeVersion1 = Lens.lens (\CardGenerationAttributes' {amexCardSecurityCodeVersion1} -> amexCardSecurityCodeVersion1) (\s@CardGenerationAttributes' {} a -> s {amexCardSecurityCodeVersion1 = a} :: CardGenerationAttributes)

-- | Card data parameters that are required to generate a Card Security Code
-- (CSC2) for an AMEX payment card.
cardGenerationAttributes_amexCardSecurityCodeVersion2 :: Lens.Lens' CardGenerationAttributes (Prelude.Maybe AmexCardSecurityCodeVersion2)
cardGenerationAttributes_amexCardSecurityCodeVersion2 = Lens.lens (\CardGenerationAttributes' {amexCardSecurityCodeVersion2} -> amexCardSecurityCodeVersion2) (\s@CardGenerationAttributes' {} a -> s {amexCardSecurityCodeVersion2 = a} :: CardGenerationAttributes)

-- | Card data parameters that are required to generate a cardholder
-- verification value for the payment card.
cardGenerationAttributes_cardHolderVerificationValue :: Lens.Lens' CardGenerationAttributes (Prelude.Maybe CardHolderVerificationValue)
cardGenerationAttributes_cardHolderVerificationValue = Lens.lens (\CardGenerationAttributes' {cardHolderVerificationValue} -> cardHolderVerificationValue) (\s@CardGenerationAttributes' {} a -> s {cardHolderVerificationValue = a} :: CardGenerationAttributes)

-- | Card data parameters that are required to generate Card Verification
-- Value (CVV) for the payment card.
cardGenerationAttributes_cardVerificationValue1 :: Lens.Lens' CardGenerationAttributes (Prelude.Maybe CardVerificationValue1)
cardGenerationAttributes_cardVerificationValue1 = Lens.lens (\CardGenerationAttributes' {cardVerificationValue1} -> cardVerificationValue1) (\s@CardGenerationAttributes' {} a -> s {cardVerificationValue1 = a} :: CardGenerationAttributes)

-- | Card data parameters that are required to generate Card Verification
-- Value (CVV2) for the payment card.
cardGenerationAttributes_cardVerificationValue2 :: Lens.Lens' CardGenerationAttributes (Prelude.Maybe CardVerificationValue2)
cardGenerationAttributes_cardVerificationValue2 = Lens.lens (\CardGenerationAttributes' {cardVerificationValue2} -> cardVerificationValue2) (\s@CardGenerationAttributes' {} a -> s {cardVerificationValue2 = a} :: CardGenerationAttributes)

-- | Card data parameters that are required to generate CDynamic Card
-- Verification Code (dCVC) for the payment card.
cardGenerationAttributes_dynamicCardVerificationCode :: Lens.Lens' CardGenerationAttributes (Prelude.Maybe DynamicCardVerificationCode)
cardGenerationAttributes_dynamicCardVerificationCode = Lens.lens (\CardGenerationAttributes' {dynamicCardVerificationCode} -> dynamicCardVerificationCode) (\s@CardGenerationAttributes' {} a -> s {dynamicCardVerificationCode = a} :: CardGenerationAttributes)

-- | Card data parameters that are required to generate CDynamic Card
-- Verification Value (dCVV) for the payment card.
cardGenerationAttributes_dynamicCardVerificationValue :: Lens.Lens' CardGenerationAttributes (Prelude.Maybe DynamicCardVerificationValue)
cardGenerationAttributes_dynamicCardVerificationValue = Lens.lens (\CardGenerationAttributes' {dynamicCardVerificationValue} -> dynamicCardVerificationValue) (\s@CardGenerationAttributes' {} a -> s {dynamicCardVerificationValue = a} :: CardGenerationAttributes)

instance Prelude.Hashable CardGenerationAttributes where
  hashWithSalt _salt CardGenerationAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` amexCardSecurityCodeVersion1
      `Prelude.hashWithSalt` amexCardSecurityCodeVersion2
      `Prelude.hashWithSalt` cardHolderVerificationValue
      `Prelude.hashWithSalt` cardVerificationValue1
      `Prelude.hashWithSalt` cardVerificationValue2
      `Prelude.hashWithSalt` dynamicCardVerificationCode
      `Prelude.hashWithSalt` dynamicCardVerificationValue

instance Prelude.NFData CardGenerationAttributes where
  rnf CardGenerationAttributes' {..} =
    Prelude.rnf amexCardSecurityCodeVersion1
      `Prelude.seq` Prelude.rnf amexCardSecurityCodeVersion2
      `Prelude.seq` Prelude.rnf cardHolderVerificationValue
      `Prelude.seq` Prelude.rnf cardVerificationValue1
      `Prelude.seq` Prelude.rnf cardVerificationValue2
      `Prelude.seq` Prelude.rnf dynamicCardVerificationCode
      `Prelude.seq` Prelude.rnf dynamicCardVerificationValue

instance Data.ToJSON CardGenerationAttributes where
  toJSON CardGenerationAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AmexCardSecurityCodeVersion1" Data..=)
              Prelude.<$> amexCardSecurityCodeVersion1,
            ("AmexCardSecurityCodeVersion2" Data..=)
              Prelude.<$> amexCardSecurityCodeVersion2,
            ("CardHolderVerificationValue" Data..=)
              Prelude.<$> cardHolderVerificationValue,
            ("CardVerificationValue1" Data..=)
              Prelude.<$> cardVerificationValue1,
            ("CardVerificationValue2" Data..=)
              Prelude.<$> cardVerificationValue2,
            ("DynamicCardVerificationCode" Data..=)
              Prelude.<$> dynamicCardVerificationCode,
            ("DynamicCardVerificationValue" Data..=)
              Prelude.<$> dynamicCardVerificationValue
          ]
      )
