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
-- Module      : Amazonka.PaymentCryptographyData.Types.CardVerificationAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.CardVerificationAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.AmexCardSecurityCodeVersion1
import Amazonka.PaymentCryptographyData.Types.AmexCardSecurityCodeVersion2
import Amazonka.PaymentCryptographyData.Types.CardHolderVerificationValue
import Amazonka.PaymentCryptographyData.Types.CardVerificationValue1
import Amazonka.PaymentCryptographyData.Types.CardVerificationValue2
import Amazonka.PaymentCryptographyData.Types.DiscoverDynamicCardVerificationCode
import Amazonka.PaymentCryptographyData.Types.DynamicCardVerificationCode
import Amazonka.PaymentCryptographyData.Types.DynamicCardVerificationValue
import qualified Amazonka.Prelude as Prelude

-- | Card data parameters that are requried to verify Card Verification
-- Values (CVV\/CVV2), Dynamic Card Verification Values (dCVV\/dCVV2), or
-- Card Security Codes (CSC).
--
-- /See:/ 'newCardVerificationAttributes' smart constructor.
data CardVerificationAttributes = CardVerificationAttributes'
  { amexCardSecurityCodeVersion1 :: Prelude.Maybe AmexCardSecurityCodeVersion1,
    -- | Card data parameters that are required to verify a Card Security Code
    -- (CSC2) for an AMEX payment card.
    amexCardSecurityCodeVersion2 :: Prelude.Maybe AmexCardSecurityCodeVersion2,
    -- | Card data parameters that are required to verify a cardholder
    -- verification value for the payment card.
    cardHolderVerificationValue :: Prelude.Maybe CardHolderVerificationValue,
    -- | Card data parameters that are required to verify Card Verification Value
    -- (CVV) for the payment card.
    cardVerificationValue1 :: Prelude.Maybe CardVerificationValue1,
    -- | Card data parameters that are required to verify Card Verification Value
    -- (CVV2) for the payment card.
    cardVerificationValue2 :: Prelude.Maybe CardVerificationValue2,
    -- | Card data parameters that are required to verify CDynamic Card
    -- Verification Code (dCVC) for the payment card.
    discoverDynamicCardVerificationCode :: Prelude.Maybe DiscoverDynamicCardVerificationCode,
    -- | Card data parameters that are required to verify CDynamic Card
    -- Verification Code (dCVC) for the payment card.
    dynamicCardVerificationCode :: Prelude.Maybe DynamicCardVerificationCode,
    -- | Card data parameters that are required to verify CDynamic Card
    -- Verification Value (dCVV) for the payment card.
    dynamicCardVerificationValue :: Prelude.Maybe DynamicCardVerificationValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CardVerificationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amexCardSecurityCodeVersion1', 'cardVerificationAttributes_amexCardSecurityCodeVersion1' - Undocumented member.
--
-- 'amexCardSecurityCodeVersion2', 'cardVerificationAttributes_amexCardSecurityCodeVersion2' - Card data parameters that are required to verify a Card Security Code
-- (CSC2) for an AMEX payment card.
--
-- 'cardHolderVerificationValue', 'cardVerificationAttributes_cardHolderVerificationValue' - Card data parameters that are required to verify a cardholder
-- verification value for the payment card.
--
-- 'cardVerificationValue1', 'cardVerificationAttributes_cardVerificationValue1' - Card data parameters that are required to verify Card Verification Value
-- (CVV) for the payment card.
--
-- 'cardVerificationValue2', 'cardVerificationAttributes_cardVerificationValue2' - Card data parameters that are required to verify Card Verification Value
-- (CVV2) for the payment card.
--
-- 'discoverDynamicCardVerificationCode', 'cardVerificationAttributes_discoverDynamicCardVerificationCode' - Card data parameters that are required to verify CDynamic Card
-- Verification Code (dCVC) for the payment card.
--
-- 'dynamicCardVerificationCode', 'cardVerificationAttributes_dynamicCardVerificationCode' - Card data parameters that are required to verify CDynamic Card
-- Verification Code (dCVC) for the payment card.
--
-- 'dynamicCardVerificationValue', 'cardVerificationAttributes_dynamicCardVerificationValue' - Card data parameters that are required to verify CDynamic Card
-- Verification Value (dCVV) for the payment card.
newCardVerificationAttributes ::
  CardVerificationAttributes
newCardVerificationAttributes =
  CardVerificationAttributes'
    { amexCardSecurityCodeVersion1 =
        Prelude.Nothing,
      amexCardSecurityCodeVersion2 = Prelude.Nothing,
      cardHolderVerificationValue = Prelude.Nothing,
      cardVerificationValue1 = Prelude.Nothing,
      cardVerificationValue2 = Prelude.Nothing,
      discoverDynamicCardVerificationCode =
        Prelude.Nothing,
      dynamicCardVerificationCode = Prelude.Nothing,
      dynamicCardVerificationValue = Prelude.Nothing
    }

-- | Undocumented member.
cardVerificationAttributes_amexCardSecurityCodeVersion1 :: Lens.Lens' CardVerificationAttributes (Prelude.Maybe AmexCardSecurityCodeVersion1)
cardVerificationAttributes_amexCardSecurityCodeVersion1 = Lens.lens (\CardVerificationAttributes' {amexCardSecurityCodeVersion1} -> amexCardSecurityCodeVersion1) (\s@CardVerificationAttributes' {} a -> s {amexCardSecurityCodeVersion1 = a} :: CardVerificationAttributes)

-- | Card data parameters that are required to verify a Card Security Code
-- (CSC2) for an AMEX payment card.
cardVerificationAttributes_amexCardSecurityCodeVersion2 :: Lens.Lens' CardVerificationAttributes (Prelude.Maybe AmexCardSecurityCodeVersion2)
cardVerificationAttributes_amexCardSecurityCodeVersion2 = Lens.lens (\CardVerificationAttributes' {amexCardSecurityCodeVersion2} -> amexCardSecurityCodeVersion2) (\s@CardVerificationAttributes' {} a -> s {amexCardSecurityCodeVersion2 = a} :: CardVerificationAttributes)

-- | Card data parameters that are required to verify a cardholder
-- verification value for the payment card.
cardVerificationAttributes_cardHolderVerificationValue :: Lens.Lens' CardVerificationAttributes (Prelude.Maybe CardHolderVerificationValue)
cardVerificationAttributes_cardHolderVerificationValue = Lens.lens (\CardVerificationAttributes' {cardHolderVerificationValue} -> cardHolderVerificationValue) (\s@CardVerificationAttributes' {} a -> s {cardHolderVerificationValue = a} :: CardVerificationAttributes)

-- | Card data parameters that are required to verify Card Verification Value
-- (CVV) for the payment card.
cardVerificationAttributes_cardVerificationValue1 :: Lens.Lens' CardVerificationAttributes (Prelude.Maybe CardVerificationValue1)
cardVerificationAttributes_cardVerificationValue1 = Lens.lens (\CardVerificationAttributes' {cardVerificationValue1} -> cardVerificationValue1) (\s@CardVerificationAttributes' {} a -> s {cardVerificationValue1 = a} :: CardVerificationAttributes)

-- | Card data parameters that are required to verify Card Verification Value
-- (CVV2) for the payment card.
cardVerificationAttributes_cardVerificationValue2 :: Lens.Lens' CardVerificationAttributes (Prelude.Maybe CardVerificationValue2)
cardVerificationAttributes_cardVerificationValue2 = Lens.lens (\CardVerificationAttributes' {cardVerificationValue2} -> cardVerificationValue2) (\s@CardVerificationAttributes' {} a -> s {cardVerificationValue2 = a} :: CardVerificationAttributes)

-- | Card data parameters that are required to verify CDynamic Card
-- Verification Code (dCVC) for the payment card.
cardVerificationAttributes_discoverDynamicCardVerificationCode :: Lens.Lens' CardVerificationAttributes (Prelude.Maybe DiscoverDynamicCardVerificationCode)
cardVerificationAttributes_discoverDynamicCardVerificationCode = Lens.lens (\CardVerificationAttributes' {discoverDynamicCardVerificationCode} -> discoverDynamicCardVerificationCode) (\s@CardVerificationAttributes' {} a -> s {discoverDynamicCardVerificationCode = a} :: CardVerificationAttributes)

-- | Card data parameters that are required to verify CDynamic Card
-- Verification Code (dCVC) for the payment card.
cardVerificationAttributes_dynamicCardVerificationCode :: Lens.Lens' CardVerificationAttributes (Prelude.Maybe DynamicCardVerificationCode)
cardVerificationAttributes_dynamicCardVerificationCode = Lens.lens (\CardVerificationAttributes' {dynamicCardVerificationCode} -> dynamicCardVerificationCode) (\s@CardVerificationAttributes' {} a -> s {dynamicCardVerificationCode = a} :: CardVerificationAttributes)

-- | Card data parameters that are required to verify CDynamic Card
-- Verification Value (dCVV) for the payment card.
cardVerificationAttributes_dynamicCardVerificationValue :: Lens.Lens' CardVerificationAttributes (Prelude.Maybe DynamicCardVerificationValue)
cardVerificationAttributes_dynamicCardVerificationValue = Lens.lens (\CardVerificationAttributes' {dynamicCardVerificationValue} -> dynamicCardVerificationValue) (\s@CardVerificationAttributes' {} a -> s {dynamicCardVerificationValue = a} :: CardVerificationAttributes)

instance Prelude.Hashable CardVerificationAttributes where
  hashWithSalt _salt CardVerificationAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` amexCardSecurityCodeVersion1
      `Prelude.hashWithSalt` amexCardSecurityCodeVersion2
      `Prelude.hashWithSalt` cardHolderVerificationValue
      `Prelude.hashWithSalt` cardVerificationValue1
      `Prelude.hashWithSalt` cardVerificationValue2
      `Prelude.hashWithSalt` discoverDynamicCardVerificationCode
      `Prelude.hashWithSalt` dynamicCardVerificationCode
      `Prelude.hashWithSalt` dynamicCardVerificationValue

instance Prelude.NFData CardVerificationAttributes where
  rnf CardVerificationAttributes' {..} =
    Prelude.rnf amexCardSecurityCodeVersion1
      `Prelude.seq` Prelude.rnf amexCardSecurityCodeVersion2
      `Prelude.seq` Prelude.rnf cardHolderVerificationValue
      `Prelude.seq` Prelude.rnf cardVerificationValue1
      `Prelude.seq` Prelude.rnf cardVerificationValue2
      `Prelude.seq` Prelude.rnf discoverDynamicCardVerificationCode
      `Prelude.seq` Prelude.rnf dynamicCardVerificationCode
      `Prelude.seq` Prelude.rnf dynamicCardVerificationValue

instance Data.ToJSON CardVerificationAttributes where
  toJSON CardVerificationAttributes' {..} =
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
            ("DiscoverDynamicCardVerificationCode" Data..=)
              Prelude.<$> discoverDynamicCardVerificationCode,
            ("DynamicCardVerificationCode" Data..=)
              Prelude.<$> dynamicCardVerificationCode,
            ("DynamicCardVerificationValue" Data..=)
              Prelude.<$> dynamicCardVerificationValue
          ]
      )
