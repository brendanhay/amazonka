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
-- Module      : Amazonka.PaymentCryptographyData.Types.DynamicCardVerificationValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.DynamicCardVerificationValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required to generate or verify Dynamic Card
-- Verification Value (dCVV).
--
-- /See:/ 'newDynamicCardVerificationValue' smart constructor.
data DynamicCardVerificationValue = DynamicCardVerificationValue'
  { -- | The transaction counter value that comes from the terminal.
    applicationTransactionCounter :: Prelude.Text,
    -- | The expiry date of a payment card.
    cardExpiryDate :: Prelude.Text,
    -- | A number that identifies and differentiates payment cards with the same
    -- Primary Account Number (PAN).
    panSequenceNumber :: Prelude.Text,
    -- | The service code of the payment card. This is different from Card
    -- Security Code (CSC).
    serviceCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DynamicCardVerificationValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationTransactionCounter', 'dynamicCardVerificationValue_applicationTransactionCounter' - The transaction counter value that comes from the terminal.
--
-- 'cardExpiryDate', 'dynamicCardVerificationValue_cardExpiryDate' - The expiry date of a payment card.
--
-- 'panSequenceNumber', 'dynamicCardVerificationValue_panSequenceNumber' - A number that identifies and differentiates payment cards with the same
-- Primary Account Number (PAN).
--
-- 'serviceCode', 'dynamicCardVerificationValue_serviceCode' - The service code of the payment card. This is different from Card
-- Security Code (CSC).
newDynamicCardVerificationValue ::
  -- | 'applicationTransactionCounter'
  Prelude.Text ->
  -- | 'cardExpiryDate'
  Prelude.Text ->
  -- | 'panSequenceNumber'
  Prelude.Text ->
  -- | 'serviceCode'
  Prelude.Text ->
  DynamicCardVerificationValue
newDynamicCardVerificationValue
  pApplicationTransactionCounter_
  pCardExpiryDate_
  pPanSequenceNumber_
  pServiceCode_ =
    DynamicCardVerificationValue'
      { applicationTransactionCounter =
          pApplicationTransactionCounter_,
        cardExpiryDate = pCardExpiryDate_,
        panSequenceNumber = pPanSequenceNumber_,
        serviceCode = pServiceCode_
      }

-- | The transaction counter value that comes from the terminal.
dynamicCardVerificationValue_applicationTransactionCounter :: Lens.Lens' DynamicCardVerificationValue Prelude.Text
dynamicCardVerificationValue_applicationTransactionCounter = Lens.lens (\DynamicCardVerificationValue' {applicationTransactionCounter} -> applicationTransactionCounter) (\s@DynamicCardVerificationValue' {} a -> s {applicationTransactionCounter = a} :: DynamicCardVerificationValue)

-- | The expiry date of a payment card.
dynamicCardVerificationValue_cardExpiryDate :: Lens.Lens' DynamicCardVerificationValue Prelude.Text
dynamicCardVerificationValue_cardExpiryDate = Lens.lens (\DynamicCardVerificationValue' {cardExpiryDate} -> cardExpiryDate) (\s@DynamicCardVerificationValue' {} a -> s {cardExpiryDate = a} :: DynamicCardVerificationValue)

-- | A number that identifies and differentiates payment cards with the same
-- Primary Account Number (PAN).
dynamicCardVerificationValue_panSequenceNumber :: Lens.Lens' DynamicCardVerificationValue Prelude.Text
dynamicCardVerificationValue_panSequenceNumber = Lens.lens (\DynamicCardVerificationValue' {panSequenceNumber} -> panSequenceNumber) (\s@DynamicCardVerificationValue' {} a -> s {panSequenceNumber = a} :: DynamicCardVerificationValue)

-- | The service code of the payment card. This is different from Card
-- Security Code (CSC).
dynamicCardVerificationValue_serviceCode :: Lens.Lens' DynamicCardVerificationValue Prelude.Text
dynamicCardVerificationValue_serviceCode = Lens.lens (\DynamicCardVerificationValue' {serviceCode} -> serviceCode) (\s@DynamicCardVerificationValue' {} a -> s {serviceCode = a} :: DynamicCardVerificationValue)

instance
  Prelude.Hashable
    DynamicCardVerificationValue
  where
  hashWithSalt _salt DynamicCardVerificationValue' {..} =
    _salt
      `Prelude.hashWithSalt` applicationTransactionCounter
      `Prelude.hashWithSalt` cardExpiryDate
      `Prelude.hashWithSalt` panSequenceNumber
      `Prelude.hashWithSalt` serviceCode

instance Prelude.NFData DynamicCardVerificationValue where
  rnf DynamicCardVerificationValue' {..} =
    Prelude.rnf applicationTransactionCounter
      `Prelude.seq` Prelude.rnf cardExpiryDate
      `Prelude.seq` Prelude.rnf panSequenceNumber
      `Prelude.seq` Prelude.rnf serviceCode

instance Data.ToJSON DynamicCardVerificationValue where
  toJSON DynamicCardVerificationValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ApplicationTransactionCounter"
                  Data..= applicationTransactionCounter
              ),
            Prelude.Just
              ("CardExpiryDate" Data..= cardExpiryDate),
            Prelude.Just
              ("PanSequenceNumber" Data..= panSequenceNumber),
            Prelude.Just ("ServiceCode" Data..= serviceCode)
          ]
      )
