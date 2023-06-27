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
-- Module      : Amazonka.PaymentCryptographyData.Types.Ibm3624PinOffset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.Ibm3624PinOffset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Pparameters that are required to generate or verify Ibm3624 PIN offset
-- PIN.
--
-- /See:/ 'newIbm3624PinOffset' smart constructor.
data Ibm3624PinOffset = Ibm3624PinOffset'
  { -- | The decimalization table to use for IBM 3624 PIN algorithm. The table is
    -- used to convert the algorithm intermediate result from hexadecimal
    -- characters to decimal.
    decimalizationTable :: Prelude.Text,
    -- | The encrypted PIN block data. According to ISO 9564 standard, a PIN
    -- Block is an encoded representation of a payment card Personal Account
    -- Number (PAN) and the cardholder Personal Identification Number (PIN).
    encryptedPinBlock :: Prelude.Text,
    -- | The unique data for cardholder identification.
    pinValidationData :: Prelude.Text,
    -- | The padding character for validation data.
    pinValidationDataPadCharacter :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ibm3624PinOffset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decimalizationTable', 'ibm3624PinOffset_decimalizationTable' - The decimalization table to use for IBM 3624 PIN algorithm. The table is
-- used to convert the algorithm intermediate result from hexadecimal
-- characters to decimal.
--
-- 'encryptedPinBlock', 'ibm3624PinOffset_encryptedPinBlock' - The encrypted PIN block data. According to ISO 9564 standard, a PIN
-- Block is an encoded representation of a payment card Personal Account
-- Number (PAN) and the cardholder Personal Identification Number (PIN).
--
-- 'pinValidationData', 'ibm3624PinOffset_pinValidationData' - The unique data for cardholder identification.
--
-- 'pinValidationDataPadCharacter', 'ibm3624PinOffset_pinValidationDataPadCharacter' - The padding character for validation data.
newIbm3624PinOffset ::
  -- | 'decimalizationTable'
  Prelude.Text ->
  -- | 'encryptedPinBlock'
  Prelude.Text ->
  -- | 'pinValidationData'
  Prelude.Text ->
  -- | 'pinValidationDataPadCharacter'
  Prelude.Text ->
  Ibm3624PinOffset
newIbm3624PinOffset
  pDecimalizationTable_
  pEncryptedPinBlock_
  pPinValidationData_
  pPinValidationDataPadCharacter_ =
    Ibm3624PinOffset'
      { decimalizationTable =
          pDecimalizationTable_,
        encryptedPinBlock = pEncryptedPinBlock_,
        pinValidationData = pPinValidationData_,
        pinValidationDataPadCharacter =
          pPinValidationDataPadCharacter_
      }

-- | The decimalization table to use for IBM 3624 PIN algorithm. The table is
-- used to convert the algorithm intermediate result from hexadecimal
-- characters to decimal.
ibm3624PinOffset_decimalizationTable :: Lens.Lens' Ibm3624PinOffset Prelude.Text
ibm3624PinOffset_decimalizationTable = Lens.lens (\Ibm3624PinOffset' {decimalizationTable} -> decimalizationTable) (\s@Ibm3624PinOffset' {} a -> s {decimalizationTable = a} :: Ibm3624PinOffset)

-- | The encrypted PIN block data. According to ISO 9564 standard, a PIN
-- Block is an encoded representation of a payment card Personal Account
-- Number (PAN) and the cardholder Personal Identification Number (PIN).
ibm3624PinOffset_encryptedPinBlock :: Lens.Lens' Ibm3624PinOffset Prelude.Text
ibm3624PinOffset_encryptedPinBlock = Lens.lens (\Ibm3624PinOffset' {encryptedPinBlock} -> encryptedPinBlock) (\s@Ibm3624PinOffset' {} a -> s {encryptedPinBlock = a} :: Ibm3624PinOffset)

-- | The unique data for cardholder identification.
ibm3624PinOffset_pinValidationData :: Lens.Lens' Ibm3624PinOffset Prelude.Text
ibm3624PinOffset_pinValidationData = Lens.lens (\Ibm3624PinOffset' {pinValidationData} -> pinValidationData) (\s@Ibm3624PinOffset' {} a -> s {pinValidationData = a} :: Ibm3624PinOffset)

-- | The padding character for validation data.
ibm3624PinOffset_pinValidationDataPadCharacter :: Lens.Lens' Ibm3624PinOffset Prelude.Text
ibm3624PinOffset_pinValidationDataPadCharacter = Lens.lens (\Ibm3624PinOffset' {pinValidationDataPadCharacter} -> pinValidationDataPadCharacter) (\s@Ibm3624PinOffset' {} a -> s {pinValidationDataPadCharacter = a} :: Ibm3624PinOffset)

instance Prelude.Hashable Ibm3624PinOffset where
  hashWithSalt _salt Ibm3624PinOffset' {..} =
    _salt
      `Prelude.hashWithSalt` decimalizationTable
      `Prelude.hashWithSalt` encryptedPinBlock
      `Prelude.hashWithSalt` pinValidationData
      `Prelude.hashWithSalt` pinValidationDataPadCharacter

instance Prelude.NFData Ibm3624PinOffset where
  rnf Ibm3624PinOffset' {..} =
    Prelude.rnf decimalizationTable
      `Prelude.seq` Prelude.rnf encryptedPinBlock
      `Prelude.seq` Prelude.rnf pinValidationData
      `Prelude.seq` Prelude.rnf pinValidationDataPadCharacter

instance Data.ToJSON Ibm3624PinOffset where
  toJSON Ibm3624PinOffset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DecimalizationTable" Data..= decimalizationTable),
            Prelude.Just
              ("EncryptedPinBlock" Data..= encryptedPinBlock),
            Prelude.Just
              ("PinValidationData" Data..= pinValidationData),
            Prelude.Just
              ( "PinValidationDataPadCharacter"
                  Data..= pinValidationDataPadCharacter
              )
          ]
      )
