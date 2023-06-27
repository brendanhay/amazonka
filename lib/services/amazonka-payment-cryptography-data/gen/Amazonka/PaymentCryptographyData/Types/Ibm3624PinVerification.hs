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
-- Module      : Amazonka.PaymentCryptographyData.Types.Ibm3624PinVerification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.Ibm3624PinVerification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required to generate or verify Ibm3624 PIN
-- verification PIN.
--
-- /See:/ 'newIbm3624PinVerification' smart constructor.
data Ibm3624PinVerification = Ibm3624PinVerification'
  { -- | The decimalization table to use for IBM 3624 PIN algorithm. The table is
    -- used to convert the algorithm intermediate result from hexadecimal
    -- characters to decimal.
    decimalizationTable :: Prelude.Text,
    -- | The PIN offset value.
    pinOffset :: Prelude.Text,
    -- | The unique data for cardholder identification.
    pinValidationData :: Prelude.Text,
    -- | The padding character for validation data.
    pinValidationDataPadCharacter :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ibm3624PinVerification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decimalizationTable', 'ibm3624PinVerification_decimalizationTable' - The decimalization table to use for IBM 3624 PIN algorithm. The table is
-- used to convert the algorithm intermediate result from hexadecimal
-- characters to decimal.
--
-- 'pinOffset', 'ibm3624PinVerification_pinOffset' - The PIN offset value.
--
-- 'pinValidationData', 'ibm3624PinVerification_pinValidationData' - The unique data for cardholder identification.
--
-- 'pinValidationDataPadCharacter', 'ibm3624PinVerification_pinValidationDataPadCharacter' - The padding character for validation data.
newIbm3624PinVerification ::
  -- | 'decimalizationTable'
  Prelude.Text ->
  -- | 'pinOffset'
  Prelude.Text ->
  -- | 'pinValidationData'
  Prelude.Text ->
  -- | 'pinValidationDataPadCharacter'
  Prelude.Text ->
  Ibm3624PinVerification
newIbm3624PinVerification
  pDecimalizationTable_
  pPinOffset_
  pPinValidationData_
  pPinValidationDataPadCharacter_ =
    Ibm3624PinVerification'
      { decimalizationTable =
          pDecimalizationTable_,
        pinOffset = pPinOffset_,
        pinValidationData = pPinValidationData_,
        pinValidationDataPadCharacter =
          pPinValidationDataPadCharacter_
      }

-- | The decimalization table to use for IBM 3624 PIN algorithm. The table is
-- used to convert the algorithm intermediate result from hexadecimal
-- characters to decimal.
ibm3624PinVerification_decimalizationTable :: Lens.Lens' Ibm3624PinVerification Prelude.Text
ibm3624PinVerification_decimalizationTable = Lens.lens (\Ibm3624PinVerification' {decimalizationTable} -> decimalizationTable) (\s@Ibm3624PinVerification' {} a -> s {decimalizationTable = a} :: Ibm3624PinVerification)

-- | The PIN offset value.
ibm3624PinVerification_pinOffset :: Lens.Lens' Ibm3624PinVerification Prelude.Text
ibm3624PinVerification_pinOffset = Lens.lens (\Ibm3624PinVerification' {pinOffset} -> pinOffset) (\s@Ibm3624PinVerification' {} a -> s {pinOffset = a} :: Ibm3624PinVerification)

-- | The unique data for cardholder identification.
ibm3624PinVerification_pinValidationData :: Lens.Lens' Ibm3624PinVerification Prelude.Text
ibm3624PinVerification_pinValidationData = Lens.lens (\Ibm3624PinVerification' {pinValidationData} -> pinValidationData) (\s@Ibm3624PinVerification' {} a -> s {pinValidationData = a} :: Ibm3624PinVerification)

-- | The padding character for validation data.
ibm3624PinVerification_pinValidationDataPadCharacter :: Lens.Lens' Ibm3624PinVerification Prelude.Text
ibm3624PinVerification_pinValidationDataPadCharacter = Lens.lens (\Ibm3624PinVerification' {pinValidationDataPadCharacter} -> pinValidationDataPadCharacter) (\s@Ibm3624PinVerification' {} a -> s {pinValidationDataPadCharacter = a} :: Ibm3624PinVerification)

instance Prelude.Hashable Ibm3624PinVerification where
  hashWithSalt _salt Ibm3624PinVerification' {..} =
    _salt
      `Prelude.hashWithSalt` decimalizationTable
      `Prelude.hashWithSalt` pinOffset
      `Prelude.hashWithSalt` pinValidationData
      `Prelude.hashWithSalt` pinValidationDataPadCharacter

instance Prelude.NFData Ibm3624PinVerification where
  rnf Ibm3624PinVerification' {..} =
    Prelude.rnf decimalizationTable
      `Prelude.seq` Prelude.rnf pinOffset
      `Prelude.seq` Prelude.rnf pinValidationData
      `Prelude.seq` Prelude.rnf pinValidationDataPadCharacter

instance Data.ToJSON Ibm3624PinVerification where
  toJSON Ibm3624PinVerification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DecimalizationTable" Data..= decimalizationTable),
            Prelude.Just ("PinOffset" Data..= pinOffset),
            Prelude.Just
              ("PinValidationData" Data..= pinValidationData),
            Prelude.Just
              ( "PinValidationDataPadCharacter"
                  Data..= pinValidationDataPadCharacter
              )
          ]
      )
