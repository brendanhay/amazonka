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
-- Module      : Amazonka.PaymentCryptographyData.Types.Ibm3624PinFromOffset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.Ibm3624PinFromOffset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required to generate or verify Ibm3624 PIN from
-- offset PIN.
--
-- /See:/ 'newIbm3624PinFromOffset' smart constructor.
data Ibm3624PinFromOffset = Ibm3624PinFromOffset'
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
-- Create a value of 'Ibm3624PinFromOffset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decimalizationTable', 'ibm3624PinFromOffset_decimalizationTable' - The decimalization table to use for IBM 3624 PIN algorithm. The table is
-- used to convert the algorithm intermediate result from hexadecimal
-- characters to decimal.
--
-- 'pinOffset', 'ibm3624PinFromOffset_pinOffset' - The PIN offset value.
--
-- 'pinValidationData', 'ibm3624PinFromOffset_pinValidationData' - The unique data for cardholder identification.
--
-- 'pinValidationDataPadCharacter', 'ibm3624PinFromOffset_pinValidationDataPadCharacter' - The padding character for validation data.
newIbm3624PinFromOffset ::
  -- | 'decimalizationTable'
  Prelude.Text ->
  -- | 'pinOffset'
  Prelude.Text ->
  -- | 'pinValidationData'
  Prelude.Text ->
  -- | 'pinValidationDataPadCharacter'
  Prelude.Text ->
  Ibm3624PinFromOffset
newIbm3624PinFromOffset
  pDecimalizationTable_
  pPinOffset_
  pPinValidationData_
  pPinValidationDataPadCharacter_ =
    Ibm3624PinFromOffset'
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
ibm3624PinFromOffset_decimalizationTable :: Lens.Lens' Ibm3624PinFromOffset Prelude.Text
ibm3624PinFromOffset_decimalizationTable = Lens.lens (\Ibm3624PinFromOffset' {decimalizationTable} -> decimalizationTable) (\s@Ibm3624PinFromOffset' {} a -> s {decimalizationTable = a} :: Ibm3624PinFromOffset)

-- | The PIN offset value.
ibm3624PinFromOffset_pinOffset :: Lens.Lens' Ibm3624PinFromOffset Prelude.Text
ibm3624PinFromOffset_pinOffset = Lens.lens (\Ibm3624PinFromOffset' {pinOffset} -> pinOffset) (\s@Ibm3624PinFromOffset' {} a -> s {pinOffset = a} :: Ibm3624PinFromOffset)

-- | The unique data for cardholder identification.
ibm3624PinFromOffset_pinValidationData :: Lens.Lens' Ibm3624PinFromOffset Prelude.Text
ibm3624PinFromOffset_pinValidationData = Lens.lens (\Ibm3624PinFromOffset' {pinValidationData} -> pinValidationData) (\s@Ibm3624PinFromOffset' {} a -> s {pinValidationData = a} :: Ibm3624PinFromOffset)

-- | The padding character for validation data.
ibm3624PinFromOffset_pinValidationDataPadCharacter :: Lens.Lens' Ibm3624PinFromOffset Prelude.Text
ibm3624PinFromOffset_pinValidationDataPadCharacter = Lens.lens (\Ibm3624PinFromOffset' {pinValidationDataPadCharacter} -> pinValidationDataPadCharacter) (\s@Ibm3624PinFromOffset' {} a -> s {pinValidationDataPadCharacter = a} :: Ibm3624PinFromOffset)

instance Prelude.Hashable Ibm3624PinFromOffset where
  hashWithSalt _salt Ibm3624PinFromOffset' {..} =
    _salt
      `Prelude.hashWithSalt` decimalizationTable
      `Prelude.hashWithSalt` pinOffset
      `Prelude.hashWithSalt` pinValidationData
      `Prelude.hashWithSalt` pinValidationDataPadCharacter

instance Prelude.NFData Ibm3624PinFromOffset where
  rnf Ibm3624PinFromOffset' {..} =
    Prelude.rnf decimalizationTable
      `Prelude.seq` Prelude.rnf pinOffset
      `Prelude.seq` Prelude.rnf pinValidationData
      `Prelude.seq` Prelude.rnf pinValidationDataPadCharacter

instance Data.ToJSON Ibm3624PinFromOffset where
  toJSON Ibm3624PinFromOffset' {..} =
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
