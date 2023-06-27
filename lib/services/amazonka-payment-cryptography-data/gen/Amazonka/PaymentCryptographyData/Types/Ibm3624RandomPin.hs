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
-- Module      : Amazonka.PaymentCryptographyData.Types.Ibm3624RandomPin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.Ibm3624RandomPin where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required to generate or verify Ibm3624 random PIN.
--
-- /See:/ 'newIbm3624RandomPin' smart constructor.
data Ibm3624RandomPin = Ibm3624RandomPin'
  { -- | The decimalization table to use for IBM 3624 PIN algorithm. The table is
    -- used to convert the algorithm intermediate result from hexadecimal
    -- characters to decimal.
    decimalizationTable :: Prelude.Text,
    -- | The unique data for cardholder identification.
    pinValidationData :: Prelude.Text,
    -- | The padding character for validation data.
    pinValidationDataPadCharacter :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ibm3624RandomPin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decimalizationTable', 'ibm3624RandomPin_decimalizationTable' - The decimalization table to use for IBM 3624 PIN algorithm. The table is
-- used to convert the algorithm intermediate result from hexadecimal
-- characters to decimal.
--
-- 'pinValidationData', 'ibm3624RandomPin_pinValidationData' - The unique data for cardholder identification.
--
-- 'pinValidationDataPadCharacter', 'ibm3624RandomPin_pinValidationDataPadCharacter' - The padding character for validation data.
newIbm3624RandomPin ::
  -- | 'decimalizationTable'
  Prelude.Text ->
  -- | 'pinValidationData'
  Prelude.Text ->
  -- | 'pinValidationDataPadCharacter'
  Prelude.Text ->
  Ibm3624RandomPin
newIbm3624RandomPin
  pDecimalizationTable_
  pPinValidationData_
  pPinValidationDataPadCharacter_ =
    Ibm3624RandomPin'
      { decimalizationTable =
          pDecimalizationTable_,
        pinValidationData = pPinValidationData_,
        pinValidationDataPadCharacter =
          pPinValidationDataPadCharacter_
      }

-- | The decimalization table to use for IBM 3624 PIN algorithm. The table is
-- used to convert the algorithm intermediate result from hexadecimal
-- characters to decimal.
ibm3624RandomPin_decimalizationTable :: Lens.Lens' Ibm3624RandomPin Prelude.Text
ibm3624RandomPin_decimalizationTable = Lens.lens (\Ibm3624RandomPin' {decimalizationTable} -> decimalizationTable) (\s@Ibm3624RandomPin' {} a -> s {decimalizationTable = a} :: Ibm3624RandomPin)

-- | The unique data for cardholder identification.
ibm3624RandomPin_pinValidationData :: Lens.Lens' Ibm3624RandomPin Prelude.Text
ibm3624RandomPin_pinValidationData = Lens.lens (\Ibm3624RandomPin' {pinValidationData} -> pinValidationData) (\s@Ibm3624RandomPin' {} a -> s {pinValidationData = a} :: Ibm3624RandomPin)

-- | The padding character for validation data.
ibm3624RandomPin_pinValidationDataPadCharacter :: Lens.Lens' Ibm3624RandomPin Prelude.Text
ibm3624RandomPin_pinValidationDataPadCharacter = Lens.lens (\Ibm3624RandomPin' {pinValidationDataPadCharacter} -> pinValidationDataPadCharacter) (\s@Ibm3624RandomPin' {} a -> s {pinValidationDataPadCharacter = a} :: Ibm3624RandomPin)

instance Prelude.Hashable Ibm3624RandomPin where
  hashWithSalt _salt Ibm3624RandomPin' {..} =
    _salt
      `Prelude.hashWithSalt` decimalizationTable
      `Prelude.hashWithSalt` pinValidationData
      `Prelude.hashWithSalt` pinValidationDataPadCharacter

instance Prelude.NFData Ibm3624RandomPin where
  rnf Ibm3624RandomPin' {..} =
    Prelude.rnf decimalizationTable
      `Prelude.seq` Prelude.rnf pinValidationData
      `Prelude.seq` Prelude.rnf pinValidationDataPadCharacter

instance Data.ToJSON Ibm3624RandomPin where
  toJSON Ibm3624RandomPin' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DecimalizationTable" Data..= decimalizationTable),
            Prelude.Just
              ("PinValidationData" Data..= pinValidationData),
            Prelude.Just
              ( "PinValidationDataPadCharacter"
                  Data..= pinValidationDataPadCharacter
              )
          ]
      )
