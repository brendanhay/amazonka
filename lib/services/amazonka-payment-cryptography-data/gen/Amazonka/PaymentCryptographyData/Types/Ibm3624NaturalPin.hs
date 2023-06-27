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
-- Module      : Amazonka.PaymentCryptographyData.Types.Ibm3624NaturalPin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.Ibm3624NaturalPin where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required to generate or verify Ibm3624 natural PIN.
--
-- /See:/ 'newIbm3624NaturalPin' smart constructor.
data Ibm3624NaturalPin = Ibm3624NaturalPin'
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
-- Create a value of 'Ibm3624NaturalPin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decimalizationTable', 'ibm3624NaturalPin_decimalizationTable' - The decimalization table to use for IBM 3624 PIN algorithm. The table is
-- used to convert the algorithm intermediate result from hexadecimal
-- characters to decimal.
--
-- 'pinValidationData', 'ibm3624NaturalPin_pinValidationData' - The unique data for cardholder identification.
--
-- 'pinValidationDataPadCharacter', 'ibm3624NaturalPin_pinValidationDataPadCharacter' - The padding character for validation data.
newIbm3624NaturalPin ::
  -- | 'decimalizationTable'
  Prelude.Text ->
  -- | 'pinValidationData'
  Prelude.Text ->
  -- | 'pinValidationDataPadCharacter'
  Prelude.Text ->
  Ibm3624NaturalPin
newIbm3624NaturalPin
  pDecimalizationTable_
  pPinValidationData_
  pPinValidationDataPadCharacter_ =
    Ibm3624NaturalPin'
      { decimalizationTable =
          pDecimalizationTable_,
        pinValidationData = pPinValidationData_,
        pinValidationDataPadCharacter =
          pPinValidationDataPadCharacter_
      }

-- | The decimalization table to use for IBM 3624 PIN algorithm. The table is
-- used to convert the algorithm intermediate result from hexadecimal
-- characters to decimal.
ibm3624NaturalPin_decimalizationTable :: Lens.Lens' Ibm3624NaturalPin Prelude.Text
ibm3624NaturalPin_decimalizationTable = Lens.lens (\Ibm3624NaturalPin' {decimalizationTable} -> decimalizationTable) (\s@Ibm3624NaturalPin' {} a -> s {decimalizationTable = a} :: Ibm3624NaturalPin)

-- | The unique data for cardholder identification.
ibm3624NaturalPin_pinValidationData :: Lens.Lens' Ibm3624NaturalPin Prelude.Text
ibm3624NaturalPin_pinValidationData = Lens.lens (\Ibm3624NaturalPin' {pinValidationData} -> pinValidationData) (\s@Ibm3624NaturalPin' {} a -> s {pinValidationData = a} :: Ibm3624NaturalPin)

-- | The padding character for validation data.
ibm3624NaturalPin_pinValidationDataPadCharacter :: Lens.Lens' Ibm3624NaturalPin Prelude.Text
ibm3624NaturalPin_pinValidationDataPadCharacter = Lens.lens (\Ibm3624NaturalPin' {pinValidationDataPadCharacter} -> pinValidationDataPadCharacter) (\s@Ibm3624NaturalPin' {} a -> s {pinValidationDataPadCharacter = a} :: Ibm3624NaturalPin)

instance Prelude.Hashable Ibm3624NaturalPin where
  hashWithSalt _salt Ibm3624NaturalPin' {..} =
    _salt
      `Prelude.hashWithSalt` decimalizationTable
      `Prelude.hashWithSalt` pinValidationData
      `Prelude.hashWithSalt` pinValidationDataPadCharacter

instance Prelude.NFData Ibm3624NaturalPin where
  rnf Ibm3624NaturalPin' {..} =
    Prelude.rnf decimalizationTable
      `Prelude.seq` Prelude.rnf pinValidationData
      `Prelude.seq` Prelude.rnf pinValidationDataPadCharacter

instance Data.ToJSON Ibm3624NaturalPin where
  toJSON Ibm3624NaturalPin' {..} =
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
