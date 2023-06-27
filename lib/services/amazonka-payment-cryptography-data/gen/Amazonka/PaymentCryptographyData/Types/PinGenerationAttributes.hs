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
-- Module      : Amazonka.PaymentCryptographyData.Types.PinGenerationAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.PinGenerationAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.Ibm3624NaturalPin
import Amazonka.PaymentCryptographyData.Types.Ibm3624PinFromOffset
import Amazonka.PaymentCryptographyData.Types.Ibm3624PinOffset
import Amazonka.PaymentCryptographyData.Types.Ibm3624RandomPin
import Amazonka.PaymentCryptographyData.Types.VisaPin
import Amazonka.PaymentCryptographyData.Types.VisaPinVerificationValue
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required for PIN data generation.
--
-- /See:/ 'newPinGenerationAttributes' smart constructor.
data PinGenerationAttributes = PinGenerationAttributes'
  { -- | Parameters that are required to generate or verify Ibm3624 natural PIN.
    ibm3624NaturalPin :: Prelude.Maybe Ibm3624NaturalPin,
    -- | Parameters that are required to generate or verify Ibm3624 PIN from
    -- offset PIN.
    ibm3624PinFromOffset :: Prelude.Maybe Ibm3624PinFromOffset,
    -- | Parameters that are required to generate or verify Ibm3624 PIN offset
    -- PIN.
    ibm3624PinOffset :: Prelude.Maybe Ibm3624PinOffset,
    -- | Parameters that are required to generate or verify Ibm3624 random PIN.
    ibm3624RandomPin :: Prelude.Maybe Ibm3624RandomPin,
    -- | Parameters that are required to generate or verify Visa PIN.
    visaPin :: Prelude.Maybe VisaPin,
    -- | Parameters that are required to generate or verify Visa PIN Verification
    -- Value (PVV).
    visaPinVerificationValue :: Prelude.Maybe VisaPinVerificationValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PinGenerationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ibm3624NaturalPin', 'pinGenerationAttributes_ibm3624NaturalPin' - Parameters that are required to generate or verify Ibm3624 natural PIN.
--
-- 'ibm3624PinFromOffset', 'pinGenerationAttributes_ibm3624PinFromOffset' - Parameters that are required to generate or verify Ibm3624 PIN from
-- offset PIN.
--
-- 'ibm3624PinOffset', 'pinGenerationAttributes_ibm3624PinOffset' - Parameters that are required to generate or verify Ibm3624 PIN offset
-- PIN.
--
-- 'ibm3624RandomPin', 'pinGenerationAttributes_ibm3624RandomPin' - Parameters that are required to generate or verify Ibm3624 random PIN.
--
-- 'visaPin', 'pinGenerationAttributes_visaPin' - Parameters that are required to generate or verify Visa PIN.
--
-- 'visaPinVerificationValue', 'pinGenerationAttributes_visaPinVerificationValue' - Parameters that are required to generate or verify Visa PIN Verification
-- Value (PVV).
newPinGenerationAttributes ::
  PinGenerationAttributes
newPinGenerationAttributes =
  PinGenerationAttributes'
    { ibm3624NaturalPin =
        Prelude.Nothing,
      ibm3624PinFromOffset = Prelude.Nothing,
      ibm3624PinOffset = Prelude.Nothing,
      ibm3624RandomPin = Prelude.Nothing,
      visaPin = Prelude.Nothing,
      visaPinVerificationValue = Prelude.Nothing
    }

-- | Parameters that are required to generate or verify Ibm3624 natural PIN.
pinGenerationAttributes_ibm3624NaturalPin :: Lens.Lens' PinGenerationAttributes (Prelude.Maybe Ibm3624NaturalPin)
pinGenerationAttributes_ibm3624NaturalPin = Lens.lens (\PinGenerationAttributes' {ibm3624NaturalPin} -> ibm3624NaturalPin) (\s@PinGenerationAttributes' {} a -> s {ibm3624NaturalPin = a} :: PinGenerationAttributes)

-- | Parameters that are required to generate or verify Ibm3624 PIN from
-- offset PIN.
pinGenerationAttributes_ibm3624PinFromOffset :: Lens.Lens' PinGenerationAttributes (Prelude.Maybe Ibm3624PinFromOffset)
pinGenerationAttributes_ibm3624PinFromOffset = Lens.lens (\PinGenerationAttributes' {ibm3624PinFromOffset} -> ibm3624PinFromOffset) (\s@PinGenerationAttributes' {} a -> s {ibm3624PinFromOffset = a} :: PinGenerationAttributes)

-- | Parameters that are required to generate or verify Ibm3624 PIN offset
-- PIN.
pinGenerationAttributes_ibm3624PinOffset :: Lens.Lens' PinGenerationAttributes (Prelude.Maybe Ibm3624PinOffset)
pinGenerationAttributes_ibm3624PinOffset = Lens.lens (\PinGenerationAttributes' {ibm3624PinOffset} -> ibm3624PinOffset) (\s@PinGenerationAttributes' {} a -> s {ibm3624PinOffset = a} :: PinGenerationAttributes)

-- | Parameters that are required to generate or verify Ibm3624 random PIN.
pinGenerationAttributes_ibm3624RandomPin :: Lens.Lens' PinGenerationAttributes (Prelude.Maybe Ibm3624RandomPin)
pinGenerationAttributes_ibm3624RandomPin = Lens.lens (\PinGenerationAttributes' {ibm3624RandomPin} -> ibm3624RandomPin) (\s@PinGenerationAttributes' {} a -> s {ibm3624RandomPin = a} :: PinGenerationAttributes)

-- | Parameters that are required to generate or verify Visa PIN.
pinGenerationAttributes_visaPin :: Lens.Lens' PinGenerationAttributes (Prelude.Maybe VisaPin)
pinGenerationAttributes_visaPin = Lens.lens (\PinGenerationAttributes' {visaPin} -> visaPin) (\s@PinGenerationAttributes' {} a -> s {visaPin = a} :: PinGenerationAttributes)

-- | Parameters that are required to generate or verify Visa PIN Verification
-- Value (PVV).
pinGenerationAttributes_visaPinVerificationValue :: Lens.Lens' PinGenerationAttributes (Prelude.Maybe VisaPinVerificationValue)
pinGenerationAttributes_visaPinVerificationValue = Lens.lens (\PinGenerationAttributes' {visaPinVerificationValue} -> visaPinVerificationValue) (\s@PinGenerationAttributes' {} a -> s {visaPinVerificationValue = a} :: PinGenerationAttributes)

instance Prelude.Hashable PinGenerationAttributes where
  hashWithSalt _salt PinGenerationAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` ibm3624NaturalPin
      `Prelude.hashWithSalt` ibm3624PinFromOffset
      `Prelude.hashWithSalt` ibm3624PinOffset
      `Prelude.hashWithSalt` ibm3624RandomPin
      `Prelude.hashWithSalt` visaPin
      `Prelude.hashWithSalt` visaPinVerificationValue

instance Prelude.NFData PinGenerationAttributes where
  rnf PinGenerationAttributes' {..} =
    Prelude.rnf ibm3624NaturalPin
      `Prelude.seq` Prelude.rnf ibm3624PinFromOffset
      `Prelude.seq` Prelude.rnf ibm3624PinOffset
      `Prelude.seq` Prelude.rnf ibm3624RandomPin
      `Prelude.seq` Prelude.rnf visaPin
      `Prelude.seq` Prelude.rnf visaPinVerificationValue

instance Data.ToJSON PinGenerationAttributes where
  toJSON PinGenerationAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Ibm3624NaturalPin" Data..=)
              Prelude.<$> ibm3624NaturalPin,
            ("Ibm3624PinFromOffset" Data..=)
              Prelude.<$> ibm3624PinFromOffset,
            ("Ibm3624PinOffset" Data..=)
              Prelude.<$> ibm3624PinOffset,
            ("Ibm3624RandomPin" Data..=)
              Prelude.<$> ibm3624RandomPin,
            ("VisaPin" Data..=) Prelude.<$> visaPin,
            ("VisaPinVerificationValue" Data..=)
              Prelude.<$> visaPinVerificationValue
          ]
      )
