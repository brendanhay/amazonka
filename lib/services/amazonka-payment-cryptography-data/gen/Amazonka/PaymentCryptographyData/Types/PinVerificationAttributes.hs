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
-- Module      : Amazonka.PaymentCryptographyData.Types.PinVerificationAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.PinVerificationAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.Ibm3624PinVerification
import Amazonka.PaymentCryptographyData.Types.VisaPinVerification
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required for PIN data verification.
--
-- /See:/ 'newPinVerificationAttributes' smart constructor.
data PinVerificationAttributes = PinVerificationAttributes'
  { -- | Parameters that are required to generate or verify Ibm3624 PIN.
    ibm3624Pin :: Prelude.Maybe Ibm3624PinVerification,
    -- | Parameters that are required to generate or verify Visa PIN.
    visaPin :: Prelude.Maybe VisaPinVerification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PinVerificationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ibm3624Pin', 'pinVerificationAttributes_ibm3624Pin' - Parameters that are required to generate or verify Ibm3624 PIN.
--
-- 'visaPin', 'pinVerificationAttributes_visaPin' - Parameters that are required to generate or verify Visa PIN.
newPinVerificationAttributes ::
  PinVerificationAttributes
newPinVerificationAttributes =
  PinVerificationAttributes'
    { ibm3624Pin =
        Prelude.Nothing,
      visaPin = Prelude.Nothing
    }

-- | Parameters that are required to generate or verify Ibm3624 PIN.
pinVerificationAttributes_ibm3624Pin :: Lens.Lens' PinVerificationAttributes (Prelude.Maybe Ibm3624PinVerification)
pinVerificationAttributes_ibm3624Pin = Lens.lens (\PinVerificationAttributes' {ibm3624Pin} -> ibm3624Pin) (\s@PinVerificationAttributes' {} a -> s {ibm3624Pin = a} :: PinVerificationAttributes)

-- | Parameters that are required to generate or verify Visa PIN.
pinVerificationAttributes_visaPin :: Lens.Lens' PinVerificationAttributes (Prelude.Maybe VisaPinVerification)
pinVerificationAttributes_visaPin = Lens.lens (\PinVerificationAttributes' {visaPin} -> visaPin) (\s@PinVerificationAttributes' {} a -> s {visaPin = a} :: PinVerificationAttributes)

instance Prelude.Hashable PinVerificationAttributes where
  hashWithSalt _salt PinVerificationAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` ibm3624Pin
      `Prelude.hashWithSalt` visaPin

instance Prelude.NFData PinVerificationAttributes where
  rnf PinVerificationAttributes' {..} =
    Prelude.rnf ibm3624Pin
      `Prelude.seq` Prelude.rnf visaPin

instance Data.ToJSON PinVerificationAttributes where
  toJSON PinVerificationAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Ibm3624Pin" Data..=) Prelude.<$> ibm3624Pin,
            ("VisaPin" Data..=) Prelude.<$> visaPin
          ]
      )
