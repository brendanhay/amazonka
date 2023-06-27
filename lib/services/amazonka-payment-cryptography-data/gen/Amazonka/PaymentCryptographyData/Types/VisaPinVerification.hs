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
-- Module      : Amazonka.PaymentCryptographyData.Types.VisaPinVerification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.VisaPinVerification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required to generate or verify Visa PIN.
--
-- /See:/ 'newVisaPinVerification' smart constructor.
data VisaPinVerification = VisaPinVerification'
  { -- | The value for PIN verification index. It is used in the Visa PIN
    -- algorithm to calculate the PVV (PIN Verification Value).
    pinVerificationKeyIndex :: Prelude.Natural,
    -- | Parameters that are required to generate or verify Visa PVV (PIN
    -- Verification Value).
    verificationValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VisaPinVerification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pinVerificationKeyIndex', 'visaPinVerification_pinVerificationKeyIndex' - The value for PIN verification index. It is used in the Visa PIN
-- algorithm to calculate the PVV (PIN Verification Value).
--
-- 'verificationValue', 'visaPinVerification_verificationValue' - Parameters that are required to generate or verify Visa PVV (PIN
-- Verification Value).
newVisaPinVerification ::
  -- | 'pinVerificationKeyIndex'
  Prelude.Natural ->
  -- | 'verificationValue'
  Prelude.Text ->
  VisaPinVerification
newVisaPinVerification
  pPinVerificationKeyIndex_
  pVerificationValue_ =
    VisaPinVerification'
      { pinVerificationKeyIndex =
          pPinVerificationKeyIndex_,
        verificationValue = pVerificationValue_
      }

-- | The value for PIN verification index. It is used in the Visa PIN
-- algorithm to calculate the PVV (PIN Verification Value).
visaPinVerification_pinVerificationKeyIndex :: Lens.Lens' VisaPinVerification Prelude.Natural
visaPinVerification_pinVerificationKeyIndex = Lens.lens (\VisaPinVerification' {pinVerificationKeyIndex} -> pinVerificationKeyIndex) (\s@VisaPinVerification' {} a -> s {pinVerificationKeyIndex = a} :: VisaPinVerification)

-- | Parameters that are required to generate or verify Visa PVV (PIN
-- Verification Value).
visaPinVerification_verificationValue :: Lens.Lens' VisaPinVerification Prelude.Text
visaPinVerification_verificationValue = Lens.lens (\VisaPinVerification' {verificationValue} -> verificationValue) (\s@VisaPinVerification' {} a -> s {verificationValue = a} :: VisaPinVerification)

instance Prelude.Hashable VisaPinVerification where
  hashWithSalt _salt VisaPinVerification' {..} =
    _salt
      `Prelude.hashWithSalt` pinVerificationKeyIndex
      `Prelude.hashWithSalt` verificationValue

instance Prelude.NFData VisaPinVerification where
  rnf VisaPinVerification' {..} =
    Prelude.rnf pinVerificationKeyIndex
      `Prelude.seq` Prelude.rnf verificationValue

instance Data.ToJSON VisaPinVerification where
  toJSON VisaPinVerification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "PinVerificationKeyIndex"
                  Data..= pinVerificationKeyIndex
              ),
            Prelude.Just
              ("VerificationValue" Data..= verificationValue)
          ]
      )
