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
-- Module      : Amazonka.PaymentCryptographyData.Types.VisaPinVerificationValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.VisaPinVerificationValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required to generate or verify Visa PVV (PIN
-- Verification Value).
--
-- /See:/ 'newVisaPinVerificationValue' smart constructor.
data VisaPinVerificationValue = VisaPinVerificationValue'
  { -- | The encrypted PIN block data to verify.
    encryptedPinBlock :: Prelude.Text,
    -- | The value for PIN verification index. It is used in the Visa PIN
    -- algorithm to calculate the PVV (PIN Verification Value).
    pinVerificationKeyIndex :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VisaPinVerificationValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptedPinBlock', 'visaPinVerificationValue_encryptedPinBlock' - The encrypted PIN block data to verify.
--
-- 'pinVerificationKeyIndex', 'visaPinVerificationValue_pinVerificationKeyIndex' - The value for PIN verification index. It is used in the Visa PIN
-- algorithm to calculate the PVV (PIN Verification Value).
newVisaPinVerificationValue ::
  -- | 'encryptedPinBlock'
  Prelude.Text ->
  -- | 'pinVerificationKeyIndex'
  Prelude.Natural ->
  VisaPinVerificationValue
newVisaPinVerificationValue
  pEncryptedPinBlock_
  pPinVerificationKeyIndex_ =
    VisaPinVerificationValue'
      { encryptedPinBlock =
          pEncryptedPinBlock_,
        pinVerificationKeyIndex =
          pPinVerificationKeyIndex_
      }

-- | The encrypted PIN block data to verify.
visaPinVerificationValue_encryptedPinBlock :: Lens.Lens' VisaPinVerificationValue Prelude.Text
visaPinVerificationValue_encryptedPinBlock = Lens.lens (\VisaPinVerificationValue' {encryptedPinBlock} -> encryptedPinBlock) (\s@VisaPinVerificationValue' {} a -> s {encryptedPinBlock = a} :: VisaPinVerificationValue)

-- | The value for PIN verification index. It is used in the Visa PIN
-- algorithm to calculate the PVV (PIN Verification Value).
visaPinVerificationValue_pinVerificationKeyIndex :: Lens.Lens' VisaPinVerificationValue Prelude.Natural
visaPinVerificationValue_pinVerificationKeyIndex = Lens.lens (\VisaPinVerificationValue' {pinVerificationKeyIndex} -> pinVerificationKeyIndex) (\s@VisaPinVerificationValue' {} a -> s {pinVerificationKeyIndex = a} :: VisaPinVerificationValue)

instance Prelude.Hashable VisaPinVerificationValue where
  hashWithSalt _salt VisaPinVerificationValue' {..} =
    _salt
      `Prelude.hashWithSalt` encryptedPinBlock
      `Prelude.hashWithSalt` pinVerificationKeyIndex

instance Prelude.NFData VisaPinVerificationValue where
  rnf VisaPinVerificationValue' {..} =
    Prelude.rnf encryptedPinBlock
      `Prelude.seq` Prelude.rnf pinVerificationKeyIndex

instance Data.ToJSON VisaPinVerificationValue where
  toJSON VisaPinVerificationValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EncryptedPinBlock" Data..= encryptedPinBlock),
            Prelude.Just
              ( "PinVerificationKeyIndex"
                  Data..= pinVerificationKeyIndex
              )
          ]
      )
