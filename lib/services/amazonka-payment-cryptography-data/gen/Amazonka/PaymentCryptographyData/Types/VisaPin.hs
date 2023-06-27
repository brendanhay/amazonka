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
-- Module      : Amazonka.PaymentCryptographyData.Types.VisaPin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.VisaPin where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required to generate or verify Visa PIN.
--
-- /See:/ 'newVisaPin' smart constructor.
data VisaPin = VisaPin'
  { -- | The value for PIN verification index. It is used in the Visa PIN
    -- algorithm to calculate the PVV (PIN Verification Value).
    pinVerificationKeyIndex :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VisaPin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pinVerificationKeyIndex', 'visaPin_pinVerificationKeyIndex' - The value for PIN verification index. It is used in the Visa PIN
-- algorithm to calculate the PVV (PIN Verification Value).
newVisaPin ::
  -- | 'pinVerificationKeyIndex'
  Prelude.Natural ->
  VisaPin
newVisaPin pPinVerificationKeyIndex_ =
  VisaPin'
    { pinVerificationKeyIndex =
        pPinVerificationKeyIndex_
    }

-- | The value for PIN verification index. It is used in the Visa PIN
-- algorithm to calculate the PVV (PIN Verification Value).
visaPin_pinVerificationKeyIndex :: Lens.Lens' VisaPin Prelude.Natural
visaPin_pinVerificationKeyIndex = Lens.lens (\VisaPin' {pinVerificationKeyIndex} -> pinVerificationKeyIndex) (\s@VisaPin' {} a -> s {pinVerificationKeyIndex = a} :: VisaPin)

instance Prelude.Hashable VisaPin where
  hashWithSalt _salt VisaPin' {..} =
    _salt
      `Prelude.hashWithSalt` pinVerificationKeyIndex

instance Prelude.NFData VisaPin where
  rnf VisaPin' {..} =
    Prelude.rnf pinVerificationKeyIndex

instance Data.ToJSON VisaPin where
  toJSON VisaPin' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "PinVerificationKeyIndex"
                  Data..= pinVerificationKeyIndex
              )
          ]
      )
