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
-- Module      : Amazonka.PaymentCryptographyData.Types.PinData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.PinData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required to generate, translate, or verify PIN data.
--
-- /See:/ 'newPinData' smart constructor.
data PinData = PinData'
  { -- | The PIN offset value.
    pinOffset :: Prelude.Maybe Prelude.Text,
    -- | The unique data to identify a cardholder. In most cases, this is the
    -- same as cardholder\'s Primary Account Number (PAN). If a value is not
    -- provided, it defaults to PAN.
    verificationValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PinData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pinOffset', 'pinData_pinOffset' - The PIN offset value.
--
-- 'verificationValue', 'pinData_verificationValue' - The unique data to identify a cardholder. In most cases, this is the
-- same as cardholder\'s Primary Account Number (PAN). If a value is not
-- provided, it defaults to PAN.
newPinData ::
  PinData
newPinData =
  PinData'
    { pinOffset = Prelude.Nothing,
      verificationValue = Prelude.Nothing
    }

-- | The PIN offset value.
pinData_pinOffset :: Lens.Lens' PinData (Prelude.Maybe Prelude.Text)
pinData_pinOffset = Lens.lens (\PinData' {pinOffset} -> pinOffset) (\s@PinData' {} a -> s {pinOffset = a} :: PinData)

-- | The unique data to identify a cardholder. In most cases, this is the
-- same as cardholder\'s Primary Account Number (PAN). If a value is not
-- provided, it defaults to PAN.
pinData_verificationValue :: Lens.Lens' PinData (Prelude.Maybe Prelude.Text)
pinData_verificationValue = Lens.lens (\PinData' {verificationValue} -> verificationValue) (\s@PinData' {} a -> s {verificationValue = a} :: PinData)

instance Data.FromJSON PinData where
  parseJSON =
    Data.withObject
      "PinData"
      ( \x ->
          PinData'
            Prelude.<$> (x Data..:? "PinOffset")
            Prelude.<*> (x Data..:? "VerificationValue")
      )

instance Prelude.Hashable PinData where
  hashWithSalt _salt PinData' {..} =
    _salt
      `Prelude.hashWithSalt` pinOffset
      `Prelude.hashWithSalt` verificationValue

instance Prelude.NFData PinData where
  rnf PinData' {..} =
    Prelude.rnf pinOffset
      `Prelude.seq` Prelude.rnf verificationValue
