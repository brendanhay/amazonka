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
-- Module      : Amazonka.PaymentCryptographyData.Types.SessionKeyDerivation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.SessionKeyDerivation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.SessionKeyAmex
import Amazonka.PaymentCryptographyData.Types.SessionKeyEmv2000
import Amazonka.PaymentCryptographyData.Types.SessionKeyEmvCommon
import Amazonka.PaymentCryptographyData.Types.SessionKeyMastercard
import Amazonka.PaymentCryptographyData.Types.SessionKeyVisa
import qualified Amazonka.Prelude as Prelude

-- | Parameters to derive a session key for Authorization Response Cryptogram
-- (ARQC) verification.
--
-- /See:/ 'newSessionKeyDerivation' smart constructor.
data SessionKeyDerivation = SessionKeyDerivation'
  { -- | Parameters to derive session key for an Amex payment card for ARQC
    -- verification.
    amex :: Prelude.Maybe SessionKeyAmex,
    -- | Parameters to derive session key for an Emv2000 payment card for ARQC
    -- verification.
    emv2000 :: Prelude.Maybe SessionKeyEmv2000,
    -- | Parameters to derive session key for an Emv common payment card for ARQC
    -- verification.
    emvCommon :: Prelude.Maybe SessionKeyEmvCommon,
    -- | Parameters to derive session key for a Mastercard payment card for ARQC
    -- verification.
    mastercard :: Prelude.Maybe SessionKeyMastercard,
    -- | Parameters to derive session key for a Visa payment cardfor ARQC
    -- verification.
    visa :: Prelude.Maybe SessionKeyVisa
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionKeyDerivation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amex', 'sessionKeyDerivation_amex' - Parameters to derive session key for an Amex payment card for ARQC
-- verification.
--
-- 'emv2000', 'sessionKeyDerivation_emv2000' - Parameters to derive session key for an Emv2000 payment card for ARQC
-- verification.
--
-- 'emvCommon', 'sessionKeyDerivation_emvCommon' - Parameters to derive session key for an Emv common payment card for ARQC
-- verification.
--
-- 'mastercard', 'sessionKeyDerivation_mastercard' - Parameters to derive session key for a Mastercard payment card for ARQC
-- verification.
--
-- 'visa', 'sessionKeyDerivation_visa' - Parameters to derive session key for a Visa payment cardfor ARQC
-- verification.
newSessionKeyDerivation ::
  SessionKeyDerivation
newSessionKeyDerivation =
  SessionKeyDerivation'
    { amex = Prelude.Nothing,
      emv2000 = Prelude.Nothing,
      emvCommon = Prelude.Nothing,
      mastercard = Prelude.Nothing,
      visa = Prelude.Nothing
    }

-- | Parameters to derive session key for an Amex payment card for ARQC
-- verification.
sessionKeyDerivation_amex :: Lens.Lens' SessionKeyDerivation (Prelude.Maybe SessionKeyAmex)
sessionKeyDerivation_amex = Lens.lens (\SessionKeyDerivation' {amex} -> amex) (\s@SessionKeyDerivation' {} a -> s {amex = a} :: SessionKeyDerivation)

-- | Parameters to derive session key for an Emv2000 payment card for ARQC
-- verification.
sessionKeyDerivation_emv2000 :: Lens.Lens' SessionKeyDerivation (Prelude.Maybe SessionKeyEmv2000)
sessionKeyDerivation_emv2000 = Lens.lens (\SessionKeyDerivation' {emv2000} -> emv2000) (\s@SessionKeyDerivation' {} a -> s {emv2000 = a} :: SessionKeyDerivation)

-- | Parameters to derive session key for an Emv common payment card for ARQC
-- verification.
sessionKeyDerivation_emvCommon :: Lens.Lens' SessionKeyDerivation (Prelude.Maybe SessionKeyEmvCommon)
sessionKeyDerivation_emvCommon = Lens.lens (\SessionKeyDerivation' {emvCommon} -> emvCommon) (\s@SessionKeyDerivation' {} a -> s {emvCommon = a} :: SessionKeyDerivation)

-- | Parameters to derive session key for a Mastercard payment card for ARQC
-- verification.
sessionKeyDerivation_mastercard :: Lens.Lens' SessionKeyDerivation (Prelude.Maybe SessionKeyMastercard)
sessionKeyDerivation_mastercard = Lens.lens (\SessionKeyDerivation' {mastercard} -> mastercard) (\s@SessionKeyDerivation' {} a -> s {mastercard = a} :: SessionKeyDerivation)

-- | Parameters to derive session key for a Visa payment cardfor ARQC
-- verification.
sessionKeyDerivation_visa :: Lens.Lens' SessionKeyDerivation (Prelude.Maybe SessionKeyVisa)
sessionKeyDerivation_visa = Lens.lens (\SessionKeyDerivation' {visa} -> visa) (\s@SessionKeyDerivation' {} a -> s {visa = a} :: SessionKeyDerivation)

instance Prelude.Hashable SessionKeyDerivation where
  hashWithSalt _salt SessionKeyDerivation' {..} =
    _salt
      `Prelude.hashWithSalt` amex
      `Prelude.hashWithSalt` emv2000
      `Prelude.hashWithSalt` emvCommon
      `Prelude.hashWithSalt` mastercard
      `Prelude.hashWithSalt` visa

instance Prelude.NFData SessionKeyDerivation where
  rnf SessionKeyDerivation' {..} =
    Prelude.rnf amex
      `Prelude.seq` Prelude.rnf emv2000
      `Prelude.seq` Prelude.rnf emvCommon
      `Prelude.seq` Prelude.rnf mastercard
      `Prelude.seq` Prelude.rnf visa

instance Data.ToJSON SessionKeyDerivation where
  toJSON SessionKeyDerivation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Amex" Data..=) Prelude.<$> amex,
            ("Emv2000" Data..=) Prelude.<$> emv2000,
            ("EmvCommon" Data..=) Prelude.<$> emvCommon,
            ("Mastercard" Data..=) Prelude.<$> mastercard,
            ("Visa" Data..=) Prelude.<$> visa
          ]
      )
