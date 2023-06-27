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
-- Module      : Amazonka.PaymentCryptographyData.Types.MacAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.MacAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.MacAlgorithm
import Amazonka.PaymentCryptographyData.Types.MacAlgorithmDukpt
import Amazonka.PaymentCryptographyData.Types.MacAlgorithmEmv
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required for DUKPT, HMAC, or EMV MAC generation or
-- verification.
--
-- /See:/ 'newMacAttributes' smart constructor.
data MacAttributes = MacAttributes'
  { -- | The encryption algorithm for MAC generation or verification.
    algorithm :: Prelude.Maybe MacAlgorithm,
    -- | Parameters that are required for MAC generation or verification using
    -- DUKPT CMAC algorithm.
    dukptCmac :: Prelude.Maybe MacAlgorithmDukpt,
    -- | Parameters that are required for MAC generation or verification using
    -- DUKPT ISO 9797 algorithm1.
    dukptIso9797Algorithm1 :: Prelude.Maybe MacAlgorithmDukpt,
    -- | Parameters that are required for MAC generation or verification using
    -- DUKPT ISO 9797 algorithm2.
    dukptIso9797Algorithm3 :: Prelude.Maybe MacAlgorithmDukpt,
    -- | Parameters that are required for MAC generation or verification using
    -- EMV MAC algorithm.
    emvMac :: Prelude.Maybe MacAlgorithmEmv
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MacAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithm', 'macAttributes_algorithm' - The encryption algorithm for MAC generation or verification.
--
-- 'dukptCmac', 'macAttributes_dukptCmac' - Parameters that are required for MAC generation or verification using
-- DUKPT CMAC algorithm.
--
-- 'dukptIso9797Algorithm1', 'macAttributes_dukptIso9797Algorithm1' - Parameters that are required for MAC generation or verification using
-- DUKPT ISO 9797 algorithm1.
--
-- 'dukptIso9797Algorithm3', 'macAttributes_dukptIso9797Algorithm3' - Parameters that are required for MAC generation or verification using
-- DUKPT ISO 9797 algorithm2.
--
-- 'emvMac', 'macAttributes_emvMac' - Parameters that are required for MAC generation or verification using
-- EMV MAC algorithm.
newMacAttributes ::
  MacAttributes
newMacAttributes =
  MacAttributes'
    { algorithm = Prelude.Nothing,
      dukptCmac = Prelude.Nothing,
      dukptIso9797Algorithm1 = Prelude.Nothing,
      dukptIso9797Algorithm3 = Prelude.Nothing,
      emvMac = Prelude.Nothing
    }

-- | The encryption algorithm for MAC generation or verification.
macAttributes_algorithm :: Lens.Lens' MacAttributes (Prelude.Maybe MacAlgorithm)
macAttributes_algorithm = Lens.lens (\MacAttributes' {algorithm} -> algorithm) (\s@MacAttributes' {} a -> s {algorithm = a} :: MacAttributes)

-- | Parameters that are required for MAC generation or verification using
-- DUKPT CMAC algorithm.
macAttributes_dukptCmac :: Lens.Lens' MacAttributes (Prelude.Maybe MacAlgorithmDukpt)
macAttributes_dukptCmac = Lens.lens (\MacAttributes' {dukptCmac} -> dukptCmac) (\s@MacAttributes' {} a -> s {dukptCmac = a} :: MacAttributes)

-- | Parameters that are required for MAC generation or verification using
-- DUKPT ISO 9797 algorithm1.
macAttributes_dukptIso9797Algorithm1 :: Lens.Lens' MacAttributes (Prelude.Maybe MacAlgorithmDukpt)
macAttributes_dukptIso9797Algorithm1 = Lens.lens (\MacAttributes' {dukptIso9797Algorithm1} -> dukptIso9797Algorithm1) (\s@MacAttributes' {} a -> s {dukptIso9797Algorithm1 = a} :: MacAttributes)

-- | Parameters that are required for MAC generation or verification using
-- DUKPT ISO 9797 algorithm2.
macAttributes_dukptIso9797Algorithm3 :: Lens.Lens' MacAttributes (Prelude.Maybe MacAlgorithmDukpt)
macAttributes_dukptIso9797Algorithm3 = Lens.lens (\MacAttributes' {dukptIso9797Algorithm3} -> dukptIso9797Algorithm3) (\s@MacAttributes' {} a -> s {dukptIso9797Algorithm3 = a} :: MacAttributes)

-- | Parameters that are required for MAC generation or verification using
-- EMV MAC algorithm.
macAttributes_emvMac :: Lens.Lens' MacAttributes (Prelude.Maybe MacAlgorithmEmv)
macAttributes_emvMac = Lens.lens (\MacAttributes' {emvMac} -> emvMac) (\s@MacAttributes' {} a -> s {emvMac = a} :: MacAttributes)

instance Prelude.Hashable MacAttributes where
  hashWithSalt _salt MacAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` algorithm
      `Prelude.hashWithSalt` dukptCmac
      `Prelude.hashWithSalt` dukptIso9797Algorithm1
      `Prelude.hashWithSalt` dukptIso9797Algorithm3
      `Prelude.hashWithSalt` emvMac

instance Prelude.NFData MacAttributes where
  rnf MacAttributes' {..} =
    Prelude.rnf algorithm
      `Prelude.seq` Prelude.rnf dukptCmac
      `Prelude.seq` Prelude.rnf dukptIso9797Algorithm1
      `Prelude.seq` Prelude.rnf dukptIso9797Algorithm3
      `Prelude.seq` Prelude.rnf emvMac

instance Data.ToJSON MacAttributes where
  toJSON MacAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Algorithm" Data..=) Prelude.<$> algorithm,
            ("DukptCmac" Data..=) Prelude.<$> dukptCmac,
            ("DukptIso9797Algorithm1" Data..=)
              Prelude.<$> dukptIso9797Algorithm1,
            ("DukptIso9797Algorithm3" Data..=)
              Prelude.<$> dukptIso9797Algorithm3,
            ("EmvMac" Data..=) Prelude.<$> emvMac
          ]
      )
