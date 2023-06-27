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
-- Module      : Amazonka.PaymentCryptographyData.Types.MacAlgorithmDukpt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.MacAlgorithmDukpt where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.DukptDerivationType
import Amazonka.PaymentCryptographyData.Types.DukptKeyVariant
import qualified Amazonka.Prelude as Prelude

-- | Parameters required for DUKPT MAC generation and verification.
--
-- /See:/ 'newMacAlgorithmDukpt' smart constructor.
data MacAlgorithmDukpt = MacAlgorithmDukpt'
  { -- | The key type derived using DUKPT from a Base Derivation Key (BDK) and
    -- Key Serial Number (KSN). This must be less than or equal to the strength
    -- of the BDK. For example, you can\'t use @AES_128@ as a derivation type
    -- for a BDK of @AES_128@ or @TDES_2KEY@.
    dukptDerivationType :: Prelude.Maybe DukptDerivationType,
    -- | The type of use of DUKPT, which can be MAC generation, MAC verification,
    -- or both.
    dukptKeyVariant :: DukptKeyVariant,
    -- | The unique identifier known as Key Serial Number (KSN) that comes from
    -- an encrypting device using DUKPT encryption method. The KSN is derived
    -- from the encrypting device unique identifier and an internal transaction
    -- counter.
    keySerialNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MacAlgorithmDukpt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dukptDerivationType', 'macAlgorithmDukpt_dukptDerivationType' - The key type derived using DUKPT from a Base Derivation Key (BDK) and
-- Key Serial Number (KSN). This must be less than or equal to the strength
-- of the BDK. For example, you can\'t use @AES_128@ as a derivation type
-- for a BDK of @AES_128@ or @TDES_2KEY@.
--
-- 'dukptKeyVariant', 'macAlgorithmDukpt_dukptKeyVariant' - The type of use of DUKPT, which can be MAC generation, MAC verification,
-- or both.
--
-- 'keySerialNumber', 'macAlgorithmDukpt_keySerialNumber' - The unique identifier known as Key Serial Number (KSN) that comes from
-- an encrypting device using DUKPT encryption method. The KSN is derived
-- from the encrypting device unique identifier and an internal transaction
-- counter.
newMacAlgorithmDukpt ::
  -- | 'dukptKeyVariant'
  DukptKeyVariant ->
  -- | 'keySerialNumber'
  Prelude.Text ->
  MacAlgorithmDukpt
newMacAlgorithmDukpt
  pDukptKeyVariant_
  pKeySerialNumber_ =
    MacAlgorithmDukpt'
      { dukptDerivationType =
          Prelude.Nothing,
        dukptKeyVariant = pDukptKeyVariant_,
        keySerialNumber = pKeySerialNumber_
      }

-- | The key type derived using DUKPT from a Base Derivation Key (BDK) and
-- Key Serial Number (KSN). This must be less than or equal to the strength
-- of the BDK. For example, you can\'t use @AES_128@ as a derivation type
-- for a BDK of @AES_128@ or @TDES_2KEY@.
macAlgorithmDukpt_dukptDerivationType :: Lens.Lens' MacAlgorithmDukpt (Prelude.Maybe DukptDerivationType)
macAlgorithmDukpt_dukptDerivationType = Lens.lens (\MacAlgorithmDukpt' {dukptDerivationType} -> dukptDerivationType) (\s@MacAlgorithmDukpt' {} a -> s {dukptDerivationType = a} :: MacAlgorithmDukpt)

-- | The type of use of DUKPT, which can be MAC generation, MAC verification,
-- or both.
macAlgorithmDukpt_dukptKeyVariant :: Lens.Lens' MacAlgorithmDukpt DukptKeyVariant
macAlgorithmDukpt_dukptKeyVariant = Lens.lens (\MacAlgorithmDukpt' {dukptKeyVariant} -> dukptKeyVariant) (\s@MacAlgorithmDukpt' {} a -> s {dukptKeyVariant = a} :: MacAlgorithmDukpt)

-- | The unique identifier known as Key Serial Number (KSN) that comes from
-- an encrypting device using DUKPT encryption method. The KSN is derived
-- from the encrypting device unique identifier and an internal transaction
-- counter.
macAlgorithmDukpt_keySerialNumber :: Lens.Lens' MacAlgorithmDukpt Prelude.Text
macAlgorithmDukpt_keySerialNumber = Lens.lens (\MacAlgorithmDukpt' {keySerialNumber} -> keySerialNumber) (\s@MacAlgorithmDukpt' {} a -> s {keySerialNumber = a} :: MacAlgorithmDukpt)

instance Prelude.Hashable MacAlgorithmDukpt where
  hashWithSalt _salt MacAlgorithmDukpt' {..} =
    _salt
      `Prelude.hashWithSalt` dukptDerivationType
      `Prelude.hashWithSalt` dukptKeyVariant
      `Prelude.hashWithSalt` keySerialNumber

instance Prelude.NFData MacAlgorithmDukpt where
  rnf MacAlgorithmDukpt' {..} =
    Prelude.rnf dukptDerivationType
      `Prelude.seq` Prelude.rnf dukptKeyVariant
      `Prelude.seq` Prelude.rnf keySerialNumber

instance Data.ToJSON MacAlgorithmDukpt where
  toJSON MacAlgorithmDukpt' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DukptDerivationType" Data..=)
              Prelude.<$> dukptDerivationType,
            Prelude.Just
              ("DukptKeyVariant" Data..= dukptKeyVariant),
            Prelude.Just
              ("KeySerialNumber" Data..= keySerialNumber)
          ]
      )
