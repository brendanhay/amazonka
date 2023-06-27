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
-- Module      : Amazonka.PaymentCryptographyData.Types.DukptDerivationAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.DukptDerivationAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.DukptDerivationType
import Amazonka.PaymentCryptographyData.Types.DukptKeyVariant
import qualified Amazonka.Prelude as Prelude

-- | Parameters required for encryption or decryption of data using DUKPT.
--
-- /See:/ 'newDukptDerivationAttributes' smart constructor.
data DukptDerivationAttributes = DukptDerivationAttributes'
  { -- | The key type derived using DUKPT from a Base Derivation Key (BDK) and
    -- Key Serial Number (KSN). This must be less than or equal to the strength
    -- of the BDK. For example, you can\'t use @AES_128@ as a derivation type
    -- for a BDK of @AES_128@ or @TDES_2KEY@
    dukptKeyDerivationType :: Prelude.Maybe DukptDerivationType,
    -- | The type of use of DUKPT, which can be for incoming data decryption,
    -- outgoing data encryption, or both.
    dukptKeyVariant :: Prelude.Maybe DukptKeyVariant,
    -- | The unique identifier known as Key Serial Number (KSN) that comes from
    -- an encrypting device using DUKPT encryption method. The KSN is derived
    -- from the encrypting device unique identifier and an internal transaction
    -- counter.
    keySerialNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DukptDerivationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dukptKeyDerivationType', 'dukptDerivationAttributes_dukptKeyDerivationType' - The key type derived using DUKPT from a Base Derivation Key (BDK) and
-- Key Serial Number (KSN). This must be less than or equal to the strength
-- of the BDK. For example, you can\'t use @AES_128@ as a derivation type
-- for a BDK of @AES_128@ or @TDES_2KEY@
--
-- 'dukptKeyVariant', 'dukptDerivationAttributes_dukptKeyVariant' - The type of use of DUKPT, which can be for incoming data decryption,
-- outgoing data encryption, or both.
--
-- 'keySerialNumber', 'dukptDerivationAttributes_keySerialNumber' - The unique identifier known as Key Serial Number (KSN) that comes from
-- an encrypting device using DUKPT encryption method. The KSN is derived
-- from the encrypting device unique identifier and an internal transaction
-- counter.
newDukptDerivationAttributes ::
  -- | 'keySerialNumber'
  Prelude.Text ->
  DukptDerivationAttributes
newDukptDerivationAttributes pKeySerialNumber_ =
  DukptDerivationAttributes'
    { dukptKeyDerivationType =
        Prelude.Nothing,
      dukptKeyVariant = Prelude.Nothing,
      keySerialNumber = pKeySerialNumber_
    }

-- | The key type derived using DUKPT from a Base Derivation Key (BDK) and
-- Key Serial Number (KSN). This must be less than or equal to the strength
-- of the BDK. For example, you can\'t use @AES_128@ as a derivation type
-- for a BDK of @AES_128@ or @TDES_2KEY@
dukptDerivationAttributes_dukptKeyDerivationType :: Lens.Lens' DukptDerivationAttributes (Prelude.Maybe DukptDerivationType)
dukptDerivationAttributes_dukptKeyDerivationType = Lens.lens (\DukptDerivationAttributes' {dukptKeyDerivationType} -> dukptKeyDerivationType) (\s@DukptDerivationAttributes' {} a -> s {dukptKeyDerivationType = a} :: DukptDerivationAttributes)

-- | The type of use of DUKPT, which can be for incoming data decryption,
-- outgoing data encryption, or both.
dukptDerivationAttributes_dukptKeyVariant :: Lens.Lens' DukptDerivationAttributes (Prelude.Maybe DukptKeyVariant)
dukptDerivationAttributes_dukptKeyVariant = Lens.lens (\DukptDerivationAttributes' {dukptKeyVariant} -> dukptKeyVariant) (\s@DukptDerivationAttributes' {} a -> s {dukptKeyVariant = a} :: DukptDerivationAttributes)

-- | The unique identifier known as Key Serial Number (KSN) that comes from
-- an encrypting device using DUKPT encryption method. The KSN is derived
-- from the encrypting device unique identifier and an internal transaction
-- counter.
dukptDerivationAttributes_keySerialNumber :: Lens.Lens' DukptDerivationAttributes Prelude.Text
dukptDerivationAttributes_keySerialNumber = Lens.lens (\DukptDerivationAttributes' {keySerialNumber} -> keySerialNumber) (\s@DukptDerivationAttributes' {} a -> s {keySerialNumber = a} :: DukptDerivationAttributes)

instance Prelude.Hashable DukptDerivationAttributes where
  hashWithSalt _salt DukptDerivationAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` dukptKeyDerivationType
      `Prelude.hashWithSalt` dukptKeyVariant
      `Prelude.hashWithSalt` keySerialNumber

instance Prelude.NFData DukptDerivationAttributes where
  rnf DukptDerivationAttributes' {..} =
    Prelude.rnf dukptKeyDerivationType
      `Prelude.seq` Prelude.rnf dukptKeyVariant
      `Prelude.seq` Prelude.rnf keySerialNumber

instance Data.ToJSON DukptDerivationAttributes where
  toJSON DukptDerivationAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DukptKeyDerivationType" Data..=)
              Prelude.<$> dukptKeyDerivationType,
            ("DukptKeyVariant" Data..=)
              Prelude.<$> dukptKeyVariant,
            Prelude.Just
              ("KeySerialNumber" Data..= keySerialNumber)
          ]
      )
