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
-- Module      : Amazonka.PaymentCryptographyData.Types.DukptEncryptionAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.DukptEncryptionAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.DukptDerivationType
import Amazonka.PaymentCryptographyData.Types.DukptEncryptionMode
import Amazonka.PaymentCryptographyData.Types.DukptKeyVariant
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required to encrypt plaintext data using DUKPT.
--
-- /See:/ 'newDukptEncryptionAttributes' smart constructor.
data DukptEncryptionAttributes = DukptEncryptionAttributes'
  { -- | The key type encrypted using DUKPT from a Base Derivation Key (BDK) and
    -- Key Serial Number (KSN). This must be less than or equal to the strength
    -- of the BDK. For example, you can\'t use @AES_128@ as a derivation type
    -- for a BDK of @AES_128@ or @TDES_2KEY@
    dukptKeyDerivationType :: Prelude.Maybe DukptDerivationType,
    -- | The type of use of DUKPT, which can be incoming data decryption,
    -- outgoing data encryption, or both.
    dukptKeyVariant :: Prelude.Maybe DukptKeyVariant,
    -- | An input to cryptographic primitive used to provide the intial state.
    -- Typically the @InitializationVector@ must have a random or psuedo-random
    -- value, but sometimes it only needs to be unpredictable or unique. If you
    -- don\'t provide a value, Amazon Web Services Payment Cryptography
    -- generates a random value.
    initializationVector :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The block cipher mode of operation. Block ciphers are designed to
    -- encrypt a block of data of fixed size, for example, 128 bits. The size
    -- of the input block is usually same as the size of the encrypted output
    -- block, while the key length can be different. A mode of operation
    -- describes how to repeatedly apply a cipher\'s single-block operation to
    -- securely transform amounts of data larger than a block.
    --
    -- The default is CBC.
    mode :: Prelude.Maybe DukptEncryptionMode,
    -- | The unique identifier known as Key Serial Number (KSN) that comes from
    -- an encrypting device using DUKPT encryption method. The KSN is derived
    -- from the encrypting device unique identifier and an internal transaction
    -- counter.
    keySerialNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DukptEncryptionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dukptKeyDerivationType', 'dukptEncryptionAttributes_dukptKeyDerivationType' - The key type encrypted using DUKPT from a Base Derivation Key (BDK) and
-- Key Serial Number (KSN). This must be less than or equal to the strength
-- of the BDK. For example, you can\'t use @AES_128@ as a derivation type
-- for a BDK of @AES_128@ or @TDES_2KEY@
--
-- 'dukptKeyVariant', 'dukptEncryptionAttributes_dukptKeyVariant' - The type of use of DUKPT, which can be incoming data decryption,
-- outgoing data encryption, or both.
--
-- 'initializationVector', 'dukptEncryptionAttributes_initializationVector' - An input to cryptographic primitive used to provide the intial state.
-- Typically the @InitializationVector@ must have a random or psuedo-random
-- value, but sometimes it only needs to be unpredictable or unique. If you
-- don\'t provide a value, Amazon Web Services Payment Cryptography
-- generates a random value.
--
-- 'mode', 'dukptEncryptionAttributes_mode' - The block cipher mode of operation. Block ciphers are designed to
-- encrypt a block of data of fixed size, for example, 128 bits. The size
-- of the input block is usually same as the size of the encrypted output
-- block, while the key length can be different. A mode of operation
-- describes how to repeatedly apply a cipher\'s single-block operation to
-- securely transform amounts of data larger than a block.
--
-- The default is CBC.
--
-- 'keySerialNumber', 'dukptEncryptionAttributes_keySerialNumber' - The unique identifier known as Key Serial Number (KSN) that comes from
-- an encrypting device using DUKPT encryption method. The KSN is derived
-- from the encrypting device unique identifier and an internal transaction
-- counter.
newDukptEncryptionAttributes ::
  -- | 'keySerialNumber'
  Prelude.Text ->
  DukptEncryptionAttributes
newDukptEncryptionAttributes pKeySerialNumber_ =
  DukptEncryptionAttributes'
    { dukptKeyDerivationType =
        Prelude.Nothing,
      dukptKeyVariant = Prelude.Nothing,
      initializationVector = Prelude.Nothing,
      mode = Prelude.Nothing,
      keySerialNumber = pKeySerialNumber_
    }

-- | The key type encrypted using DUKPT from a Base Derivation Key (BDK) and
-- Key Serial Number (KSN). This must be less than or equal to the strength
-- of the BDK. For example, you can\'t use @AES_128@ as a derivation type
-- for a BDK of @AES_128@ or @TDES_2KEY@
dukptEncryptionAttributes_dukptKeyDerivationType :: Lens.Lens' DukptEncryptionAttributes (Prelude.Maybe DukptDerivationType)
dukptEncryptionAttributes_dukptKeyDerivationType = Lens.lens (\DukptEncryptionAttributes' {dukptKeyDerivationType} -> dukptKeyDerivationType) (\s@DukptEncryptionAttributes' {} a -> s {dukptKeyDerivationType = a} :: DukptEncryptionAttributes)

-- | The type of use of DUKPT, which can be incoming data decryption,
-- outgoing data encryption, or both.
dukptEncryptionAttributes_dukptKeyVariant :: Lens.Lens' DukptEncryptionAttributes (Prelude.Maybe DukptKeyVariant)
dukptEncryptionAttributes_dukptKeyVariant = Lens.lens (\DukptEncryptionAttributes' {dukptKeyVariant} -> dukptKeyVariant) (\s@DukptEncryptionAttributes' {} a -> s {dukptKeyVariant = a} :: DukptEncryptionAttributes)

-- | An input to cryptographic primitive used to provide the intial state.
-- Typically the @InitializationVector@ must have a random or psuedo-random
-- value, but sometimes it only needs to be unpredictable or unique. If you
-- don\'t provide a value, Amazon Web Services Payment Cryptography
-- generates a random value.
dukptEncryptionAttributes_initializationVector :: Lens.Lens' DukptEncryptionAttributes (Prelude.Maybe Prelude.Text)
dukptEncryptionAttributes_initializationVector = Lens.lens (\DukptEncryptionAttributes' {initializationVector} -> initializationVector) (\s@DukptEncryptionAttributes' {} a -> s {initializationVector = a} :: DukptEncryptionAttributes) Prelude.. Lens.mapping Data._Sensitive

-- | The block cipher mode of operation. Block ciphers are designed to
-- encrypt a block of data of fixed size, for example, 128 bits. The size
-- of the input block is usually same as the size of the encrypted output
-- block, while the key length can be different. A mode of operation
-- describes how to repeatedly apply a cipher\'s single-block operation to
-- securely transform amounts of data larger than a block.
--
-- The default is CBC.
dukptEncryptionAttributes_mode :: Lens.Lens' DukptEncryptionAttributes (Prelude.Maybe DukptEncryptionMode)
dukptEncryptionAttributes_mode = Lens.lens (\DukptEncryptionAttributes' {mode} -> mode) (\s@DukptEncryptionAttributes' {} a -> s {mode = a} :: DukptEncryptionAttributes)

-- | The unique identifier known as Key Serial Number (KSN) that comes from
-- an encrypting device using DUKPT encryption method. The KSN is derived
-- from the encrypting device unique identifier and an internal transaction
-- counter.
dukptEncryptionAttributes_keySerialNumber :: Lens.Lens' DukptEncryptionAttributes Prelude.Text
dukptEncryptionAttributes_keySerialNumber = Lens.lens (\DukptEncryptionAttributes' {keySerialNumber} -> keySerialNumber) (\s@DukptEncryptionAttributes' {} a -> s {keySerialNumber = a} :: DukptEncryptionAttributes)

instance Prelude.Hashable DukptEncryptionAttributes where
  hashWithSalt _salt DukptEncryptionAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` dukptKeyDerivationType
      `Prelude.hashWithSalt` dukptKeyVariant
      `Prelude.hashWithSalt` initializationVector
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` keySerialNumber

instance Prelude.NFData DukptEncryptionAttributes where
  rnf DukptEncryptionAttributes' {..} =
    Prelude.rnf dukptKeyDerivationType
      `Prelude.seq` Prelude.rnf dukptKeyVariant
      `Prelude.seq` Prelude.rnf initializationVector
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf keySerialNumber

instance Data.ToJSON DukptEncryptionAttributes where
  toJSON DukptEncryptionAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DukptKeyDerivationType" Data..=)
              Prelude.<$> dukptKeyDerivationType,
            ("DukptKeyVariant" Data..=)
              Prelude.<$> dukptKeyVariant,
            ("InitializationVector" Data..=)
              Prelude.<$> initializationVector,
            ("Mode" Data..=) Prelude.<$> mode,
            Prelude.Just
              ("KeySerialNumber" Data..= keySerialNumber)
          ]
      )
