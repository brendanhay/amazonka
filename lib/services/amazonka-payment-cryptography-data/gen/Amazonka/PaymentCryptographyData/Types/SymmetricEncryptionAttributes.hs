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
-- Module      : Amazonka.PaymentCryptographyData.Types.SymmetricEncryptionAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.SymmetricEncryptionAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.EncryptionMode
import Amazonka.PaymentCryptographyData.Types.PaddingType
import qualified Amazonka.Prelude as Prelude

-- | Parameters requried to encrypt plaintext data using symmetric keys.
--
-- /See:/ 'newSymmetricEncryptionAttributes' smart constructor.
data SymmetricEncryptionAttributes = SymmetricEncryptionAttributes'
  { -- | An input to cryptographic primitive used to provide the intial state.
    -- The @InitializationVector@ is typically required have a random or
    -- psuedo-random value, but sometimes it only needs to be unpredictable or
    -- unique. If a value is not provided, Amazon Web Services Payment
    -- Cryptography generates a random value.
    initializationVector :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The padding to be included with the data.
    paddingType :: Prelude.Maybe PaddingType,
    -- | The block cipher mode of operation. Block ciphers are designed to
    -- encrypt a block of data of fixed size (for example, 128 bits). The size
    -- of the input block is usually same as the size of the encrypted output
    -- block, while the key length can be different. A mode of operation
    -- describes how to repeatedly apply a cipher\'s single-block operation to
    -- securely transform amounts of data larger than a block.
    mode :: EncryptionMode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SymmetricEncryptionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initializationVector', 'symmetricEncryptionAttributes_initializationVector' - An input to cryptographic primitive used to provide the intial state.
-- The @InitializationVector@ is typically required have a random or
-- psuedo-random value, but sometimes it only needs to be unpredictable or
-- unique. If a value is not provided, Amazon Web Services Payment
-- Cryptography generates a random value.
--
-- 'paddingType', 'symmetricEncryptionAttributes_paddingType' - The padding to be included with the data.
--
-- 'mode', 'symmetricEncryptionAttributes_mode' - The block cipher mode of operation. Block ciphers are designed to
-- encrypt a block of data of fixed size (for example, 128 bits). The size
-- of the input block is usually same as the size of the encrypted output
-- block, while the key length can be different. A mode of operation
-- describes how to repeatedly apply a cipher\'s single-block operation to
-- securely transform amounts of data larger than a block.
newSymmetricEncryptionAttributes ::
  -- | 'mode'
  EncryptionMode ->
  SymmetricEncryptionAttributes
newSymmetricEncryptionAttributes pMode_ =
  SymmetricEncryptionAttributes'
    { initializationVector =
        Prelude.Nothing,
      paddingType = Prelude.Nothing,
      mode = pMode_
    }

-- | An input to cryptographic primitive used to provide the intial state.
-- The @InitializationVector@ is typically required have a random or
-- psuedo-random value, but sometimes it only needs to be unpredictable or
-- unique. If a value is not provided, Amazon Web Services Payment
-- Cryptography generates a random value.
symmetricEncryptionAttributes_initializationVector :: Lens.Lens' SymmetricEncryptionAttributes (Prelude.Maybe Prelude.Text)
symmetricEncryptionAttributes_initializationVector = Lens.lens (\SymmetricEncryptionAttributes' {initializationVector} -> initializationVector) (\s@SymmetricEncryptionAttributes' {} a -> s {initializationVector = a} :: SymmetricEncryptionAttributes) Prelude.. Lens.mapping Data._Sensitive

-- | The padding to be included with the data.
symmetricEncryptionAttributes_paddingType :: Lens.Lens' SymmetricEncryptionAttributes (Prelude.Maybe PaddingType)
symmetricEncryptionAttributes_paddingType = Lens.lens (\SymmetricEncryptionAttributes' {paddingType} -> paddingType) (\s@SymmetricEncryptionAttributes' {} a -> s {paddingType = a} :: SymmetricEncryptionAttributes)

-- | The block cipher mode of operation. Block ciphers are designed to
-- encrypt a block of data of fixed size (for example, 128 bits). The size
-- of the input block is usually same as the size of the encrypted output
-- block, while the key length can be different. A mode of operation
-- describes how to repeatedly apply a cipher\'s single-block operation to
-- securely transform amounts of data larger than a block.
symmetricEncryptionAttributes_mode :: Lens.Lens' SymmetricEncryptionAttributes EncryptionMode
symmetricEncryptionAttributes_mode = Lens.lens (\SymmetricEncryptionAttributes' {mode} -> mode) (\s@SymmetricEncryptionAttributes' {} a -> s {mode = a} :: SymmetricEncryptionAttributes)

instance
  Prelude.Hashable
    SymmetricEncryptionAttributes
  where
  hashWithSalt _salt SymmetricEncryptionAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` initializationVector
      `Prelude.hashWithSalt` paddingType
      `Prelude.hashWithSalt` mode

instance Prelude.NFData SymmetricEncryptionAttributes where
  rnf SymmetricEncryptionAttributes' {..} =
    Prelude.rnf initializationVector
      `Prelude.seq` Prelude.rnf paddingType
      `Prelude.seq` Prelude.rnf mode

instance Data.ToJSON SymmetricEncryptionAttributes where
  toJSON SymmetricEncryptionAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InitializationVector" Data..=)
              Prelude.<$> initializationVector,
            ("PaddingType" Data..=) Prelude.<$> paddingType,
            Prelude.Just ("Mode" Data..= mode)
          ]
      )
