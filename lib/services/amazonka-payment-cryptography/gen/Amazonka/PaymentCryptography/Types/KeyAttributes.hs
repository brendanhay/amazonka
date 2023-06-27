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
-- Module      : Amazonka.PaymentCryptography.Types.KeyAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.KeyAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types.KeyAlgorithm
import Amazonka.PaymentCryptography.Types.KeyClass
import Amazonka.PaymentCryptography.Types.KeyModesOfUse
import Amazonka.PaymentCryptography.Types.KeyUsage
import qualified Amazonka.Prelude as Prelude

-- | The role of the key, the algorithm it supports, and the cryptographic
-- operations allowed with the key. This data is immutable after the key is
-- created.
--
-- /See:/ 'newKeyAttributes' smart constructor.
data KeyAttributes = KeyAttributes'
  { -- | The key algorithm to be use during creation of an Amazon Web Services
    -- Payment Cryptography key.
    --
    -- For symmetric keys, Amazon Web Services Payment Cryptography supports
    -- @AES@ and @TDES@ algorithms. For asymmetric keys, Amazon Web Services
    -- Payment Cryptography supports @RSA@ and @ECC_NIST@ algorithms.
    keyAlgorithm :: KeyAlgorithm,
    -- | The type of Amazon Web Services Payment Cryptography key to create,
    -- which determines the classiﬁcation of the cryptographic method and
    -- whether Amazon Web Services Payment Cryptography key contains a
    -- symmetric key or an asymmetric key pair.
    keyClass :: KeyClass,
    -- | The list of cryptographic operations that you can perform using the key.
    keyModesOfUse :: KeyModesOfUse,
    -- | The cryptographic usage of an Amazon Web Services Payment Cryptography
    -- key as deﬁned in section A.5.2 of the TR-31 spec.
    keyUsage :: KeyUsage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyAlgorithm', 'keyAttributes_keyAlgorithm' - The key algorithm to be use during creation of an Amazon Web Services
-- Payment Cryptography key.
--
-- For symmetric keys, Amazon Web Services Payment Cryptography supports
-- @AES@ and @TDES@ algorithms. For asymmetric keys, Amazon Web Services
-- Payment Cryptography supports @RSA@ and @ECC_NIST@ algorithms.
--
-- 'keyClass', 'keyAttributes_keyClass' - The type of Amazon Web Services Payment Cryptography key to create,
-- which determines the classiﬁcation of the cryptographic method and
-- whether Amazon Web Services Payment Cryptography key contains a
-- symmetric key or an asymmetric key pair.
--
-- 'keyModesOfUse', 'keyAttributes_keyModesOfUse' - The list of cryptographic operations that you can perform using the key.
--
-- 'keyUsage', 'keyAttributes_keyUsage' - The cryptographic usage of an Amazon Web Services Payment Cryptography
-- key as deﬁned in section A.5.2 of the TR-31 spec.
newKeyAttributes ::
  -- | 'keyAlgorithm'
  KeyAlgorithm ->
  -- | 'keyClass'
  KeyClass ->
  -- | 'keyModesOfUse'
  KeyModesOfUse ->
  -- | 'keyUsage'
  KeyUsage ->
  KeyAttributes
newKeyAttributes
  pKeyAlgorithm_
  pKeyClass_
  pKeyModesOfUse_
  pKeyUsage_ =
    KeyAttributes'
      { keyAlgorithm = pKeyAlgorithm_,
        keyClass = pKeyClass_,
        keyModesOfUse = pKeyModesOfUse_,
        keyUsage = pKeyUsage_
      }

-- | The key algorithm to be use during creation of an Amazon Web Services
-- Payment Cryptography key.
--
-- For symmetric keys, Amazon Web Services Payment Cryptography supports
-- @AES@ and @TDES@ algorithms. For asymmetric keys, Amazon Web Services
-- Payment Cryptography supports @RSA@ and @ECC_NIST@ algorithms.
keyAttributes_keyAlgorithm :: Lens.Lens' KeyAttributes KeyAlgorithm
keyAttributes_keyAlgorithm = Lens.lens (\KeyAttributes' {keyAlgorithm} -> keyAlgorithm) (\s@KeyAttributes' {} a -> s {keyAlgorithm = a} :: KeyAttributes)

-- | The type of Amazon Web Services Payment Cryptography key to create,
-- which determines the classiﬁcation of the cryptographic method and
-- whether Amazon Web Services Payment Cryptography key contains a
-- symmetric key or an asymmetric key pair.
keyAttributes_keyClass :: Lens.Lens' KeyAttributes KeyClass
keyAttributes_keyClass = Lens.lens (\KeyAttributes' {keyClass} -> keyClass) (\s@KeyAttributes' {} a -> s {keyClass = a} :: KeyAttributes)

-- | The list of cryptographic operations that you can perform using the key.
keyAttributes_keyModesOfUse :: Lens.Lens' KeyAttributes KeyModesOfUse
keyAttributes_keyModesOfUse = Lens.lens (\KeyAttributes' {keyModesOfUse} -> keyModesOfUse) (\s@KeyAttributes' {} a -> s {keyModesOfUse = a} :: KeyAttributes)

-- | The cryptographic usage of an Amazon Web Services Payment Cryptography
-- key as deﬁned in section A.5.2 of the TR-31 spec.
keyAttributes_keyUsage :: Lens.Lens' KeyAttributes KeyUsage
keyAttributes_keyUsage = Lens.lens (\KeyAttributes' {keyUsage} -> keyUsage) (\s@KeyAttributes' {} a -> s {keyUsage = a} :: KeyAttributes)

instance Data.FromJSON KeyAttributes where
  parseJSON =
    Data.withObject
      "KeyAttributes"
      ( \x ->
          KeyAttributes'
            Prelude.<$> (x Data..: "KeyAlgorithm")
            Prelude.<*> (x Data..: "KeyClass")
            Prelude.<*> (x Data..: "KeyModesOfUse")
            Prelude.<*> (x Data..: "KeyUsage")
      )

instance Prelude.Hashable KeyAttributes where
  hashWithSalt _salt KeyAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` keyAlgorithm
      `Prelude.hashWithSalt` keyClass
      `Prelude.hashWithSalt` keyModesOfUse
      `Prelude.hashWithSalt` keyUsage

instance Prelude.NFData KeyAttributes where
  rnf KeyAttributes' {..} =
    Prelude.rnf keyAlgorithm
      `Prelude.seq` Prelude.rnf keyClass
      `Prelude.seq` Prelude.rnf keyModesOfUse
      `Prelude.seq` Prelude.rnf keyUsage

instance Data.ToJSON KeyAttributes where
  toJSON KeyAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("KeyAlgorithm" Data..= keyAlgorithm),
            Prelude.Just ("KeyClass" Data..= keyClass),
            Prelude.Just ("KeyModesOfUse" Data..= keyModesOfUse),
            Prelude.Just ("KeyUsage" Data..= keyUsage)
          ]
      )
