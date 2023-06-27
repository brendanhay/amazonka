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
-- Module      : Amazonka.PaymentCryptography.Types.KeyModesOfUse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.KeyModesOfUse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The list of cryptographic operations that you can perform using the key.
-- The modes of use are deﬁned in section A.5.3 of the TR-31 spec.
--
-- /See:/ 'newKeyModesOfUse' smart constructor.
data KeyModesOfUse = KeyModesOfUse'
  { -- | Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
    -- used to decrypt data.
    decrypt :: Prelude.Maybe Prelude.Bool,
    -- | Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
    -- used to derive new keys.
    deriveKey :: Prelude.Maybe Prelude.Bool,
    -- | Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
    -- used to encrypt data.
    encrypt :: Prelude.Maybe Prelude.Bool,
    -- | Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
    -- used to generate and verify other card and PIN verification keys.
    generate :: Prelude.Maybe Prelude.Bool,
    -- | Speciﬁes whether an Amazon Web Services Payment Cryptography key has no
    -- special restrictions other than the restrictions implied by @KeyUsage@.
    noRestrictions :: Prelude.Maybe Prelude.Bool,
    -- | Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
    -- used for signing.
    sign :: Prelude.Maybe Prelude.Bool,
    -- | Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
    -- used to unwrap other keys.
    unwrap :: Prelude.Maybe Prelude.Bool,
    -- | Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
    -- used to verify signatures.
    verify :: Prelude.Maybe Prelude.Bool,
    -- | Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
    -- used to wrap other keys.
    wrap :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyModesOfUse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decrypt', 'keyModesOfUse_decrypt' - Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
-- used to decrypt data.
--
-- 'deriveKey', 'keyModesOfUse_deriveKey' - Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
-- used to derive new keys.
--
-- 'encrypt', 'keyModesOfUse_encrypt' - Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
-- used to encrypt data.
--
-- 'generate', 'keyModesOfUse_generate' - Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
-- used to generate and verify other card and PIN verification keys.
--
-- 'noRestrictions', 'keyModesOfUse_noRestrictions' - Speciﬁes whether an Amazon Web Services Payment Cryptography key has no
-- special restrictions other than the restrictions implied by @KeyUsage@.
--
-- 'sign', 'keyModesOfUse_sign' - Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
-- used for signing.
--
-- 'unwrap', 'keyModesOfUse_unwrap' - Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
-- used to unwrap other keys.
--
-- 'verify', 'keyModesOfUse_verify' - Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
-- used to verify signatures.
--
-- 'wrap', 'keyModesOfUse_wrap' - Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
-- used to wrap other keys.
newKeyModesOfUse ::
  KeyModesOfUse
newKeyModesOfUse =
  KeyModesOfUse'
    { decrypt = Prelude.Nothing,
      deriveKey = Prelude.Nothing,
      encrypt = Prelude.Nothing,
      generate = Prelude.Nothing,
      noRestrictions = Prelude.Nothing,
      sign = Prelude.Nothing,
      unwrap = Prelude.Nothing,
      verify = Prelude.Nothing,
      wrap = Prelude.Nothing
    }

-- | Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
-- used to decrypt data.
keyModesOfUse_decrypt :: Lens.Lens' KeyModesOfUse (Prelude.Maybe Prelude.Bool)
keyModesOfUse_decrypt = Lens.lens (\KeyModesOfUse' {decrypt} -> decrypt) (\s@KeyModesOfUse' {} a -> s {decrypt = a} :: KeyModesOfUse)

-- | Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
-- used to derive new keys.
keyModesOfUse_deriveKey :: Lens.Lens' KeyModesOfUse (Prelude.Maybe Prelude.Bool)
keyModesOfUse_deriveKey = Lens.lens (\KeyModesOfUse' {deriveKey} -> deriveKey) (\s@KeyModesOfUse' {} a -> s {deriveKey = a} :: KeyModesOfUse)

-- | Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
-- used to encrypt data.
keyModesOfUse_encrypt :: Lens.Lens' KeyModesOfUse (Prelude.Maybe Prelude.Bool)
keyModesOfUse_encrypt = Lens.lens (\KeyModesOfUse' {encrypt} -> encrypt) (\s@KeyModesOfUse' {} a -> s {encrypt = a} :: KeyModesOfUse)

-- | Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
-- used to generate and verify other card and PIN verification keys.
keyModesOfUse_generate :: Lens.Lens' KeyModesOfUse (Prelude.Maybe Prelude.Bool)
keyModesOfUse_generate = Lens.lens (\KeyModesOfUse' {generate} -> generate) (\s@KeyModesOfUse' {} a -> s {generate = a} :: KeyModesOfUse)

-- | Speciﬁes whether an Amazon Web Services Payment Cryptography key has no
-- special restrictions other than the restrictions implied by @KeyUsage@.
keyModesOfUse_noRestrictions :: Lens.Lens' KeyModesOfUse (Prelude.Maybe Prelude.Bool)
keyModesOfUse_noRestrictions = Lens.lens (\KeyModesOfUse' {noRestrictions} -> noRestrictions) (\s@KeyModesOfUse' {} a -> s {noRestrictions = a} :: KeyModesOfUse)

-- | Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
-- used for signing.
keyModesOfUse_sign :: Lens.Lens' KeyModesOfUse (Prelude.Maybe Prelude.Bool)
keyModesOfUse_sign = Lens.lens (\KeyModesOfUse' {sign} -> sign) (\s@KeyModesOfUse' {} a -> s {sign = a} :: KeyModesOfUse)

-- | Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
-- used to unwrap other keys.
keyModesOfUse_unwrap :: Lens.Lens' KeyModesOfUse (Prelude.Maybe Prelude.Bool)
keyModesOfUse_unwrap = Lens.lens (\KeyModesOfUse' {unwrap} -> unwrap) (\s@KeyModesOfUse' {} a -> s {unwrap = a} :: KeyModesOfUse)

-- | Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
-- used to verify signatures.
keyModesOfUse_verify :: Lens.Lens' KeyModesOfUse (Prelude.Maybe Prelude.Bool)
keyModesOfUse_verify = Lens.lens (\KeyModesOfUse' {verify} -> verify) (\s@KeyModesOfUse' {} a -> s {verify = a} :: KeyModesOfUse)

-- | Speciﬁes whether an Amazon Web Services Payment Cryptography key can be
-- used to wrap other keys.
keyModesOfUse_wrap :: Lens.Lens' KeyModesOfUse (Prelude.Maybe Prelude.Bool)
keyModesOfUse_wrap = Lens.lens (\KeyModesOfUse' {wrap} -> wrap) (\s@KeyModesOfUse' {} a -> s {wrap = a} :: KeyModesOfUse)

instance Data.FromJSON KeyModesOfUse where
  parseJSON =
    Data.withObject
      "KeyModesOfUse"
      ( \x ->
          KeyModesOfUse'
            Prelude.<$> (x Data..:? "Decrypt")
            Prelude.<*> (x Data..:? "DeriveKey")
            Prelude.<*> (x Data..:? "Encrypt")
            Prelude.<*> (x Data..:? "Generate")
            Prelude.<*> (x Data..:? "NoRestrictions")
            Prelude.<*> (x Data..:? "Sign")
            Prelude.<*> (x Data..:? "Unwrap")
            Prelude.<*> (x Data..:? "Verify")
            Prelude.<*> (x Data..:? "Wrap")
      )

instance Prelude.Hashable KeyModesOfUse where
  hashWithSalt _salt KeyModesOfUse' {..} =
    _salt
      `Prelude.hashWithSalt` decrypt
      `Prelude.hashWithSalt` deriveKey
      `Prelude.hashWithSalt` encrypt
      `Prelude.hashWithSalt` generate
      `Prelude.hashWithSalt` noRestrictions
      `Prelude.hashWithSalt` sign
      `Prelude.hashWithSalt` unwrap
      `Prelude.hashWithSalt` verify
      `Prelude.hashWithSalt` wrap

instance Prelude.NFData KeyModesOfUse where
  rnf KeyModesOfUse' {..} =
    Prelude.rnf decrypt
      `Prelude.seq` Prelude.rnf deriveKey
      `Prelude.seq` Prelude.rnf encrypt
      `Prelude.seq` Prelude.rnf generate
      `Prelude.seq` Prelude.rnf noRestrictions
      `Prelude.seq` Prelude.rnf sign
      `Prelude.seq` Prelude.rnf unwrap
      `Prelude.seq` Prelude.rnf verify
      `Prelude.seq` Prelude.rnf wrap

instance Data.ToJSON KeyModesOfUse where
  toJSON KeyModesOfUse' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Decrypt" Data..=) Prelude.<$> decrypt,
            ("DeriveKey" Data..=) Prelude.<$> deriveKey,
            ("Encrypt" Data..=) Prelude.<$> encrypt,
            ("Generate" Data..=) Prelude.<$> generate,
            ("NoRestrictions" Data..=)
              Prelude.<$> noRestrictions,
            ("Sign" Data..=) Prelude.<$> sign,
            ("Unwrap" Data..=) Prelude.<$> unwrap,
            ("Verify" Data..=) Prelude.<$> verify,
            ("Wrap" Data..=) Prelude.<$> wrap
          ]
      )
