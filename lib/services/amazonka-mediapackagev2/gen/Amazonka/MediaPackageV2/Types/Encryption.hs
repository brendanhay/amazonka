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
-- Module      : Amazonka.MediaPackageV2.Types.Encryption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.Encryption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types.EncryptionMethod
import Amazonka.MediaPackageV2.Types.SpekeKeyProvider
import qualified Amazonka.Prelude as Prelude

-- | The parameters for encrypting content.
--
-- /See:/ 'newEncryption' smart constructor.
data Encryption = Encryption'
  { -- | A 128-bit, 16-byte hex value represented by a 32-character string, used
    -- in conjunction with the key for encrypting content. If you don\'t
    -- specify a value, then MediaPackage creates the constant initialization
    -- vector (IV).
    constantInitializationVector :: Prelude.Maybe Prelude.Text,
    -- | The frequency (in seconds) of key changes for live workflows, in which
    -- content is streamed real time. The service retrieves content keys before
    -- the live content begins streaming, and then retrieves them as needed
    -- over the lifetime of the workflow. By default, key rotation is set to
    -- 300 seconds (5 minutes), the minimum rotation interval, which is
    -- equivalent to setting it to 300. If you don\'t enter an interval,
    -- content keys aren\'t rotated.
    --
    -- The following example setting causes the service to rotate keys every
    -- thirty minutes: @1800@
    keyRotationIntervalSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The encryption method to use.
    encryptionMethod :: EncryptionMethod,
    -- | The parameters for the SPEKE key provider.
    spekeKeyProvider :: SpekeKeyProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Encryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constantInitializationVector', 'encryption_constantInitializationVector' - A 128-bit, 16-byte hex value represented by a 32-character string, used
-- in conjunction with the key for encrypting content. If you don\'t
-- specify a value, then MediaPackage creates the constant initialization
-- vector (IV).
--
-- 'keyRotationIntervalSeconds', 'encryption_keyRotationIntervalSeconds' - The frequency (in seconds) of key changes for live workflows, in which
-- content is streamed real time. The service retrieves content keys before
-- the live content begins streaming, and then retrieves them as needed
-- over the lifetime of the workflow. By default, key rotation is set to
-- 300 seconds (5 minutes), the minimum rotation interval, which is
-- equivalent to setting it to 300. If you don\'t enter an interval,
-- content keys aren\'t rotated.
--
-- The following example setting causes the service to rotate keys every
-- thirty minutes: @1800@
--
-- 'encryptionMethod', 'encryption_encryptionMethod' - The encryption method to use.
--
-- 'spekeKeyProvider', 'encryption_spekeKeyProvider' - The parameters for the SPEKE key provider.
newEncryption ::
  -- | 'encryptionMethod'
  EncryptionMethod ->
  -- | 'spekeKeyProvider'
  SpekeKeyProvider ->
  Encryption
newEncryption pEncryptionMethod_ pSpekeKeyProvider_ =
  Encryption'
    { constantInitializationVector =
        Prelude.Nothing,
      keyRotationIntervalSeconds = Prelude.Nothing,
      encryptionMethod = pEncryptionMethod_,
      spekeKeyProvider = pSpekeKeyProvider_
    }

-- | A 128-bit, 16-byte hex value represented by a 32-character string, used
-- in conjunction with the key for encrypting content. If you don\'t
-- specify a value, then MediaPackage creates the constant initialization
-- vector (IV).
encryption_constantInitializationVector :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_constantInitializationVector = Lens.lens (\Encryption' {constantInitializationVector} -> constantInitializationVector) (\s@Encryption' {} a -> s {constantInitializationVector = a} :: Encryption)

-- | The frequency (in seconds) of key changes for live workflows, in which
-- content is streamed real time. The service retrieves content keys before
-- the live content begins streaming, and then retrieves them as needed
-- over the lifetime of the workflow. By default, key rotation is set to
-- 300 seconds (5 minutes), the minimum rotation interval, which is
-- equivalent to setting it to 300. If you don\'t enter an interval,
-- content keys aren\'t rotated.
--
-- The following example setting causes the service to rotate keys every
-- thirty minutes: @1800@
encryption_keyRotationIntervalSeconds :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Natural)
encryption_keyRotationIntervalSeconds = Lens.lens (\Encryption' {keyRotationIntervalSeconds} -> keyRotationIntervalSeconds) (\s@Encryption' {} a -> s {keyRotationIntervalSeconds = a} :: Encryption)

-- | The encryption method to use.
encryption_encryptionMethod :: Lens.Lens' Encryption EncryptionMethod
encryption_encryptionMethod = Lens.lens (\Encryption' {encryptionMethod} -> encryptionMethod) (\s@Encryption' {} a -> s {encryptionMethod = a} :: Encryption)

-- | The parameters for the SPEKE key provider.
encryption_spekeKeyProvider :: Lens.Lens' Encryption SpekeKeyProvider
encryption_spekeKeyProvider = Lens.lens (\Encryption' {spekeKeyProvider} -> spekeKeyProvider) (\s@Encryption' {} a -> s {spekeKeyProvider = a} :: Encryption)

instance Data.FromJSON Encryption where
  parseJSON =
    Data.withObject
      "Encryption"
      ( \x ->
          Encryption'
            Prelude.<$> (x Data..:? "ConstantInitializationVector")
            Prelude.<*> (x Data..:? "KeyRotationIntervalSeconds")
            Prelude.<*> (x Data..: "EncryptionMethod")
            Prelude.<*> (x Data..: "SpekeKeyProvider")
      )

instance Prelude.Hashable Encryption where
  hashWithSalt _salt Encryption' {..} =
    _salt
      `Prelude.hashWithSalt` constantInitializationVector
      `Prelude.hashWithSalt` keyRotationIntervalSeconds
      `Prelude.hashWithSalt` encryptionMethod
      `Prelude.hashWithSalt` spekeKeyProvider

instance Prelude.NFData Encryption where
  rnf Encryption' {..} =
    Prelude.rnf constantInitializationVector
      `Prelude.seq` Prelude.rnf keyRotationIntervalSeconds
      `Prelude.seq` Prelude.rnf encryptionMethod
      `Prelude.seq` Prelude.rnf spekeKeyProvider

instance Data.ToJSON Encryption where
  toJSON Encryption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConstantInitializationVector" Data..=)
              Prelude.<$> constantInitializationVector,
            ("KeyRotationIntervalSeconds" Data..=)
              Prelude.<$> keyRotationIntervalSeconds,
            Prelude.Just
              ("EncryptionMethod" Data..= encryptionMethod),
            Prelude.Just
              ("SpekeKeyProvider" Data..= spekeKeyProvider)
          ]
      )
