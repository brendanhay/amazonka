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
-- Module      : Amazonka.MediaPackage.Types.HlsEncryption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.HlsEncryption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types.EncryptionMethod
import Amazonka.MediaPackage.Types.SpekeKeyProvider
import qualified Amazonka.Prelude as Prelude

-- | An HTTP Live Streaming (HLS) encryption configuration.
--
-- /See:/ 'newHlsEncryption' smart constructor.
data HlsEncryption = HlsEncryption'
  { -- | A constant initialization vector for encryption (optional). When not
    -- specified the initialization vector will be periodically rotated.
    constantInitializationVector :: Prelude.Maybe Prelude.Text,
    -- | When enabled, the EXT-X-KEY tag will be repeated in output manifests.
    repeatExtXKey :: Prelude.Maybe Prelude.Bool,
    -- | The encryption method to use.
    encryptionMethod :: Prelude.Maybe EncryptionMethod,
    -- | Interval (in seconds) between each encryption key rotation.
    keyRotationIntervalSeconds :: Prelude.Maybe Prelude.Int,
    spekeKeyProvider :: SpekeKeyProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constantInitializationVector', 'hlsEncryption_constantInitializationVector' - A constant initialization vector for encryption (optional). When not
-- specified the initialization vector will be periodically rotated.
--
-- 'repeatExtXKey', 'hlsEncryption_repeatExtXKey' - When enabled, the EXT-X-KEY tag will be repeated in output manifests.
--
-- 'encryptionMethod', 'hlsEncryption_encryptionMethod' - The encryption method to use.
--
-- 'keyRotationIntervalSeconds', 'hlsEncryption_keyRotationIntervalSeconds' - Interval (in seconds) between each encryption key rotation.
--
-- 'spekeKeyProvider', 'hlsEncryption_spekeKeyProvider' - Undocumented member.
newHlsEncryption ::
  -- | 'spekeKeyProvider'
  SpekeKeyProvider ->
  HlsEncryption
newHlsEncryption pSpekeKeyProvider_ =
  HlsEncryption'
    { constantInitializationVector =
        Prelude.Nothing,
      repeatExtXKey = Prelude.Nothing,
      encryptionMethod = Prelude.Nothing,
      keyRotationIntervalSeconds = Prelude.Nothing,
      spekeKeyProvider = pSpekeKeyProvider_
    }

-- | A constant initialization vector for encryption (optional). When not
-- specified the initialization vector will be periodically rotated.
hlsEncryption_constantInitializationVector :: Lens.Lens' HlsEncryption (Prelude.Maybe Prelude.Text)
hlsEncryption_constantInitializationVector = Lens.lens (\HlsEncryption' {constantInitializationVector} -> constantInitializationVector) (\s@HlsEncryption' {} a -> s {constantInitializationVector = a} :: HlsEncryption)

-- | When enabled, the EXT-X-KEY tag will be repeated in output manifests.
hlsEncryption_repeatExtXKey :: Lens.Lens' HlsEncryption (Prelude.Maybe Prelude.Bool)
hlsEncryption_repeatExtXKey = Lens.lens (\HlsEncryption' {repeatExtXKey} -> repeatExtXKey) (\s@HlsEncryption' {} a -> s {repeatExtXKey = a} :: HlsEncryption)

-- | The encryption method to use.
hlsEncryption_encryptionMethod :: Lens.Lens' HlsEncryption (Prelude.Maybe EncryptionMethod)
hlsEncryption_encryptionMethod = Lens.lens (\HlsEncryption' {encryptionMethod} -> encryptionMethod) (\s@HlsEncryption' {} a -> s {encryptionMethod = a} :: HlsEncryption)

-- | Interval (in seconds) between each encryption key rotation.
hlsEncryption_keyRotationIntervalSeconds :: Lens.Lens' HlsEncryption (Prelude.Maybe Prelude.Int)
hlsEncryption_keyRotationIntervalSeconds = Lens.lens (\HlsEncryption' {keyRotationIntervalSeconds} -> keyRotationIntervalSeconds) (\s@HlsEncryption' {} a -> s {keyRotationIntervalSeconds = a} :: HlsEncryption)

-- | Undocumented member.
hlsEncryption_spekeKeyProvider :: Lens.Lens' HlsEncryption SpekeKeyProvider
hlsEncryption_spekeKeyProvider = Lens.lens (\HlsEncryption' {spekeKeyProvider} -> spekeKeyProvider) (\s@HlsEncryption' {} a -> s {spekeKeyProvider = a} :: HlsEncryption)

instance Data.FromJSON HlsEncryption where
  parseJSON =
    Data.withObject
      "HlsEncryption"
      ( \x ->
          HlsEncryption'
            Prelude.<$> (x Data..:? "constantInitializationVector")
            Prelude.<*> (x Data..:? "repeatExtXKey")
            Prelude.<*> (x Data..:? "encryptionMethod")
            Prelude.<*> (x Data..:? "keyRotationIntervalSeconds")
            Prelude.<*> (x Data..: "spekeKeyProvider")
      )

instance Prelude.Hashable HlsEncryption where
  hashWithSalt _salt HlsEncryption' {..} =
    _salt
      `Prelude.hashWithSalt` constantInitializationVector
      `Prelude.hashWithSalt` repeatExtXKey
      `Prelude.hashWithSalt` encryptionMethod
      `Prelude.hashWithSalt` keyRotationIntervalSeconds
      `Prelude.hashWithSalt` spekeKeyProvider

instance Prelude.NFData HlsEncryption where
  rnf HlsEncryption' {..} =
    Prelude.rnf constantInitializationVector
      `Prelude.seq` Prelude.rnf repeatExtXKey
      `Prelude.seq` Prelude.rnf encryptionMethod
      `Prelude.seq` Prelude.rnf keyRotationIntervalSeconds
      `Prelude.seq` Prelude.rnf spekeKeyProvider

instance Data.ToJSON HlsEncryption where
  toJSON HlsEncryption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("constantInitializationVector" Data..=)
              Prelude.<$> constantInitializationVector,
            ("repeatExtXKey" Data..=) Prelude.<$> repeatExtXKey,
            ("encryptionMethod" Data..=)
              Prelude.<$> encryptionMethod,
            ("keyRotationIntervalSeconds" Data..=)
              Prelude.<$> keyRotationIntervalSeconds,
            Prelude.Just
              ("spekeKeyProvider" Data..= spekeKeyProvider)
          ]
      )
