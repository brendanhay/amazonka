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
-- Module      : Network.AWS.MediaPackage.Types.HlsEncryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.HlsEncryption where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.EncryptionMethod
import Network.AWS.MediaPackage.Types.SpekeKeyProvider

-- | An HTTP Live Streaming (HLS) encryption configuration.
--
-- /See:/ 'newHlsEncryption' smart constructor.
data HlsEncryption = HlsEncryption'
  { -- | When enabled, the EXT-X-KEY tag will be repeated in output manifests.
    repeatExtXKey :: Core.Maybe Core.Bool,
    -- | The encryption method to use.
    encryptionMethod :: Core.Maybe EncryptionMethod,
    -- | A constant initialization vector for encryption (optional). When not
    -- specified the initialization vector will be periodically rotated.
    constantInitializationVector :: Core.Maybe Core.Text,
    -- | Interval (in seconds) between each encryption key rotation.
    keyRotationIntervalSeconds :: Core.Maybe Core.Int,
    spekeKeyProvider :: SpekeKeyProvider
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HlsEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repeatExtXKey', 'hlsEncryption_repeatExtXKey' - When enabled, the EXT-X-KEY tag will be repeated in output manifests.
--
-- 'encryptionMethod', 'hlsEncryption_encryptionMethod' - The encryption method to use.
--
-- 'constantInitializationVector', 'hlsEncryption_constantInitializationVector' - A constant initialization vector for encryption (optional). When not
-- specified the initialization vector will be periodically rotated.
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
    { repeatExtXKey = Core.Nothing,
      encryptionMethod = Core.Nothing,
      constantInitializationVector = Core.Nothing,
      keyRotationIntervalSeconds = Core.Nothing,
      spekeKeyProvider = pSpekeKeyProvider_
    }

-- | When enabled, the EXT-X-KEY tag will be repeated in output manifests.
hlsEncryption_repeatExtXKey :: Lens.Lens' HlsEncryption (Core.Maybe Core.Bool)
hlsEncryption_repeatExtXKey = Lens.lens (\HlsEncryption' {repeatExtXKey} -> repeatExtXKey) (\s@HlsEncryption' {} a -> s {repeatExtXKey = a} :: HlsEncryption)

-- | The encryption method to use.
hlsEncryption_encryptionMethod :: Lens.Lens' HlsEncryption (Core.Maybe EncryptionMethod)
hlsEncryption_encryptionMethod = Lens.lens (\HlsEncryption' {encryptionMethod} -> encryptionMethod) (\s@HlsEncryption' {} a -> s {encryptionMethod = a} :: HlsEncryption)

-- | A constant initialization vector for encryption (optional). When not
-- specified the initialization vector will be periodically rotated.
hlsEncryption_constantInitializationVector :: Lens.Lens' HlsEncryption (Core.Maybe Core.Text)
hlsEncryption_constantInitializationVector = Lens.lens (\HlsEncryption' {constantInitializationVector} -> constantInitializationVector) (\s@HlsEncryption' {} a -> s {constantInitializationVector = a} :: HlsEncryption)

-- | Interval (in seconds) between each encryption key rotation.
hlsEncryption_keyRotationIntervalSeconds :: Lens.Lens' HlsEncryption (Core.Maybe Core.Int)
hlsEncryption_keyRotationIntervalSeconds = Lens.lens (\HlsEncryption' {keyRotationIntervalSeconds} -> keyRotationIntervalSeconds) (\s@HlsEncryption' {} a -> s {keyRotationIntervalSeconds = a} :: HlsEncryption)

-- | Undocumented member.
hlsEncryption_spekeKeyProvider :: Lens.Lens' HlsEncryption SpekeKeyProvider
hlsEncryption_spekeKeyProvider = Lens.lens (\HlsEncryption' {spekeKeyProvider} -> spekeKeyProvider) (\s@HlsEncryption' {} a -> s {spekeKeyProvider = a} :: HlsEncryption)

instance Core.FromJSON HlsEncryption where
  parseJSON =
    Core.withObject
      "HlsEncryption"
      ( \x ->
          HlsEncryption'
            Core.<$> (x Core..:? "repeatExtXKey")
            Core.<*> (x Core..:? "encryptionMethod")
            Core.<*> (x Core..:? "constantInitializationVector")
            Core.<*> (x Core..:? "keyRotationIntervalSeconds")
            Core.<*> (x Core..: "spekeKeyProvider")
      )

instance Core.Hashable HlsEncryption

instance Core.NFData HlsEncryption

instance Core.ToJSON HlsEncryption where
  toJSON HlsEncryption' {..} =
    Core.object
      ( Core.catMaybes
          [ ("repeatExtXKey" Core..=) Core.<$> repeatExtXKey,
            ("encryptionMethod" Core..=)
              Core.<$> encryptionMethod,
            ("constantInitializationVector" Core..=)
              Core.<$> constantInitializationVector,
            ("keyRotationIntervalSeconds" Core..=)
              Core.<$> keyRotationIntervalSeconds,
            Core.Just
              ("spekeKeyProvider" Core..= spekeKeyProvider)
          ]
      )
