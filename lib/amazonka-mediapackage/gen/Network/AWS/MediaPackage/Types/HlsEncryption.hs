{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.HlsEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.HlsEncryption
  ( HlsEncryption (..),

    -- * Smart constructor
    mkHlsEncryption,

    -- * Lenses
    heEncryptionMethod,
    heKeyRotationIntervalSeconds,
    heConstantInitializationVector,
    heSpekeKeyProvider,
    heRepeatExtXKey,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.EncryptionMethod
import Network.AWS.MediaPackage.Types.SpekeKeyProvider
import qualified Network.AWS.Prelude as Lude

-- | An HTTP Live Streaming (HLS) encryption configuration.
--
-- /See:/ 'mkHlsEncryption' smart constructor.
data HlsEncryption = HlsEncryption'
  { -- | The encryption method to use.
    encryptionMethod :: Lude.Maybe EncryptionMethod,
    -- | Interval (in seconds) between each encryption key rotation.
    keyRotationIntervalSeconds :: Lude.Maybe Lude.Int,
    -- | A constant initialization vector for encryption (optional).
    --
    -- When not specified the initialization vector will be periodically rotated.
    constantInitializationVector :: Lude.Maybe Lude.Text,
    spekeKeyProvider :: SpekeKeyProvider,
    -- | When enabled, the EXT-X-KEY tag will be repeated in output manifests.
    repeatExtXKey :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsEncryption' with the minimum fields required to make a request.
--
-- * 'encryptionMethod' - The encryption method to use.
-- * 'keyRotationIntervalSeconds' - Interval (in seconds) between each encryption key rotation.
-- * 'constantInitializationVector' - A constant initialization vector for encryption (optional).
--
-- When not specified the initialization vector will be periodically rotated.
-- * 'spekeKeyProvider' -
-- * 'repeatExtXKey' - When enabled, the EXT-X-KEY tag will be repeated in output manifests.
mkHlsEncryption ::
  -- | 'spekeKeyProvider'
  SpekeKeyProvider ->
  HlsEncryption
mkHlsEncryption pSpekeKeyProvider_ =
  HlsEncryption'
    { encryptionMethod = Lude.Nothing,
      keyRotationIntervalSeconds = Lude.Nothing,
      constantInitializationVector = Lude.Nothing,
      spekeKeyProvider = pSpekeKeyProvider_,
      repeatExtXKey = Lude.Nothing
    }

-- | The encryption method to use.
--
-- /Note:/ Consider using 'encryptionMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heEncryptionMethod :: Lens.Lens' HlsEncryption (Lude.Maybe EncryptionMethod)
heEncryptionMethod = Lens.lens (encryptionMethod :: HlsEncryption -> Lude.Maybe EncryptionMethod) (\s a -> s {encryptionMethod = a} :: HlsEncryption)
{-# DEPRECATED heEncryptionMethod "Use generic-lens or generic-optics with 'encryptionMethod' instead." #-}

-- | Interval (in seconds) between each encryption key rotation.
--
-- /Note:/ Consider using 'keyRotationIntervalSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heKeyRotationIntervalSeconds :: Lens.Lens' HlsEncryption (Lude.Maybe Lude.Int)
heKeyRotationIntervalSeconds = Lens.lens (keyRotationIntervalSeconds :: HlsEncryption -> Lude.Maybe Lude.Int) (\s a -> s {keyRotationIntervalSeconds = a} :: HlsEncryption)
{-# DEPRECATED heKeyRotationIntervalSeconds "Use generic-lens or generic-optics with 'keyRotationIntervalSeconds' instead." #-}

-- | A constant initialization vector for encryption (optional).
--
-- When not specified the initialization vector will be periodically rotated.
--
-- /Note:/ Consider using 'constantInitializationVector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heConstantInitializationVector :: Lens.Lens' HlsEncryption (Lude.Maybe Lude.Text)
heConstantInitializationVector = Lens.lens (constantInitializationVector :: HlsEncryption -> Lude.Maybe Lude.Text) (\s a -> s {constantInitializationVector = a} :: HlsEncryption)
{-# DEPRECATED heConstantInitializationVector "Use generic-lens or generic-optics with 'constantInitializationVector' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'spekeKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heSpekeKeyProvider :: Lens.Lens' HlsEncryption SpekeKeyProvider
heSpekeKeyProvider = Lens.lens (spekeKeyProvider :: HlsEncryption -> SpekeKeyProvider) (\s a -> s {spekeKeyProvider = a} :: HlsEncryption)
{-# DEPRECATED heSpekeKeyProvider "Use generic-lens or generic-optics with 'spekeKeyProvider' instead." #-}

-- | When enabled, the EXT-X-KEY tag will be repeated in output manifests.
--
-- /Note:/ Consider using 'repeatExtXKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heRepeatExtXKey :: Lens.Lens' HlsEncryption (Lude.Maybe Lude.Bool)
heRepeatExtXKey = Lens.lens (repeatExtXKey :: HlsEncryption -> Lude.Maybe Lude.Bool) (\s a -> s {repeatExtXKey = a} :: HlsEncryption)
{-# DEPRECATED heRepeatExtXKey "Use generic-lens or generic-optics with 'repeatExtXKey' instead." #-}

instance Lude.FromJSON HlsEncryption where
  parseJSON =
    Lude.withObject
      "HlsEncryption"
      ( \x ->
          HlsEncryption'
            Lude.<$> (x Lude..:? "encryptionMethod")
            Lude.<*> (x Lude..:? "keyRotationIntervalSeconds")
            Lude.<*> (x Lude..:? "constantInitializationVector")
            Lude.<*> (x Lude..: "spekeKeyProvider")
            Lude.<*> (x Lude..:? "repeatExtXKey")
      )

instance Lude.ToJSON HlsEncryption where
  toJSON HlsEncryption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("encryptionMethod" Lude..=) Lude.<$> encryptionMethod,
            ("keyRotationIntervalSeconds" Lude..=)
              Lude.<$> keyRotationIntervalSeconds,
            ("constantInitializationVector" Lude..=)
              Lude.<$> constantInitializationVector,
            Lude.Just ("spekeKeyProvider" Lude..= spekeKeyProvider),
            ("repeatExtXKey" Lude..=) Lude.<$> repeatExtXKey
          ]
      )
