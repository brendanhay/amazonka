{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.CmafEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.CmafEncryption
  ( CmafEncryption (..),

    -- * Smart constructor
    mkCmafEncryption,

    -- * Lenses
    ceKeyRotationIntervalSeconds,
    ceSpekeKeyProvider,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.SpekeKeyProvider
import qualified Network.AWS.Prelude as Lude

-- | A Common Media Application Format (CMAF) encryption configuration.
--
-- /See:/ 'mkCmafEncryption' smart constructor.
data CmafEncryption = CmafEncryption'
  { -- | Time (in seconds) between each encryption key rotation.
    keyRotationIntervalSeconds :: Lude.Maybe Lude.Int,
    spekeKeyProvider :: SpekeKeyProvider
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CmafEncryption' with the minimum fields required to make a request.
--
-- * 'keyRotationIntervalSeconds' - Time (in seconds) between each encryption key rotation.
-- * 'spekeKeyProvider' -
mkCmafEncryption ::
  -- | 'spekeKeyProvider'
  SpekeKeyProvider ->
  CmafEncryption
mkCmafEncryption pSpekeKeyProvider_ =
  CmafEncryption'
    { keyRotationIntervalSeconds = Lude.Nothing,
      spekeKeyProvider = pSpekeKeyProvider_
    }

-- | Time (in seconds) between each encryption key rotation.
--
-- /Note:/ Consider using 'keyRotationIntervalSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceKeyRotationIntervalSeconds :: Lens.Lens' CmafEncryption (Lude.Maybe Lude.Int)
ceKeyRotationIntervalSeconds = Lens.lens (keyRotationIntervalSeconds :: CmafEncryption -> Lude.Maybe Lude.Int) (\s a -> s {keyRotationIntervalSeconds = a} :: CmafEncryption)
{-# DEPRECATED ceKeyRotationIntervalSeconds "Use generic-lens or generic-optics with 'keyRotationIntervalSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'spekeKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceSpekeKeyProvider :: Lens.Lens' CmafEncryption SpekeKeyProvider
ceSpekeKeyProvider = Lens.lens (spekeKeyProvider :: CmafEncryption -> SpekeKeyProvider) (\s a -> s {spekeKeyProvider = a} :: CmafEncryption)
{-# DEPRECATED ceSpekeKeyProvider "Use generic-lens or generic-optics with 'spekeKeyProvider' instead." #-}

instance Lude.FromJSON CmafEncryption where
  parseJSON =
    Lude.withObject
      "CmafEncryption"
      ( \x ->
          CmafEncryption'
            Lude.<$> (x Lude..:? "keyRotationIntervalSeconds")
            Lude.<*> (x Lude..: "spekeKeyProvider")
      )

instance Lude.ToJSON CmafEncryption where
  toJSON CmafEncryption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("keyRotationIntervalSeconds" Lude..=)
              Lude.<$> keyRotationIntervalSeconds,
            Lude.Just ("spekeKeyProvider" Lude..= spekeKeyProvider)
          ]
      )
