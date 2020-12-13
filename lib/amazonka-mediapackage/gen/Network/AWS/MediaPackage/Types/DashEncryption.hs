{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.DashEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.DashEncryption
  ( DashEncryption (..),

    -- * Smart constructor
    mkDashEncryption,

    -- * Lenses
    deKeyRotationIntervalSeconds,
    deSpekeKeyProvider,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.SpekeKeyProvider
import qualified Network.AWS.Prelude as Lude

-- | A Dynamic Adaptive Streaming over HTTP (DASH) encryption configuration.
--
-- /See:/ 'mkDashEncryption' smart constructor.
data DashEncryption = DashEncryption'
  { -- | Time (in seconds) between each encryption key rotation.
    keyRotationIntervalSeconds :: Lude.Maybe Lude.Int,
    spekeKeyProvider :: SpekeKeyProvider
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DashEncryption' with the minimum fields required to make a request.
--
-- * 'keyRotationIntervalSeconds' - Time (in seconds) between each encryption key rotation.
-- * 'spekeKeyProvider' -
mkDashEncryption ::
  -- | 'spekeKeyProvider'
  SpekeKeyProvider ->
  DashEncryption
mkDashEncryption pSpekeKeyProvider_ =
  DashEncryption'
    { keyRotationIntervalSeconds = Lude.Nothing,
      spekeKeyProvider = pSpekeKeyProvider_
    }

-- | Time (in seconds) between each encryption key rotation.
--
-- /Note:/ Consider using 'keyRotationIntervalSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deKeyRotationIntervalSeconds :: Lens.Lens' DashEncryption (Lude.Maybe Lude.Int)
deKeyRotationIntervalSeconds = Lens.lens (keyRotationIntervalSeconds :: DashEncryption -> Lude.Maybe Lude.Int) (\s a -> s {keyRotationIntervalSeconds = a} :: DashEncryption)
{-# DEPRECATED deKeyRotationIntervalSeconds "Use generic-lens or generic-optics with 'keyRotationIntervalSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'spekeKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSpekeKeyProvider :: Lens.Lens' DashEncryption SpekeKeyProvider
deSpekeKeyProvider = Lens.lens (spekeKeyProvider :: DashEncryption -> SpekeKeyProvider) (\s a -> s {spekeKeyProvider = a} :: DashEncryption)
{-# DEPRECATED deSpekeKeyProvider "Use generic-lens or generic-optics with 'spekeKeyProvider' instead." #-}

instance Lude.FromJSON DashEncryption where
  parseJSON =
    Lude.withObject
      "DashEncryption"
      ( \x ->
          DashEncryption'
            Lude.<$> (x Lude..:? "keyRotationIntervalSeconds")
            Lude.<*> (x Lude..: "spekeKeyProvider")
      )

instance Lude.ToJSON DashEncryption where
  toJSON DashEncryption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("keyRotationIntervalSeconds" Lude..=)
              Lude.<$> keyRotationIntervalSeconds,
            Lude.Just ("spekeKeyProvider" Lude..= spekeKeyProvider)
          ]
      )
