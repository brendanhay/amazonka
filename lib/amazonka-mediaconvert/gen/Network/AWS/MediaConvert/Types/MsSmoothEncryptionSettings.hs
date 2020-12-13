{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MsSmoothEncryptionSettings
  ( MsSmoothEncryptionSettings (..),

    -- * Smart constructor
    mkMsSmoothEncryptionSettings,

    -- * Lenses
    msesSpekeKeyProvider,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.SpekeKeyProvider
import qualified Network.AWS.Prelude as Lude

-- | If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to specify the value SpekeKeyProvider.
--
-- /See:/ 'mkMsSmoothEncryptionSettings' smart constructor.
newtype MsSmoothEncryptionSettings = MsSmoothEncryptionSettings'
  { -- | If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
    spekeKeyProvider :: Lude.Maybe SpekeKeyProvider
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MsSmoothEncryptionSettings' with the minimum fields required to make a request.
--
-- * 'spekeKeyProvider' - If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
mkMsSmoothEncryptionSettings ::
  MsSmoothEncryptionSettings
mkMsSmoothEncryptionSettings =
  MsSmoothEncryptionSettings' {spekeKeyProvider = Lude.Nothing}

-- | If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
--
-- /Note:/ Consider using 'spekeKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msesSpekeKeyProvider :: Lens.Lens' MsSmoothEncryptionSettings (Lude.Maybe SpekeKeyProvider)
msesSpekeKeyProvider = Lens.lens (spekeKeyProvider :: MsSmoothEncryptionSettings -> Lude.Maybe SpekeKeyProvider) (\s a -> s {spekeKeyProvider = a} :: MsSmoothEncryptionSettings)
{-# DEPRECATED msesSpekeKeyProvider "Use generic-lens or generic-optics with 'spekeKeyProvider' instead." #-}

instance Lude.FromJSON MsSmoothEncryptionSettings where
  parseJSON =
    Lude.withObject
      "MsSmoothEncryptionSettings"
      ( \x ->
          MsSmoothEncryptionSettings'
            Lude.<$> (x Lude..:? "spekeKeyProvider")
      )

instance Lude.ToJSON MsSmoothEncryptionSettings where
  toJSON MsSmoothEncryptionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [("spekeKeyProvider" Lude..=) Lude.<$> spekeKeyProvider]
      )
