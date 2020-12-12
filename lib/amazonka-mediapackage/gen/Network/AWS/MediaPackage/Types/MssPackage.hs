{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.MssPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.MssPackage
  ( MssPackage (..),

    -- * Smart constructor
    mkMssPackage,

    -- * Lenses
    mpSegmentDurationSeconds,
    mpStreamSelection,
    mpEncryption,
    mpManifestWindowSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.MssEncryption
import Network.AWS.MediaPackage.Types.StreamSelection
import qualified Network.AWS.Prelude as Lude

-- | A Microsoft Smooth Streaming (MSS) packaging configuration.
--
-- /See:/ 'mkMssPackage' smart constructor.
data MssPackage = MssPackage'
  { segmentDurationSeconds ::
      Lude.Maybe Lude.Int,
    streamSelection :: Lude.Maybe StreamSelection,
    encryption :: Lude.Maybe MssEncryption,
    manifestWindowSeconds :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MssPackage' with the minimum fields required to make a request.
--
-- * 'encryption' - Undocumented field.
-- * 'manifestWindowSeconds' - The time window (in seconds) contained in each manifest.
-- * 'segmentDurationSeconds' - The duration (in seconds) of each segment.
-- * 'streamSelection' - Undocumented field.
mkMssPackage ::
  MssPackage
mkMssPackage =
  MssPackage'
    { segmentDurationSeconds = Lude.Nothing,
      streamSelection = Lude.Nothing,
      encryption = Lude.Nothing,
      manifestWindowSeconds = Lude.Nothing
    }

-- | The duration (in seconds) of each segment.
--
-- /Note:/ Consider using 'segmentDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpSegmentDurationSeconds :: Lens.Lens' MssPackage (Lude.Maybe Lude.Int)
mpSegmentDurationSeconds = Lens.lens (segmentDurationSeconds :: MssPackage -> Lude.Maybe Lude.Int) (\s a -> s {segmentDurationSeconds = a} :: MssPackage)
{-# DEPRECATED mpSegmentDurationSeconds "Use generic-lens or generic-optics with 'segmentDurationSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'streamSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpStreamSelection :: Lens.Lens' MssPackage (Lude.Maybe StreamSelection)
mpStreamSelection = Lens.lens (streamSelection :: MssPackage -> Lude.Maybe StreamSelection) (\s a -> s {streamSelection = a} :: MssPackage)
{-# DEPRECATED mpStreamSelection "Use generic-lens or generic-optics with 'streamSelection' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpEncryption :: Lens.Lens' MssPackage (Lude.Maybe MssEncryption)
mpEncryption = Lens.lens (encryption :: MssPackage -> Lude.Maybe MssEncryption) (\s a -> s {encryption = a} :: MssPackage)
{-# DEPRECATED mpEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | The time window (in seconds) contained in each manifest.
--
-- /Note:/ Consider using 'manifestWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpManifestWindowSeconds :: Lens.Lens' MssPackage (Lude.Maybe Lude.Int)
mpManifestWindowSeconds = Lens.lens (manifestWindowSeconds :: MssPackage -> Lude.Maybe Lude.Int) (\s a -> s {manifestWindowSeconds = a} :: MssPackage)
{-# DEPRECATED mpManifestWindowSeconds "Use generic-lens or generic-optics with 'manifestWindowSeconds' instead." #-}

instance Lude.FromJSON MssPackage where
  parseJSON =
    Lude.withObject
      "MssPackage"
      ( \x ->
          MssPackage'
            Lude.<$> (x Lude..:? "segmentDurationSeconds")
            Lude.<*> (x Lude..:? "streamSelection")
            Lude.<*> (x Lude..:? "encryption")
            Lude.<*> (x Lude..:? "manifestWindowSeconds")
      )

instance Lude.ToJSON MssPackage where
  toJSON MssPackage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("segmentDurationSeconds" Lude..=)
              Lude.<$> segmentDurationSeconds,
            ("streamSelection" Lude..=) Lude.<$> streamSelection,
            ("encryption" Lude..=) Lude.<$> encryption,
            ("manifestWindowSeconds" Lude..=) Lude.<$> manifestWindowSeconds
          ]
      )
