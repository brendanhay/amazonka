{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.CmafPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.CmafPackage
  ( CmafPackage (..),

    -- * Smart constructor
    mkCmafPackage,

    -- * Lenses
    cpHlsManifests,
    cpSegmentDurationSeconds,
    cpStreamSelection,
    cpEncryption,
    cpSegmentPrefix,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.CmafEncryption
import Network.AWS.MediaPackage.Types.HlsManifest
import Network.AWS.MediaPackage.Types.StreamSelection
import qualified Network.AWS.Prelude as Lude

-- | A Common Media Application Format (CMAF) packaging configuration.
--
-- /See:/ 'mkCmafPackage' smart constructor.
data CmafPackage = CmafPackage'
  { hlsManifests ::
      Lude.Maybe [HlsManifest],
    segmentDurationSeconds :: Lude.Maybe Lude.Int,
    streamSelection :: Lude.Maybe StreamSelection,
    encryption :: Lude.Maybe CmafEncryption,
    segmentPrefix :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CmafPackage' with the minimum fields required to make a request.
--
-- * 'encryption' - Undocumented field.
-- * 'hlsManifests' - A list of HLS manifest configurations
-- * 'segmentDurationSeconds' - Duration (in seconds) of each segment. Actual segments will be
--
-- rounded to the nearest multiple of the source segment duration.
-- * 'segmentPrefix' - An optional custom string that is prepended to the name of each segment. If not specified, it defaults to the ChannelId.
-- * 'streamSelection' - Undocumented field.
mkCmafPackage ::
  CmafPackage
mkCmafPackage =
  CmafPackage'
    { hlsManifests = Lude.Nothing,
      segmentDurationSeconds = Lude.Nothing,
      streamSelection = Lude.Nothing,
      encryption = Lude.Nothing,
      segmentPrefix = Lude.Nothing
    }

-- | A list of HLS manifest configurations
--
-- /Note:/ Consider using 'hlsManifests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpHlsManifests :: Lens.Lens' CmafPackage (Lude.Maybe [HlsManifest])
cpHlsManifests = Lens.lens (hlsManifests :: CmafPackage -> Lude.Maybe [HlsManifest]) (\s a -> s {hlsManifests = a} :: CmafPackage)
{-# DEPRECATED cpHlsManifests "Use generic-lens or generic-optics with 'hlsManifests' instead." #-}

-- | Duration (in seconds) of each segment. Actual segments will be
--
-- rounded to the nearest multiple of the source segment duration.
--
-- /Note:/ Consider using 'segmentDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSegmentDurationSeconds :: Lens.Lens' CmafPackage (Lude.Maybe Lude.Int)
cpSegmentDurationSeconds = Lens.lens (segmentDurationSeconds :: CmafPackage -> Lude.Maybe Lude.Int) (\s a -> s {segmentDurationSeconds = a} :: CmafPackage)
{-# DEPRECATED cpSegmentDurationSeconds "Use generic-lens or generic-optics with 'segmentDurationSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'streamSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpStreamSelection :: Lens.Lens' CmafPackage (Lude.Maybe StreamSelection)
cpStreamSelection = Lens.lens (streamSelection :: CmafPackage -> Lude.Maybe StreamSelection) (\s a -> s {streamSelection = a} :: CmafPackage)
{-# DEPRECATED cpStreamSelection "Use generic-lens or generic-optics with 'streamSelection' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpEncryption :: Lens.Lens' CmafPackage (Lude.Maybe CmafEncryption)
cpEncryption = Lens.lens (encryption :: CmafPackage -> Lude.Maybe CmafEncryption) (\s a -> s {encryption = a} :: CmafPackage)
{-# DEPRECATED cpEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | An optional custom string that is prepended to the name of each segment. If not specified, it defaults to the ChannelId.
--
-- /Note:/ Consider using 'segmentPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSegmentPrefix :: Lens.Lens' CmafPackage (Lude.Maybe Lude.Text)
cpSegmentPrefix = Lens.lens (segmentPrefix :: CmafPackage -> Lude.Maybe Lude.Text) (\s a -> s {segmentPrefix = a} :: CmafPackage)
{-# DEPRECATED cpSegmentPrefix "Use generic-lens or generic-optics with 'segmentPrefix' instead." #-}

instance Lude.FromJSON CmafPackage where
  parseJSON =
    Lude.withObject
      "CmafPackage"
      ( \x ->
          CmafPackage'
            Lude.<$> (x Lude..:? "hlsManifests" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "segmentDurationSeconds")
            Lude.<*> (x Lude..:? "streamSelection")
            Lude.<*> (x Lude..:? "encryption")
            Lude.<*> (x Lude..:? "segmentPrefix")
      )
