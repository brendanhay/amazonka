-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.CmafPackageCreateOrUpdateParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.CmafPackageCreateOrUpdateParameters
  ( CmafPackageCreateOrUpdateParameters (..),

    -- * Smart constructor
    mkCmafPackageCreateOrUpdateParameters,

    -- * Lenses
    cpcoupHlsManifests,
    cpcoupSegmentDurationSeconds,
    cpcoupStreamSelection,
    cpcoupEncryption,
    cpcoupSegmentPrefix,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.CmafEncryption
import Network.AWS.MediaPackage.Types.HlsManifestCreateOrUpdateParameters
import Network.AWS.MediaPackage.Types.StreamSelection
import qualified Network.AWS.Prelude as Lude

-- | A Common Media Application Format (CMAF) packaging configuration.
--
-- /See:/ 'mkCmafPackageCreateOrUpdateParameters' smart constructor.
data CmafPackageCreateOrUpdateParameters = CmafPackageCreateOrUpdateParameters'
  { hlsManifests ::
      Lude.Maybe
        [HlsManifestCreateOrUpdateParameters],
    segmentDurationSeconds ::
      Lude.Maybe Lude.Int,
    streamSelection ::
      Lude.Maybe
        StreamSelection,
    encryption ::
      Lude.Maybe
        CmafEncryption,
    segmentPrefix ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CmafPackageCreateOrUpdateParameters' with the minimum fields required to make a request.
--
-- * 'encryption' - Undocumented field.
-- * 'hlsManifests' - A list of HLS manifest configurations
-- * 'segmentDurationSeconds' - Duration (in seconds) of each segment. Actual segments will be
--
-- rounded to the nearest multiple of the source segment duration.
-- * 'segmentPrefix' - An optional custom string that is prepended to the name of each segment. If not specified, it defaults to the ChannelId.
-- * 'streamSelection' - Undocumented field.
mkCmafPackageCreateOrUpdateParameters ::
  CmafPackageCreateOrUpdateParameters
mkCmafPackageCreateOrUpdateParameters =
  CmafPackageCreateOrUpdateParameters'
    { hlsManifests = Lude.Nothing,
      segmentDurationSeconds = Lude.Nothing,
      streamSelection = Lude.Nothing,
      encryption = Lude.Nothing,
      segmentPrefix = Lude.Nothing
    }

-- | A list of HLS manifest configurations
--
-- /Note:/ Consider using 'hlsManifests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcoupHlsManifests :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Lude.Maybe [HlsManifestCreateOrUpdateParameters])
cpcoupHlsManifests = Lens.lens (hlsManifests :: CmafPackageCreateOrUpdateParameters -> Lude.Maybe [HlsManifestCreateOrUpdateParameters]) (\s a -> s {hlsManifests = a} :: CmafPackageCreateOrUpdateParameters)
{-# DEPRECATED cpcoupHlsManifests "Use generic-lens or generic-optics with 'hlsManifests' instead." #-}

-- | Duration (in seconds) of each segment. Actual segments will be
--
-- rounded to the nearest multiple of the source segment duration.
--
-- /Note:/ Consider using 'segmentDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcoupSegmentDurationSeconds :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Lude.Maybe Lude.Int)
cpcoupSegmentDurationSeconds = Lens.lens (segmentDurationSeconds :: CmafPackageCreateOrUpdateParameters -> Lude.Maybe Lude.Int) (\s a -> s {segmentDurationSeconds = a} :: CmafPackageCreateOrUpdateParameters)
{-# DEPRECATED cpcoupSegmentDurationSeconds "Use generic-lens or generic-optics with 'segmentDurationSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'streamSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcoupStreamSelection :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Lude.Maybe StreamSelection)
cpcoupStreamSelection = Lens.lens (streamSelection :: CmafPackageCreateOrUpdateParameters -> Lude.Maybe StreamSelection) (\s a -> s {streamSelection = a} :: CmafPackageCreateOrUpdateParameters)
{-# DEPRECATED cpcoupStreamSelection "Use generic-lens or generic-optics with 'streamSelection' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcoupEncryption :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Lude.Maybe CmafEncryption)
cpcoupEncryption = Lens.lens (encryption :: CmafPackageCreateOrUpdateParameters -> Lude.Maybe CmafEncryption) (\s a -> s {encryption = a} :: CmafPackageCreateOrUpdateParameters)
{-# DEPRECATED cpcoupEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | An optional custom string that is prepended to the name of each segment. If not specified, it defaults to the ChannelId.
--
-- /Note:/ Consider using 'segmentPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcoupSegmentPrefix :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Lude.Maybe Lude.Text)
cpcoupSegmentPrefix = Lens.lens (segmentPrefix :: CmafPackageCreateOrUpdateParameters -> Lude.Maybe Lude.Text) (\s a -> s {segmentPrefix = a} :: CmafPackageCreateOrUpdateParameters)
{-# DEPRECATED cpcoupSegmentPrefix "Use generic-lens or generic-optics with 'segmentPrefix' instead." #-}

instance Lude.ToJSON CmafPackageCreateOrUpdateParameters where
  toJSON CmafPackageCreateOrUpdateParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("hlsManifests" Lude..=) Lude.<$> hlsManifests,
            ("segmentDurationSeconds" Lude..=) Lude.<$> segmentDurationSeconds,
            ("streamSelection" Lude..=) Lude.<$> streamSelection,
            ("encryption" Lude..=) Lude.<$> encryption,
            ("segmentPrefix" Lude..=) Lude.<$> segmentPrefix
          ]
      )
