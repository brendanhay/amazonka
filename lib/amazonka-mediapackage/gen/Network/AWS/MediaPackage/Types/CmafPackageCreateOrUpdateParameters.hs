{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.CmafPackageCreateOrUpdateParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.CmafPackageCreateOrUpdateParameters
  ( CmafPackageCreateOrUpdateParameters (..)
  -- * Smart constructor
  , mkCmafPackageCreateOrUpdateParameters
  -- * Lenses
  , cpcoupEncryption
  , cpcoupHlsManifests
  , cpcoupSegmentDurationSeconds
  , cpcoupSegmentPrefix
  , cpcoupStreamSelection
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types.CmafEncryption as Types
import qualified Network.AWS.MediaPackage.Types.HlsManifestCreateOrUpdateParameters as Types
import qualified Network.AWS.MediaPackage.Types.StreamSelection as Types
import qualified Network.AWS.Prelude as Core

-- | A Common Media Application Format (CMAF) packaging configuration.
--
-- /See:/ 'mkCmafPackageCreateOrUpdateParameters' smart constructor.
data CmafPackageCreateOrUpdateParameters = CmafPackageCreateOrUpdateParameters'
  { encryption :: Core.Maybe Types.CmafEncryption
  , hlsManifests :: Core.Maybe [Types.HlsManifestCreateOrUpdateParameters]
    -- ^ A list of HLS manifest configurations
  , segmentDurationSeconds :: Core.Maybe Core.Int
    -- ^ Duration (in seconds) of each segment. Actual segments will be
--
-- rounded to the nearest multiple of the source segment duration.
  , segmentPrefix :: Core.Maybe Core.Text
    -- ^ An optional custom string that is prepended to the name of each segment. If not specified, it defaults to the ChannelId.
  , streamSelection :: Core.Maybe Types.StreamSelection
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CmafPackageCreateOrUpdateParameters' value with any optional fields omitted.
mkCmafPackageCreateOrUpdateParameters
    :: CmafPackageCreateOrUpdateParameters
mkCmafPackageCreateOrUpdateParameters
  = CmafPackageCreateOrUpdateParameters'{encryption = Core.Nothing,
                                         hlsManifests = Core.Nothing,
                                         segmentDurationSeconds = Core.Nothing,
                                         segmentPrefix = Core.Nothing,
                                         streamSelection = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcoupEncryption :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Core.Maybe Types.CmafEncryption)
cpcoupEncryption = Lens.field @"encryption"
{-# INLINEABLE cpcoupEncryption #-}
{-# DEPRECATED encryption "Use generic-lens or generic-optics with 'encryption' instead"  #-}

-- | A list of HLS manifest configurations
--
-- /Note:/ Consider using 'hlsManifests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcoupHlsManifests :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Core.Maybe [Types.HlsManifestCreateOrUpdateParameters])
cpcoupHlsManifests = Lens.field @"hlsManifests"
{-# INLINEABLE cpcoupHlsManifests #-}
{-# DEPRECATED hlsManifests "Use generic-lens or generic-optics with 'hlsManifests' instead"  #-}

-- | Duration (in seconds) of each segment. Actual segments will be
--
-- rounded to the nearest multiple of the source segment duration.
--
-- /Note:/ Consider using 'segmentDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcoupSegmentDurationSeconds :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Core.Maybe Core.Int)
cpcoupSegmentDurationSeconds = Lens.field @"segmentDurationSeconds"
{-# INLINEABLE cpcoupSegmentDurationSeconds #-}
{-# DEPRECATED segmentDurationSeconds "Use generic-lens or generic-optics with 'segmentDurationSeconds' instead"  #-}

-- | An optional custom string that is prepended to the name of each segment. If not specified, it defaults to the ChannelId.
--
-- /Note:/ Consider using 'segmentPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcoupSegmentPrefix :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Core.Maybe Core.Text)
cpcoupSegmentPrefix = Lens.field @"segmentPrefix"
{-# INLINEABLE cpcoupSegmentPrefix #-}
{-# DEPRECATED segmentPrefix "Use generic-lens or generic-optics with 'segmentPrefix' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'streamSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcoupStreamSelection :: Lens.Lens' CmafPackageCreateOrUpdateParameters (Core.Maybe Types.StreamSelection)
cpcoupStreamSelection = Lens.field @"streamSelection"
{-# INLINEABLE cpcoupStreamSelection #-}
{-# DEPRECATED streamSelection "Use generic-lens or generic-optics with 'streamSelection' instead"  #-}

instance Core.FromJSON CmafPackageCreateOrUpdateParameters where
        toJSON CmafPackageCreateOrUpdateParameters{..}
          = Core.object
              (Core.catMaybes
                 [("encryption" Core..=) Core.<$> encryption,
                  ("hlsManifests" Core..=) Core.<$> hlsManifests,
                  ("segmentDurationSeconds" Core..=) Core.<$> segmentDurationSeconds,
                  ("segmentPrefix" Core..=) Core.<$> segmentPrefix,
                  ("streamSelection" Core..=) Core.<$> streamSelection])
