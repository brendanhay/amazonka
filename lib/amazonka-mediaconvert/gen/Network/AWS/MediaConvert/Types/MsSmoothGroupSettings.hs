{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MsSmoothGroupSettings
  ( MsSmoothGroupSettings (..)
  -- * Smart constructor
  , mkMsSmoothGroupSettings
  -- * Lenses
  , msgsAdditionalManifests
  , msgsAudioDeduplication
  , msgsDestination
  , msgsDestinationSettings
  , msgsEncryption
  , msgsFragmentLength
  , msgsManifestEncoding
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.DestinationSettings as Types
import qualified Network.AWS.MediaConvert.Types.MsSmoothAdditionalManifest as Types
import qualified Network.AWS.MediaConvert.Types.MsSmoothAudioDeduplication as Types
import qualified Network.AWS.MediaConvert.Types.MsSmoothEncryptionSettings as Types
import qualified Network.AWS.MediaConvert.Types.MsSmoothManifestEncoding as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to MS_SMOOTH_GROUP_SETTINGS.
--
-- /See:/ 'mkMsSmoothGroupSettings' smart constructor.
data MsSmoothGroupSettings = MsSmoothGroupSettings'
  { additionalManifests :: Core.Maybe [Types.MsSmoothAdditionalManifest]
    -- ^ By default, the service creates one .ism Microsoft Smooth Streaming manifest for each Microsoft Smooth Streaming output group in your job. This default manifest references every output in the output group. To create additional manifests that reference a subset of the outputs in the output group, specify a list of them here.
  , audioDeduplication :: Core.Maybe Types.MsSmoothAudioDeduplication
    -- ^ COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings across a Microsoft Smooth output group into a single audio stream.
  , destination :: Core.Maybe Core.Text
    -- ^ Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
  , destinationSettings :: Core.Maybe Types.DestinationSettings
    -- ^ Settings associated with the destination. Will vary based on the type of destination
  , encryption :: Core.Maybe Types.MsSmoothEncryptionSettings
    -- ^ If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to specify the value SpekeKeyProvider.
  , fragmentLength :: Core.Maybe Core.Natural
    -- ^ Use Fragment length (FragmentLength) to specify the mp4 fragment sizes in seconds. Fragment length must be compatible with GOP size and frame rate.
  , manifestEncoding :: Core.Maybe Types.MsSmoothManifestEncoding
    -- ^ Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding format for the server and client manifest. Valid options are utf8 and utf16.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MsSmoothGroupSettings' value with any optional fields omitted.
mkMsSmoothGroupSettings
    :: MsSmoothGroupSettings
mkMsSmoothGroupSettings
  = MsSmoothGroupSettings'{additionalManifests = Core.Nothing,
                           audioDeduplication = Core.Nothing, destination = Core.Nothing,
                           destinationSettings = Core.Nothing, encryption = Core.Nothing,
                           fragmentLength = Core.Nothing, manifestEncoding = Core.Nothing}

-- | By default, the service creates one .ism Microsoft Smooth Streaming manifest for each Microsoft Smooth Streaming output group in your job. This default manifest references every output in the output group. To create additional manifests that reference a subset of the outputs in the output group, specify a list of them here.
--
-- /Note:/ Consider using 'additionalManifests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsAdditionalManifests :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe [Types.MsSmoothAdditionalManifest])
msgsAdditionalManifests = Lens.field @"additionalManifests"
{-# INLINEABLE msgsAdditionalManifests #-}
{-# DEPRECATED additionalManifests "Use generic-lens or generic-optics with 'additionalManifests' instead"  #-}

-- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings across a Microsoft Smooth output group into a single audio stream.
--
-- /Note:/ Consider using 'audioDeduplication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsAudioDeduplication :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Types.MsSmoothAudioDeduplication)
msgsAudioDeduplication = Lens.field @"audioDeduplication"
{-# INLINEABLE msgsAudioDeduplication #-}
{-# DEPRECATED audioDeduplication "Use generic-lens or generic-optics with 'audioDeduplication' instead"  #-}

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsDestination :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Text)
msgsDestination = Lens.field @"destination"
{-# INLINEABLE msgsDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

-- | Settings associated with the destination. Will vary based on the type of destination
--
-- /Note:/ Consider using 'destinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsDestinationSettings :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Types.DestinationSettings)
msgsDestinationSettings = Lens.field @"destinationSettings"
{-# INLINEABLE msgsDestinationSettings #-}
{-# DEPRECATED destinationSettings "Use generic-lens or generic-optics with 'destinationSettings' instead"  #-}

-- | If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to specify the value SpekeKeyProvider.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsEncryption :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Types.MsSmoothEncryptionSettings)
msgsEncryption = Lens.field @"encryption"
{-# INLINEABLE msgsEncryption #-}
{-# DEPRECATED encryption "Use generic-lens or generic-optics with 'encryption' instead"  #-}

-- | Use Fragment length (FragmentLength) to specify the mp4 fragment sizes in seconds. Fragment length must be compatible with GOP size and frame rate.
--
-- /Note:/ Consider using 'fragmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsFragmentLength :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Natural)
msgsFragmentLength = Lens.field @"fragmentLength"
{-# INLINEABLE msgsFragmentLength #-}
{-# DEPRECATED fragmentLength "Use generic-lens or generic-optics with 'fragmentLength' instead"  #-}

-- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding format for the server and client manifest. Valid options are utf8 and utf16.
--
-- /Note:/ Consider using 'manifestEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsManifestEncoding :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Types.MsSmoothManifestEncoding)
msgsManifestEncoding = Lens.field @"manifestEncoding"
{-# INLINEABLE msgsManifestEncoding #-}
{-# DEPRECATED manifestEncoding "Use generic-lens or generic-optics with 'manifestEncoding' instead"  #-}

instance Core.FromJSON MsSmoothGroupSettings where
        toJSON MsSmoothGroupSettings{..}
          = Core.object
              (Core.catMaybes
                 [("additionalManifests" Core..=) Core.<$> additionalManifests,
                  ("audioDeduplication" Core..=) Core.<$> audioDeduplication,
                  ("destination" Core..=) Core.<$> destination,
                  ("destinationSettings" Core..=) Core.<$> destinationSettings,
                  ("encryption" Core..=) Core.<$> encryption,
                  ("fragmentLength" Core..=) Core.<$> fragmentLength,
                  ("manifestEncoding" Core..=) Core.<$> manifestEncoding])

instance Core.FromJSON MsSmoothGroupSettings where
        parseJSON
          = Core.withObject "MsSmoothGroupSettings" Core.$
              \ x ->
                MsSmoothGroupSettings' Core.<$>
                  (x Core..:? "additionalManifests") Core.<*>
                    x Core..:? "audioDeduplication"
                    Core.<*> x Core..:? "destination"
                    Core.<*> x Core..:? "destinationSettings"
                    Core.<*> x Core..:? "encryption"
                    Core.<*> x Core..:? "fragmentLength"
                    Core.<*> x Core..:? "manifestEncoding"
