{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MsSmoothGroupSettings
  ( MsSmoothGroupSettings (..),

    -- * Smart constructor
    mkMsSmoothGroupSettings,

    -- * Lenses
    msgsFragmentLength,
    msgsManifestEncoding,
    msgsDestination,
    msgsAudioDeduplication,
    msgsAdditionalManifests,
    msgsDestinationSettings,
    msgsEncryption,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DestinationSettings
import Network.AWS.MediaConvert.Types.MsSmoothAdditionalManifest
import Network.AWS.MediaConvert.Types.MsSmoothAudioDeduplication
import Network.AWS.MediaConvert.Types.MsSmoothEncryptionSettings
import Network.AWS.MediaConvert.Types.MsSmoothManifestEncoding
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to MS_SMOOTH_GROUP_SETTINGS.
--
-- /See:/ 'mkMsSmoothGroupSettings' smart constructor.
data MsSmoothGroupSettings = MsSmoothGroupSettings'
  { -- | Use Fragment length (FragmentLength) to specify the mp4 fragment sizes in seconds. Fragment length must be compatible with GOP size and frame rate.
    fragmentLength :: Lude.Maybe Lude.Natural,
    -- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding format for the server and client manifest. Valid options are utf8 and utf16.
    manifestEncoding :: Lude.Maybe MsSmoothManifestEncoding,
    -- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
    destination :: Lude.Maybe Lude.Text,
    -- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings across a Microsoft Smooth output group into a single audio stream.
    audioDeduplication :: Lude.Maybe MsSmoothAudioDeduplication,
    -- | By default, the service creates one .ism Microsoft Smooth Streaming manifest for each Microsoft Smooth Streaming output group in your job. This default manifest references every output in the output group. To create additional manifests that reference a subset of the outputs in the output group, specify a list of them here.
    additionalManifests :: Lude.Maybe [MsSmoothAdditionalManifest],
    -- | Settings associated with the destination. Will vary based on the type of destination
    destinationSettings :: Lude.Maybe DestinationSettings,
    -- | If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to specify the value SpekeKeyProvider.
    encryption :: Lude.Maybe MsSmoothEncryptionSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MsSmoothGroupSettings' with the minimum fields required to make a request.
--
-- * 'fragmentLength' - Use Fragment length (FragmentLength) to specify the mp4 fragment sizes in seconds. Fragment length must be compatible with GOP size and frame rate.
-- * 'manifestEncoding' - Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding format for the server and client manifest. Valid options are utf8 and utf16.
-- * 'destination' - Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
-- * 'audioDeduplication' - COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings across a Microsoft Smooth output group into a single audio stream.
-- * 'additionalManifests' - By default, the service creates one .ism Microsoft Smooth Streaming manifest for each Microsoft Smooth Streaming output group in your job. This default manifest references every output in the output group. To create additional manifests that reference a subset of the outputs in the output group, specify a list of them here.
-- * 'destinationSettings' - Settings associated with the destination. Will vary based on the type of destination
-- * 'encryption' - If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to specify the value SpekeKeyProvider.
mkMsSmoothGroupSettings ::
  MsSmoothGroupSettings
mkMsSmoothGroupSettings =
  MsSmoothGroupSettings'
    { fragmentLength = Lude.Nothing,
      manifestEncoding = Lude.Nothing,
      destination = Lude.Nothing,
      audioDeduplication = Lude.Nothing,
      additionalManifests = Lude.Nothing,
      destinationSettings = Lude.Nothing,
      encryption = Lude.Nothing
    }

-- | Use Fragment length (FragmentLength) to specify the mp4 fragment sizes in seconds. Fragment length must be compatible with GOP size and frame rate.
--
-- /Note:/ Consider using 'fragmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsFragmentLength :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe Lude.Natural)
msgsFragmentLength = Lens.lens (fragmentLength :: MsSmoothGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {fragmentLength = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsFragmentLength "Use generic-lens or generic-optics with 'fragmentLength' instead." #-}

-- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding format for the server and client manifest. Valid options are utf8 and utf16.
--
-- /Note:/ Consider using 'manifestEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsManifestEncoding :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe MsSmoothManifestEncoding)
msgsManifestEncoding = Lens.lens (manifestEncoding :: MsSmoothGroupSettings -> Lude.Maybe MsSmoothManifestEncoding) (\s a -> s {manifestEncoding = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsManifestEncoding "Use generic-lens or generic-optics with 'manifestEncoding' instead." #-}

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsDestination :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe Lude.Text)
msgsDestination = Lens.lens (destination :: MsSmoothGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {destination = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings across a Microsoft Smooth output group into a single audio stream.
--
-- /Note:/ Consider using 'audioDeduplication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsAudioDeduplication :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe MsSmoothAudioDeduplication)
msgsAudioDeduplication = Lens.lens (audioDeduplication :: MsSmoothGroupSettings -> Lude.Maybe MsSmoothAudioDeduplication) (\s a -> s {audioDeduplication = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsAudioDeduplication "Use generic-lens or generic-optics with 'audioDeduplication' instead." #-}

-- | By default, the service creates one .ism Microsoft Smooth Streaming manifest for each Microsoft Smooth Streaming output group in your job. This default manifest references every output in the output group. To create additional manifests that reference a subset of the outputs in the output group, specify a list of them here.
--
-- /Note:/ Consider using 'additionalManifests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsAdditionalManifests :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe [MsSmoothAdditionalManifest])
msgsAdditionalManifests = Lens.lens (additionalManifests :: MsSmoothGroupSettings -> Lude.Maybe [MsSmoothAdditionalManifest]) (\s a -> s {additionalManifests = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsAdditionalManifests "Use generic-lens or generic-optics with 'additionalManifests' instead." #-}

-- | Settings associated with the destination. Will vary based on the type of destination
--
-- /Note:/ Consider using 'destinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsDestinationSettings :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe DestinationSettings)
msgsDestinationSettings = Lens.lens (destinationSettings :: MsSmoothGroupSettings -> Lude.Maybe DestinationSettings) (\s a -> s {destinationSettings = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsDestinationSettings "Use generic-lens or generic-optics with 'destinationSettings' instead." #-}

-- | If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to specify the value SpekeKeyProvider.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msgsEncryption :: Lens.Lens' MsSmoothGroupSettings (Lude.Maybe MsSmoothEncryptionSettings)
msgsEncryption = Lens.lens (encryption :: MsSmoothGroupSettings -> Lude.Maybe MsSmoothEncryptionSettings) (\s a -> s {encryption = a} :: MsSmoothGroupSettings)
{-# DEPRECATED msgsEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

instance Lude.FromJSON MsSmoothGroupSettings where
  parseJSON =
    Lude.withObject
      "MsSmoothGroupSettings"
      ( \x ->
          MsSmoothGroupSettings'
            Lude.<$> (x Lude..:? "fragmentLength")
            Lude.<*> (x Lude..:? "manifestEncoding")
            Lude.<*> (x Lude..:? "destination")
            Lude.<*> (x Lude..:? "audioDeduplication")
            Lude.<*> (x Lude..:? "additionalManifests" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "destinationSettings")
            Lude.<*> (x Lude..:? "encryption")
      )

instance Lude.ToJSON MsSmoothGroupSettings where
  toJSON MsSmoothGroupSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("fragmentLength" Lude..=) Lude.<$> fragmentLength,
            ("manifestEncoding" Lude..=) Lude.<$> manifestEncoding,
            ("destination" Lude..=) Lude.<$> destination,
            ("audioDeduplication" Lude..=) Lude.<$> audioDeduplication,
            ("additionalManifests" Lude..=) Lude.<$> additionalManifests,
            ("destinationSettings" Lude..=) Lude.<$> destinationSettings,
            ("encryption" Lude..=) Lude.<$> encryption
          ]
      )
