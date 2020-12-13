{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NexGuardFileMarkerSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NexGuardFileMarkerSettings
  ( NexGuardFileMarkerSettings (..),

    -- * Smart constructor
    mkNexGuardFileMarkerSettings,

    -- * Lenses
    ngfmsStrength,
    ngfmsPayload,
    ngfmsPreset,
    ngfmsLicense,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.WatermarkingStrength
import qualified Network.AWS.Prelude as Lude

-- | For forensic video watermarking, MediaConvert supports Nagra NexGuard File Marker watermarking. MediaConvert supports both PreRelease Content (NGPR/G2) and OTT Streaming workflows.
--
-- /See:/ 'mkNexGuardFileMarkerSettings' smart constructor.
data NexGuardFileMarkerSettings = NexGuardFileMarkerSettings'
  { -- | Optional. Ignore this setting unless Nagra support directs you to specify a value. When you don't specify a value here, the Nagra NexGuard library uses its default value.
    strength :: Lude.Maybe WatermarkingStrength,
    -- | Specify the payload ID that you want associated with this output. Valid values vary depending on your Nagra NexGuard forensic watermarking workflow. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job. For PreRelease Content (NGPR/G2), specify an integer from 1 through 4,194,303. You must generate a unique ID for each asset you watermark, and keep a record of which ID you have assigned to each asset. Neither Nagra nor MediaConvert keep track of the relationship between output files and your IDs. For OTT Streaming, create two adaptive bitrate (ABR) stacks for each asset. Do this by setting up two output groups. For one output group, set the value of Payload ID (payload) to 0 in every output. For the other output group, set Payload ID (payload) to 1 in every output.
    payload :: Lude.Maybe Lude.Natural,
    -- | Enter one of the watermarking preset strings that Nagra provides you. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job.
    preset :: Lude.Maybe Lude.Text,
    -- | Use the base64 license string that Nagra provides you. Enter it directly in your JSON job specification or in the console. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job.
    license :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NexGuardFileMarkerSettings' with the minimum fields required to make a request.
--
-- * 'strength' - Optional. Ignore this setting unless Nagra support directs you to specify a value. When you don't specify a value here, the Nagra NexGuard library uses its default value.
-- * 'payload' - Specify the payload ID that you want associated with this output. Valid values vary depending on your Nagra NexGuard forensic watermarking workflow. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job. For PreRelease Content (NGPR/G2), specify an integer from 1 through 4,194,303. You must generate a unique ID for each asset you watermark, and keep a record of which ID you have assigned to each asset. Neither Nagra nor MediaConvert keep track of the relationship between output files and your IDs. For OTT Streaming, create two adaptive bitrate (ABR) stacks for each asset. Do this by setting up two output groups. For one output group, set the value of Payload ID (payload) to 0 in every output. For the other output group, set Payload ID (payload) to 1 in every output.
-- * 'preset' - Enter one of the watermarking preset strings that Nagra provides you. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job.
-- * 'license' - Use the base64 license string that Nagra provides you. Enter it directly in your JSON job specification or in the console. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job.
mkNexGuardFileMarkerSettings ::
  NexGuardFileMarkerSettings
mkNexGuardFileMarkerSettings =
  NexGuardFileMarkerSettings'
    { strength = Lude.Nothing,
      payload = Lude.Nothing,
      preset = Lude.Nothing,
      license = Lude.Nothing
    }

-- | Optional. Ignore this setting unless Nagra support directs you to specify a value. When you don't specify a value here, the Nagra NexGuard library uses its default value.
--
-- /Note:/ Consider using 'strength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngfmsStrength :: Lens.Lens' NexGuardFileMarkerSettings (Lude.Maybe WatermarkingStrength)
ngfmsStrength = Lens.lens (strength :: NexGuardFileMarkerSettings -> Lude.Maybe WatermarkingStrength) (\s a -> s {strength = a} :: NexGuardFileMarkerSettings)
{-# DEPRECATED ngfmsStrength "Use generic-lens or generic-optics with 'strength' instead." #-}

-- | Specify the payload ID that you want associated with this output. Valid values vary depending on your Nagra NexGuard forensic watermarking workflow. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job. For PreRelease Content (NGPR/G2), specify an integer from 1 through 4,194,303. You must generate a unique ID for each asset you watermark, and keep a record of which ID you have assigned to each asset. Neither Nagra nor MediaConvert keep track of the relationship between output files and your IDs. For OTT Streaming, create two adaptive bitrate (ABR) stacks for each asset. Do this by setting up two output groups. For one output group, set the value of Payload ID (payload) to 0 in every output. For the other output group, set Payload ID (payload) to 1 in every output.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngfmsPayload :: Lens.Lens' NexGuardFileMarkerSettings (Lude.Maybe Lude.Natural)
ngfmsPayload = Lens.lens (payload :: NexGuardFileMarkerSettings -> Lude.Maybe Lude.Natural) (\s a -> s {payload = a} :: NexGuardFileMarkerSettings)
{-# DEPRECATED ngfmsPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | Enter one of the watermarking preset strings that Nagra provides you. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job.
--
-- /Note:/ Consider using 'preset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngfmsPreset :: Lens.Lens' NexGuardFileMarkerSettings (Lude.Maybe Lude.Text)
ngfmsPreset = Lens.lens (preset :: NexGuardFileMarkerSettings -> Lude.Maybe Lude.Text) (\s a -> s {preset = a} :: NexGuardFileMarkerSettings)
{-# DEPRECATED ngfmsPreset "Use generic-lens or generic-optics with 'preset' instead." #-}

-- | Use the base64 license string that Nagra provides you. Enter it directly in your JSON job specification or in the console. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job.
--
-- /Note:/ Consider using 'license' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngfmsLicense :: Lens.Lens' NexGuardFileMarkerSettings (Lude.Maybe Lude.Text)
ngfmsLicense = Lens.lens (license :: NexGuardFileMarkerSettings -> Lude.Maybe Lude.Text) (\s a -> s {license = a} :: NexGuardFileMarkerSettings)
{-# DEPRECATED ngfmsLicense "Use generic-lens or generic-optics with 'license' instead." #-}

instance Lude.FromJSON NexGuardFileMarkerSettings where
  parseJSON =
    Lude.withObject
      "NexGuardFileMarkerSettings"
      ( \x ->
          NexGuardFileMarkerSettings'
            Lude.<$> (x Lude..:? "strength")
            Lude.<*> (x Lude..:? "payload")
            Lude.<*> (x Lude..:? "preset")
            Lude.<*> (x Lude..:? "license")
      )

instance Lude.ToJSON NexGuardFileMarkerSettings where
  toJSON NexGuardFileMarkerSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("strength" Lude..=) Lude.<$> strength,
            ("payload" Lude..=) Lude.<$> payload,
            ("preset" Lude..=) Lude.<$> preset,
            ("license" Lude..=) Lude.<$> license
          ]
      )
