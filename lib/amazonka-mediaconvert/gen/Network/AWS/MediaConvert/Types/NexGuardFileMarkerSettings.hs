{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NexGuardFileMarkerSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.NexGuardFileMarkerSettings
  ( NexGuardFileMarkerSettings (..)
  -- * Smart constructor
  , mkNexGuardFileMarkerSettings
  -- * Lenses
  , ngfmsLicense
  , ngfmsPayload
  , ngfmsPreset
  , ngfmsStrength
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.WatermarkingStrength as Types
import qualified Network.AWS.Prelude as Core

-- | For forensic video watermarking, MediaConvert supports Nagra NexGuard File Marker watermarking. MediaConvert supports both PreRelease Content (NGPR/G2) and OTT Streaming workflows.
--
-- /See:/ 'mkNexGuardFileMarkerSettings' smart constructor.
data NexGuardFileMarkerSettings = NexGuardFileMarkerSettings'
  { license :: Core.Maybe Core.Text
    -- ^ Use the base64 license string that Nagra provides you. Enter it directly in your JSON job specification or in the console. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job.
  , payload :: Core.Maybe Core.Natural
    -- ^ Specify the payload ID that you want associated with this output. Valid values vary depending on your Nagra NexGuard forensic watermarking workflow. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job. For PreRelease Content (NGPR/G2), specify an integer from 1 through 4,194,303. You must generate a unique ID for each asset you watermark, and keep a record of which ID you have assigned to each asset. Neither Nagra nor MediaConvert keep track of the relationship between output files and your IDs. For OTT Streaming, create two adaptive bitrate (ABR) stacks for each asset. Do this by setting up two output groups. For one output group, set the value of Payload ID (payload) to 0 in every output. For the other output group, set Payload ID (payload) to 1 in every output.
  , preset :: Core.Maybe Core.Text
    -- ^ Enter one of the watermarking preset strings that Nagra provides you. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job.
  , strength :: Core.Maybe Types.WatermarkingStrength
    -- ^ Optional. Ignore this setting unless Nagra support directs you to specify a value. When you don't specify a value here, the Nagra NexGuard library uses its default value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NexGuardFileMarkerSettings' value with any optional fields omitted.
mkNexGuardFileMarkerSettings
    :: NexGuardFileMarkerSettings
mkNexGuardFileMarkerSettings
  = NexGuardFileMarkerSettings'{license = Core.Nothing,
                                payload = Core.Nothing, preset = Core.Nothing,
                                strength = Core.Nothing}

-- | Use the base64 license string that Nagra provides you. Enter it directly in your JSON job specification or in the console. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job.
--
-- /Note:/ Consider using 'license' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngfmsLicense :: Lens.Lens' NexGuardFileMarkerSettings (Core.Maybe Core.Text)
ngfmsLicense = Lens.field @"license"
{-# INLINEABLE ngfmsLicense #-}
{-# DEPRECATED license "Use generic-lens or generic-optics with 'license' instead"  #-}

-- | Specify the payload ID that you want associated with this output. Valid values vary depending on your Nagra NexGuard forensic watermarking workflow. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job. For PreRelease Content (NGPR/G2), specify an integer from 1 through 4,194,303. You must generate a unique ID for each asset you watermark, and keep a record of which ID you have assigned to each asset. Neither Nagra nor MediaConvert keep track of the relationship between output files and your IDs. For OTT Streaming, create two adaptive bitrate (ABR) stacks for each asset. Do this by setting up two output groups. For one output group, set the value of Payload ID (payload) to 0 in every output. For the other output group, set Payload ID (payload) to 1 in every output.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngfmsPayload :: Lens.Lens' NexGuardFileMarkerSettings (Core.Maybe Core.Natural)
ngfmsPayload = Lens.field @"payload"
{-# INLINEABLE ngfmsPayload #-}
{-# DEPRECATED payload "Use generic-lens or generic-optics with 'payload' instead"  #-}

-- | Enter one of the watermarking preset strings that Nagra provides you. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job.
--
-- /Note:/ Consider using 'preset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngfmsPreset :: Lens.Lens' NexGuardFileMarkerSettings (Core.Maybe Core.Text)
ngfmsPreset = Lens.field @"preset"
{-# INLINEABLE ngfmsPreset #-}
{-# DEPRECATED preset "Use generic-lens or generic-optics with 'preset' instead"  #-}

-- | Optional. Ignore this setting unless Nagra support directs you to specify a value. When you don't specify a value here, the Nagra NexGuard library uses its default value.
--
-- /Note:/ Consider using 'strength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngfmsStrength :: Lens.Lens' NexGuardFileMarkerSettings (Core.Maybe Types.WatermarkingStrength)
ngfmsStrength = Lens.field @"strength"
{-# INLINEABLE ngfmsStrength #-}
{-# DEPRECATED strength "Use generic-lens or generic-optics with 'strength' instead"  #-}

instance Core.FromJSON NexGuardFileMarkerSettings where
        toJSON NexGuardFileMarkerSettings{..}
          = Core.object
              (Core.catMaybes
                 [("license" Core..=) Core.<$> license,
                  ("payload" Core..=) Core.<$> payload,
                  ("preset" Core..=) Core.<$> preset,
                  ("strength" Core..=) Core.<$> strength])

instance Core.FromJSON NexGuardFileMarkerSettings where
        parseJSON
          = Core.withObject "NexGuardFileMarkerSettings" Core.$
              \ x ->
                NexGuardFileMarkerSettings' Core.<$>
                  (x Core..:? "license") Core.<*> x Core..:? "payload" Core.<*>
                    x Core..:? "preset"
                    Core.<*> x Core..:? "strength"
