{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NexGuardFileMarkerSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NexGuardFileMarkerSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.WatermarkingStrength
import qualified Network.AWS.Prelude as Prelude

-- | For forensic video watermarking, MediaConvert supports Nagra NexGuard
-- File Marker watermarking. MediaConvert supports both PreRelease Content
-- (NGPR\/G2) and OTT Streaming workflows.
--
-- /See:/ 'newNexGuardFileMarkerSettings' smart constructor.
data NexGuardFileMarkerSettings = NexGuardFileMarkerSettings'
  { -- | Specify the payload ID that you want associated with this output. Valid
    -- values vary depending on your Nagra NexGuard forensic watermarking
    -- workflow. Required when you include Nagra NexGuard File Marker
    -- watermarking (NexGuardWatermarkingSettings) in your job. For PreRelease
    -- Content (NGPR\/G2), specify an integer from 1 through 4,194,303. You
    -- must generate a unique ID for each asset you watermark, and keep a
    -- record of which ID you have assigned to each asset. Neither Nagra nor
    -- MediaConvert keep track of the relationship between output files and
    -- your IDs. For OTT Streaming, create two adaptive bitrate (ABR) stacks
    -- for each asset. Do this by setting up two output groups. For one output
    -- group, set the value of Payload ID (payload) to 0 in every output. For
    -- the other output group, set Payload ID (payload) to 1 in every output.
    payload :: Prelude.Maybe Prelude.Natural,
    -- | Use the base64 license string that Nagra provides you. Enter it directly
    -- in your JSON job specification or in the console. Required when you
    -- include Nagra NexGuard File Marker watermarking
    -- (NexGuardWatermarkingSettings) in your job.
    license :: Prelude.Maybe Prelude.Text,
    -- | Enter one of the watermarking preset strings that Nagra provides you.
    -- Required when you include Nagra NexGuard File Marker watermarking
    -- (NexGuardWatermarkingSettings) in your job.
    preset :: Prelude.Maybe Prelude.Text,
    -- | Optional. Ignore this setting unless Nagra support directs you to
    -- specify a value. When you don\'t specify a value here, the Nagra
    -- NexGuard library uses its default value.
    strength :: Prelude.Maybe WatermarkingStrength
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NexGuardFileMarkerSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payload', 'nexGuardFileMarkerSettings_payload' - Specify the payload ID that you want associated with this output. Valid
-- values vary depending on your Nagra NexGuard forensic watermarking
-- workflow. Required when you include Nagra NexGuard File Marker
-- watermarking (NexGuardWatermarkingSettings) in your job. For PreRelease
-- Content (NGPR\/G2), specify an integer from 1 through 4,194,303. You
-- must generate a unique ID for each asset you watermark, and keep a
-- record of which ID you have assigned to each asset. Neither Nagra nor
-- MediaConvert keep track of the relationship between output files and
-- your IDs. For OTT Streaming, create two adaptive bitrate (ABR) stacks
-- for each asset. Do this by setting up two output groups. For one output
-- group, set the value of Payload ID (payload) to 0 in every output. For
-- the other output group, set Payload ID (payload) to 1 in every output.
--
-- 'license', 'nexGuardFileMarkerSettings_license' - Use the base64 license string that Nagra provides you. Enter it directly
-- in your JSON job specification or in the console. Required when you
-- include Nagra NexGuard File Marker watermarking
-- (NexGuardWatermarkingSettings) in your job.
--
-- 'preset', 'nexGuardFileMarkerSettings_preset' - Enter one of the watermarking preset strings that Nagra provides you.
-- Required when you include Nagra NexGuard File Marker watermarking
-- (NexGuardWatermarkingSettings) in your job.
--
-- 'strength', 'nexGuardFileMarkerSettings_strength' - Optional. Ignore this setting unless Nagra support directs you to
-- specify a value. When you don\'t specify a value here, the Nagra
-- NexGuard library uses its default value.
newNexGuardFileMarkerSettings ::
  NexGuardFileMarkerSettings
newNexGuardFileMarkerSettings =
  NexGuardFileMarkerSettings'
    { payload =
        Prelude.Nothing,
      license = Prelude.Nothing,
      preset = Prelude.Nothing,
      strength = Prelude.Nothing
    }

-- | Specify the payload ID that you want associated with this output. Valid
-- values vary depending on your Nagra NexGuard forensic watermarking
-- workflow. Required when you include Nagra NexGuard File Marker
-- watermarking (NexGuardWatermarkingSettings) in your job. For PreRelease
-- Content (NGPR\/G2), specify an integer from 1 through 4,194,303. You
-- must generate a unique ID for each asset you watermark, and keep a
-- record of which ID you have assigned to each asset. Neither Nagra nor
-- MediaConvert keep track of the relationship between output files and
-- your IDs. For OTT Streaming, create two adaptive bitrate (ABR) stacks
-- for each asset. Do this by setting up two output groups. For one output
-- group, set the value of Payload ID (payload) to 0 in every output. For
-- the other output group, set Payload ID (payload) to 1 in every output.
nexGuardFileMarkerSettings_payload :: Lens.Lens' NexGuardFileMarkerSettings (Prelude.Maybe Prelude.Natural)
nexGuardFileMarkerSettings_payload = Lens.lens (\NexGuardFileMarkerSettings' {payload} -> payload) (\s@NexGuardFileMarkerSettings' {} a -> s {payload = a} :: NexGuardFileMarkerSettings)

-- | Use the base64 license string that Nagra provides you. Enter it directly
-- in your JSON job specification or in the console. Required when you
-- include Nagra NexGuard File Marker watermarking
-- (NexGuardWatermarkingSettings) in your job.
nexGuardFileMarkerSettings_license :: Lens.Lens' NexGuardFileMarkerSettings (Prelude.Maybe Prelude.Text)
nexGuardFileMarkerSettings_license = Lens.lens (\NexGuardFileMarkerSettings' {license} -> license) (\s@NexGuardFileMarkerSettings' {} a -> s {license = a} :: NexGuardFileMarkerSettings)

-- | Enter one of the watermarking preset strings that Nagra provides you.
-- Required when you include Nagra NexGuard File Marker watermarking
-- (NexGuardWatermarkingSettings) in your job.
nexGuardFileMarkerSettings_preset :: Lens.Lens' NexGuardFileMarkerSettings (Prelude.Maybe Prelude.Text)
nexGuardFileMarkerSettings_preset = Lens.lens (\NexGuardFileMarkerSettings' {preset} -> preset) (\s@NexGuardFileMarkerSettings' {} a -> s {preset = a} :: NexGuardFileMarkerSettings)

-- | Optional. Ignore this setting unless Nagra support directs you to
-- specify a value. When you don\'t specify a value here, the Nagra
-- NexGuard library uses its default value.
nexGuardFileMarkerSettings_strength :: Lens.Lens' NexGuardFileMarkerSettings (Prelude.Maybe WatermarkingStrength)
nexGuardFileMarkerSettings_strength = Lens.lens (\NexGuardFileMarkerSettings' {strength} -> strength) (\s@NexGuardFileMarkerSettings' {} a -> s {strength = a} :: NexGuardFileMarkerSettings)

instance Core.FromJSON NexGuardFileMarkerSettings where
  parseJSON =
    Core.withObject
      "NexGuardFileMarkerSettings"
      ( \x ->
          NexGuardFileMarkerSettings'
            Prelude.<$> (x Core..:? "payload")
            Prelude.<*> (x Core..:? "license")
            Prelude.<*> (x Core..:? "preset")
            Prelude.<*> (x Core..:? "strength")
      )

instance Prelude.Hashable NexGuardFileMarkerSettings

instance Prelude.NFData NexGuardFileMarkerSettings

instance Core.ToJSON NexGuardFileMarkerSettings where
  toJSON NexGuardFileMarkerSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("payload" Core..=) Prelude.<$> payload,
            ("license" Core..=) Prelude.<$> license,
            ("preset" Core..=) Prelude.<$> preset,
            ("strength" Core..=) Prelude.<$> strength
          ]
      )
