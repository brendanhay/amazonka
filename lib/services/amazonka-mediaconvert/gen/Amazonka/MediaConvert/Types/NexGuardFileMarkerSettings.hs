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
-- Module      : Amazonka.MediaConvert.Types.NexGuardFileMarkerSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.NexGuardFileMarkerSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.WatermarkingStrength
import qualified Amazonka.Prelude as Prelude

-- | For forensic video watermarking, MediaConvert supports Nagra NexGuard
-- File Marker watermarking. MediaConvert supports both PreRelease Content
-- (NGPR\/G2) and OTT Streaming workflows.
--
-- /See:/ 'newNexGuardFileMarkerSettings' smart constructor.
data NexGuardFileMarkerSettings = NexGuardFileMarkerSettings'
  { -- | Optional. Ignore this setting unless Nagra support directs you to
    -- specify a value. When you don\'t specify a value here, the Nagra
    -- NexGuard library uses its default value.
    strength :: Prelude.Maybe WatermarkingStrength,
    -- | Use the base64 license string that Nagra provides you. Enter it directly
    -- in your JSON job specification or in the console. Required when you
    -- include Nagra NexGuard File Marker watermarking
    -- (NexGuardWatermarkingSettings) in your job.
    license :: Prelude.Maybe Prelude.Text,
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
    payload :: Prelude.Maybe Prelude.Natural,
    -- | Enter one of the watermarking preset strings that Nagra provides you.
    -- Required when you include Nagra NexGuard File Marker watermarking
    -- (NexGuardWatermarkingSettings) in your job.
    preset :: Prelude.Maybe Prelude.Text
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
-- 'strength', 'nexGuardFileMarkerSettings_strength' - Optional. Ignore this setting unless Nagra support directs you to
-- specify a value. When you don\'t specify a value here, the Nagra
-- NexGuard library uses its default value.
--
-- 'license', 'nexGuardFileMarkerSettings_license' - Use the base64 license string that Nagra provides you. Enter it directly
-- in your JSON job specification or in the console. Required when you
-- include Nagra NexGuard File Marker watermarking
-- (NexGuardWatermarkingSettings) in your job.
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
-- 'preset', 'nexGuardFileMarkerSettings_preset' - Enter one of the watermarking preset strings that Nagra provides you.
-- Required when you include Nagra NexGuard File Marker watermarking
-- (NexGuardWatermarkingSettings) in your job.
newNexGuardFileMarkerSettings ::
  NexGuardFileMarkerSettings
newNexGuardFileMarkerSettings =
  NexGuardFileMarkerSettings'
    { strength =
        Prelude.Nothing,
      license = Prelude.Nothing,
      payload = Prelude.Nothing,
      preset = Prelude.Nothing
    }

-- | Optional. Ignore this setting unless Nagra support directs you to
-- specify a value. When you don\'t specify a value here, the Nagra
-- NexGuard library uses its default value.
nexGuardFileMarkerSettings_strength :: Lens.Lens' NexGuardFileMarkerSettings (Prelude.Maybe WatermarkingStrength)
nexGuardFileMarkerSettings_strength = Lens.lens (\NexGuardFileMarkerSettings' {strength} -> strength) (\s@NexGuardFileMarkerSettings' {} a -> s {strength = a} :: NexGuardFileMarkerSettings)

-- | Use the base64 license string that Nagra provides you. Enter it directly
-- in your JSON job specification or in the console. Required when you
-- include Nagra NexGuard File Marker watermarking
-- (NexGuardWatermarkingSettings) in your job.
nexGuardFileMarkerSettings_license :: Lens.Lens' NexGuardFileMarkerSettings (Prelude.Maybe Prelude.Text)
nexGuardFileMarkerSettings_license = Lens.lens (\NexGuardFileMarkerSettings' {license} -> license) (\s@NexGuardFileMarkerSettings' {} a -> s {license = a} :: NexGuardFileMarkerSettings)

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

-- | Enter one of the watermarking preset strings that Nagra provides you.
-- Required when you include Nagra NexGuard File Marker watermarking
-- (NexGuardWatermarkingSettings) in your job.
nexGuardFileMarkerSettings_preset :: Lens.Lens' NexGuardFileMarkerSettings (Prelude.Maybe Prelude.Text)
nexGuardFileMarkerSettings_preset = Lens.lens (\NexGuardFileMarkerSettings' {preset} -> preset) (\s@NexGuardFileMarkerSettings' {} a -> s {preset = a} :: NexGuardFileMarkerSettings)

instance Data.FromJSON NexGuardFileMarkerSettings where
  parseJSON =
    Data.withObject
      "NexGuardFileMarkerSettings"
      ( \x ->
          NexGuardFileMarkerSettings'
            Prelude.<$> (x Data..:? "strength")
            Prelude.<*> (x Data..:? "license")
            Prelude.<*> (x Data..:? "payload")
            Prelude.<*> (x Data..:? "preset")
      )

instance Prelude.Hashable NexGuardFileMarkerSettings where
  hashWithSalt _salt NexGuardFileMarkerSettings' {..} =
    _salt `Prelude.hashWithSalt` strength
      `Prelude.hashWithSalt` license
      `Prelude.hashWithSalt` payload
      `Prelude.hashWithSalt` preset

instance Prelude.NFData NexGuardFileMarkerSettings where
  rnf NexGuardFileMarkerSettings' {..} =
    Prelude.rnf strength
      `Prelude.seq` Prelude.rnf license
      `Prelude.seq` Prelude.rnf payload
      `Prelude.seq` Prelude.rnf preset

instance Data.ToJSON NexGuardFileMarkerSettings where
  toJSON NexGuardFileMarkerSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("strength" Data..=) Prelude.<$> strength,
            ("license" Data..=) Prelude.<$> license,
            ("payload" Data..=) Prelude.<$> payload,
            ("preset" Data..=) Prelude.<$> preset
          ]
      )
