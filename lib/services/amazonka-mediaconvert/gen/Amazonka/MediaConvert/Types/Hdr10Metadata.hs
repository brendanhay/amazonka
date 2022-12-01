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
-- Module      : Amazonka.MediaConvert.Types.Hdr10Metadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Hdr10Metadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Use these settings to specify static color calibration metadata, as
-- defined by SMPTE ST 2086. These values don\'t affect the pixel values
-- that are encoded in the video stream. They are intended to help the
-- downstream video player display content in a way that reflects the
-- intentions of the the content creator.
--
-- /See:/ 'newHdr10Metadata' smart constructor.
data Hdr10Metadata = Hdr10Metadata'
  { -- | HDR Master Display Information must be provided by a color grader, using
    -- color grading tools. Range is 0 to 50,000, each increment represents
    -- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
    -- color correction.
    redPrimaryX :: Prelude.Maybe Prelude.Natural,
    -- | HDR Master Display Information must be provided by a color grader, using
    -- color grading tools. Range is 0 to 50,000, each increment represents
    -- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
    -- color correction.
    whitePointY :: Prelude.Maybe Prelude.Natural,
    -- | HDR Master Display Information must be provided by a color grader, using
    -- color grading tools. Range is 0 to 50,000, each increment represents
    -- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
    -- color correction.
    greenPrimaryX :: Prelude.Maybe Prelude.Natural,
    -- | Maximum light level among all samples in the coded video sequence, in
    -- units of candelas per square meter. This setting doesn\'t have a default
    -- value; you must specify a value that is suitable for the content.
    maxContentLightLevel :: Prelude.Maybe Prelude.Natural,
    -- | HDR Master Display Information must be provided by a color grader, using
    -- color grading tools. Range is 0 to 50,000, each increment represents
    -- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
    -- color correction.
    redPrimaryY :: Prelude.Maybe Prelude.Natural,
    -- | HDR Master Display Information must be provided by a color grader, using
    -- color grading tools. Range is 0 to 50,000, each increment represents
    -- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
    -- color correction.
    bluePrimaryX :: Prelude.Maybe Prelude.Natural,
    -- | HDR Master Display Information must be provided by a color grader, using
    -- color grading tools. Range is 0 to 50,000, each increment represents
    -- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
    -- color correction.
    whitePointX :: Prelude.Maybe Prelude.Natural,
    -- | Nominal maximum mastering display luminance in units of of 0.0001
    -- candelas per square meter.
    maxLuminance :: Prelude.Maybe Prelude.Natural,
    -- | Maximum average light level of any frame in the coded video sequence, in
    -- units of candelas per square meter. This setting doesn\'t have a default
    -- value; you must specify a value that is suitable for the content.
    maxFrameAverageLightLevel :: Prelude.Maybe Prelude.Natural,
    -- | HDR Master Display Information must be provided by a color grader, using
    -- color grading tools. Range is 0 to 50,000, each increment represents
    -- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
    -- color correction.
    greenPrimaryY :: Prelude.Maybe Prelude.Natural,
    -- | Nominal minimum mastering display luminance in units of of 0.0001
    -- candelas per square meter
    minLuminance :: Prelude.Maybe Prelude.Natural,
    -- | HDR Master Display Information must be provided by a color grader, using
    -- color grading tools. Range is 0 to 50,000, each increment represents
    -- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
    -- color correction.
    bluePrimaryY :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Hdr10Metadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'redPrimaryX', 'hdr10Metadata_redPrimaryX' - HDR Master Display Information must be provided by a color grader, using
-- color grading tools. Range is 0 to 50,000, each increment represents
-- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
-- color correction.
--
-- 'whitePointY', 'hdr10Metadata_whitePointY' - HDR Master Display Information must be provided by a color grader, using
-- color grading tools. Range is 0 to 50,000, each increment represents
-- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
-- color correction.
--
-- 'greenPrimaryX', 'hdr10Metadata_greenPrimaryX' - HDR Master Display Information must be provided by a color grader, using
-- color grading tools. Range is 0 to 50,000, each increment represents
-- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
-- color correction.
--
-- 'maxContentLightLevel', 'hdr10Metadata_maxContentLightLevel' - Maximum light level among all samples in the coded video sequence, in
-- units of candelas per square meter. This setting doesn\'t have a default
-- value; you must specify a value that is suitable for the content.
--
-- 'redPrimaryY', 'hdr10Metadata_redPrimaryY' - HDR Master Display Information must be provided by a color grader, using
-- color grading tools. Range is 0 to 50,000, each increment represents
-- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
-- color correction.
--
-- 'bluePrimaryX', 'hdr10Metadata_bluePrimaryX' - HDR Master Display Information must be provided by a color grader, using
-- color grading tools. Range is 0 to 50,000, each increment represents
-- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
-- color correction.
--
-- 'whitePointX', 'hdr10Metadata_whitePointX' - HDR Master Display Information must be provided by a color grader, using
-- color grading tools. Range is 0 to 50,000, each increment represents
-- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
-- color correction.
--
-- 'maxLuminance', 'hdr10Metadata_maxLuminance' - Nominal maximum mastering display luminance in units of of 0.0001
-- candelas per square meter.
--
-- 'maxFrameAverageLightLevel', 'hdr10Metadata_maxFrameAverageLightLevel' - Maximum average light level of any frame in the coded video sequence, in
-- units of candelas per square meter. This setting doesn\'t have a default
-- value; you must specify a value that is suitable for the content.
--
-- 'greenPrimaryY', 'hdr10Metadata_greenPrimaryY' - HDR Master Display Information must be provided by a color grader, using
-- color grading tools. Range is 0 to 50,000, each increment represents
-- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
-- color correction.
--
-- 'minLuminance', 'hdr10Metadata_minLuminance' - Nominal minimum mastering display luminance in units of of 0.0001
-- candelas per square meter
--
-- 'bluePrimaryY', 'hdr10Metadata_bluePrimaryY' - HDR Master Display Information must be provided by a color grader, using
-- color grading tools. Range is 0 to 50,000, each increment represents
-- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
-- color correction.
newHdr10Metadata ::
  Hdr10Metadata
newHdr10Metadata =
  Hdr10Metadata'
    { redPrimaryX = Prelude.Nothing,
      whitePointY = Prelude.Nothing,
      greenPrimaryX = Prelude.Nothing,
      maxContentLightLevel = Prelude.Nothing,
      redPrimaryY = Prelude.Nothing,
      bluePrimaryX = Prelude.Nothing,
      whitePointX = Prelude.Nothing,
      maxLuminance = Prelude.Nothing,
      maxFrameAverageLightLevel = Prelude.Nothing,
      greenPrimaryY = Prelude.Nothing,
      minLuminance = Prelude.Nothing,
      bluePrimaryY = Prelude.Nothing
    }

-- | HDR Master Display Information must be provided by a color grader, using
-- color grading tools. Range is 0 to 50,000, each increment represents
-- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
-- color correction.
hdr10Metadata_redPrimaryX :: Lens.Lens' Hdr10Metadata (Prelude.Maybe Prelude.Natural)
hdr10Metadata_redPrimaryX = Lens.lens (\Hdr10Metadata' {redPrimaryX} -> redPrimaryX) (\s@Hdr10Metadata' {} a -> s {redPrimaryX = a} :: Hdr10Metadata)

-- | HDR Master Display Information must be provided by a color grader, using
-- color grading tools. Range is 0 to 50,000, each increment represents
-- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
-- color correction.
hdr10Metadata_whitePointY :: Lens.Lens' Hdr10Metadata (Prelude.Maybe Prelude.Natural)
hdr10Metadata_whitePointY = Lens.lens (\Hdr10Metadata' {whitePointY} -> whitePointY) (\s@Hdr10Metadata' {} a -> s {whitePointY = a} :: Hdr10Metadata)

-- | HDR Master Display Information must be provided by a color grader, using
-- color grading tools. Range is 0 to 50,000, each increment represents
-- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
-- color correction.
hdr10Metadata_greenPrimaryX :: Lens.Lens' Hdr10Metadata (Prelude.Maybe Prelude.Natural)
hdr10Metadata_greenPrimaryX = Lens.lens (\Hdr10Metadata' {greenPrimaryX} -> greenPrimaryX) (\s@Hdr10Metadata' {} a -> s {greenPrimaryX = a} :: Hdr10Metadata)

-- | Maximum light level among all samples in the coded video sequence, in
-- units of candelas per square meter. This setting doesn\'t have a default
-- value; you must specify a value that is suitable for the content.
hdr10Metadata_maxContentLightLevel :: Lens.Lens' Hdr10Metadata (Prelude.Maybe Prelude.Natural)
hdr10Metadata_maxContentLightLevel = Lens.lens (\Hdr10Metadata' {maxContentLightLevel} -> maxContentLightLevel) (\s@Hdr10Metadata' {} a -> s {maxContentLightLevel = a} :: Hdr10Metadata)

-- | HDR Master Display Information must be provided by a color grader, using
-- color grading tools. Range is 0 to 50,000, each increment represents
-- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
-- color correction.
hdr10Metadata_redPrimaryY :: Lens.Lens' Hdr10Metadata (Prelude.Maybe Prelude.Natural)
hdr10Metadata_redPrimaryY = Lens.lens (\Hdr10Metadata' {redPrimaryY} -> redPrimaryY) (\s@Hdr10Metadata' {} a -> s {redPrimaryY = a} :: Hdr10Metadata)

-- | HDR Master Display Information must be provided by a color grader, using
-- color grading tools. Range is 0 to 50,000, each increment represents
-- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
-- color correction.
hdr10Metadata_bluePrimaryX :: Lens.Lens' Hdr10Metadata (Prelude.Maybe Prelude.Natural)
hdr10Metadata_bluePrimaryX = Lens.lens (\Hdr10Metadata' {bluePrimaryX} -> bluePrimaryX) (\s@Hdr10Metadata' {} a -> s {bluePrimaryX = a} :: Hdr10Metadata)

-- | HDR Master Display Information must be provided by a color grader, using
-- color grading tools. Range is 0 to 50,000, each increment represents
-- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
-- color correction.
hdr10Metadata_whitePointX :: Lens.Lens' Hdr10Metadata (Prelude.Maybe Prelude.Natural)
hdr10Metadata_whitePointX = Lens.lens (\Hdr10Metadata' {whitePointX} -> whitePointX) (\s@Hdr10Metadata' {} a -> s {whitePointX = a} :: Hdr10Metadata)

-- | Nominal maximum mastering display luminance in units of of 0.0001
-- candelas per square meter.
hdr10Metadata_maxLuminance :: Lens.Lens' Hdr10Metadata (Prelude.Maybe Prelude.Natural)
hdr10Metadata_maxLuminance = Lens.lens (\Hdr10Metadata' {maxLuminance} -> maxLuminance) (\s@Hdr10Metadata' {} a -> s {maxLuminance = a} :: Hdr10Metadata)

-- | Maximum average light level of any frame in the coded video sequence, in
-- units of candelas per square meter. This setting doesn\'t have a default
-- value; you must specify a value that is suitable for the content.
hdr10Metadata_maxFrameAverageLightLevel :: Lens.Lens' Hdr10Metadata (Prelude.Maybe Prelude.Natural)
hdr10Metadata_maxFrameAverageLightLevel = Lens.lens (\Hdr10Metadata' {maxFrameAverageLightLevel} -> maxFrameAverageLightLevel) (\s@Hdr10Metadata' {} a -> s {maxFrameAverageLightLevel = a} :: Hdr10Metadata)

-- | HDR Master Display Information must be provided by a color grader, using
-- color grading tools. Range is 0 to 50,000, each increment represents
-- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
-- color correction.
hdr10Metadata_greenPrimaryY :: Lens.Lens' Hdr10Metadata (Prelude.Maybe Prelude.Natural)
hdr10Metadata_greenPrimaryY = Lens.lens (\Hdr10Metadata' {greenPrimaryY} -> greenPrimaryY) (\s@Hdr10Metadata' {} a -> s {greenPrimaryY = a} :: Hdr10Metadata)

-- | Nominal minimum mastering display luminance in units of of 0.0001
-- candelas per square meter
hdr10Metadata_minLuminance :: Lens.Lens' Hdr10Metadata (Prelude.Maybe Prelude.Natural)
hdr10Metadata_minLuminance = Lens.lens (\Hdr10Metadata' {minLuminance} -> minLuminance) (\s@Hdr10Metadata' {} a -> s {minLuminance = a} :: Hdr10Metadata)

-- | HDR Master Display Information must be provided by a color grader, using
-- color grading tools. Range is 0 to 50,000, each increment represents
-- 0.00002 in CIE1931 color coordinate. Note that this setting is not for
-- color correction.
hdr10Metadata_bluePrimaryY :: Lens.Lens' Hdr10Metadata (Prelude.Maybe Prelude.Natural)
hdr10Metadata_bluePrimaryY = Lens.lens (\Hdr10Metadata' {bluePrimaryY} -> bluePrimaryY) (\s@Hdr10Metadata' {} a -> s {bluePrimaryY = a} :: Hdr10Metadata)

instance Core.FromJSON Hdr10Metadata where
  parseJSON =
    Core.withObject
      "Hdr10Metadata"
      ( \x ->
          Hdr10Metadata'
            Prelude.<$> (x Core..:? "redPrimaryX")
            Prelude.<*> (x Core..:? "whitePointY")
            Prelude.<*> (x Core..:? "greenPrimaryX")
            Prelude.<*> (x Core..:? "maxContentLightLevel")
            Prelude.<*> (x Core..:? "redPrimaryY")
            Prelude.<*> (x Core..:? "bluePrimaryX")
            Prelude.<*> (x Core..:? "whitePointX")
            Prelude.<*> (x Core..:? "maxLuminance")
            Prelude.<*> (x Core..:? "maxFrameAverageLightLevel")
            Prelude.<*> (x Core..:? "greenPrimaryY")
            Prelude.<*> (x Core..:? "minLuminance")
            Prelude.<*> (x Core..:? "bluePrimaryY")
      )

instance Prelude.Hashable Hdr10Metadata where
  hashWithSalt _salt Hdr10Metadata' {..} =
    _salt `Prelude.hashWithSalt` redPrimaryX
      `Prelude.hashWithSalt` whitePointY
      `Prelude.hashWithSalt` greenPrimaryX
      `Prelude.hashWithSalt` maxContentLightLevel
      `Prelude.hashWithSalt` redPrimaryY
      `Prelude.hashWithSalt` bluePrimaryX
      `Prelude.hashWithSalt` whitePointX
      `Prelude.hashWithSalt` maxLuminance
      `Prelude.hashWithSalt` maxFrameAverageLightLevel
      `Prelude.hashWithSalt` greenPrimaryY
      `Prelude.hashWithSalt` minLuminance
      `Prelude.hashWithSalt` bluePrimaryY

instance Prelude.NFData Hdr10Metadata where
  rnf Hdr10Metadata' {..} =
    Prelude.rnf redPrimaryX
      `Prelude.seq` Prelude.rnf whitePointY
      `Prelude.seq` Prelude.rnf greenPrimaryX
      `Prelude.seq` Prelude.rnf maxContentLightLevel
      `Prelude.seq` Prelude.rnf redPrimaryY
      `Prelude.seq` Prelude.rnf bluePrimaryX
      `Prelude.seq` Prelude.rnf whitePointX
      `Prelude.seq` Prelude.rnf maxLuminance
      `Prelude.seq` Prelude.rnf maxFrameAverageLightLevel
      `Prelude.seq` Prelude.rnf greenPrimaryY
      `Prelude.seq` Prelude.rnf minLuminance
      `Prelude.seq` Prelude.rnf bluePrimaryY

instance Core.ToJSON Hdr10Metadata where
  toJSON Hdr10Metadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("redPrimaryX" Core..=) Prelude.<$> redPrimaryX,
            ("whitePointY" Core..=) Prelude.<$> whitePointY,
            ("greenPrimaryX" Core..=) Prelude.<$> greenPrimaryX,
            ("maxContentLightLevel" Core..=)
              Prelude.<$> maxContentLightLevel,
            ("redPrimaryY" Core..=) Prelude.<$> redPrimaryY,
            ("bluePrimaryX" Core..=) Prelude.<$> bluePrimaryX,
            ("whitePointX" Core..=) Prelude.<$> whitePointX,
            ("maxLuminance" Core..=) Prelude.<$> maxLuminance,
            ("maxFrameAverageLightLevel" Core..=)
              Prelude.<$> maxFrameAverageLightLevel,
            ("greenPrimaryY" Core..=) Prelude.<$> greenPrimaryY,
            ("minLuminance" Core..=) Prelude.<$> minLuminance,
            ("bluePrimaryY" Core..=) Prelude.<$> bluePrimaryY
          ]
      )
