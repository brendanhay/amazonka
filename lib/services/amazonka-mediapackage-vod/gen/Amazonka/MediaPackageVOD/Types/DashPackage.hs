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
-- Module      : Amazonka.MediaPackageVOD.Types.DashPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.DashPackage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaPackageVOD.Types.DashEncryption
import Amazonka.MediaPackageVOD.Types.DashManifest
import Amazonka.MediaPackageVOD.Types.PeriodTriggersElement
import Amazonka.MediaPackageVOD.Types.SegmentTemplateFormat
import qualified Amazonka.Prelude as Prelude

-- | A Dynamic Adaptive Streaming over HTTP (DASH) packaging configuration.
--
-- /See:/ 'newDashPackage' smart constructor.
data DashPackage = DashPackage'
  { -- | When includeEncoderConfigurationInSegments is set to true, MediaPackage
    -- places your encoder\'s Sequence Parameter Set (SPS), Picture Parameter
    -- Set (PPS), and Video Parameter Set (VPS) metadata in every video segment
    -- instead of in the init fragment. This lets you use different
    -- SPS\/PPS\/VPS settings for your assets during content playback.
    includeEncoderConfigurationInSegments :: Prelude.Maybe Prelude.Bool,
    -- | Determines the type of SegmentTemplate included in the Media
    -- Presentation Description (MPD). When set to NUMBER_WITH_TIMELINE, a full
    -- timeline is presented in each SegmentTemplate, with $Number$ media URLs.
    -- When set to TIME_WITH_TIMELINE, a full timeline is presented in each
    -- SegmentTemplate, with $Time$ media URLs. When set to
    -- NUMBER_WITH_DURATION, only a duration is included in each
    -- SegmentTemplate, with $Number$ media URLs.
    segmentTemplateFormat :: Prelude.Maybe SegmentTemplateFormat,
    -- | Duration (in seconds) of each segment. Actual segments will be rounded
    -- to the nearest multiple of the source segment duration.
    segmentDurationSeconds :: Prelude.Maybe Prelude.Int,
    encryption :: Prelude.Maybe DashEncryption,
    -- | A list of triggers that controls when the outgoing Dynamic Adaptive
    -- Streaming over HTTP (DASH) Media Presentation Description (MPD) will be
    -- partitioned into multiple periods. If empty, the content will not be
    -- partitioned into more than one period. If the list contains \"ADS\", new
    -- periods will be created where the Asset contains SCTE-35 ad markers.
    periodTriggers :: Prelude.Maybe [PeriodTriggersElement],
    -- | A list of DASH manifest configurations.
    dashManifests :: [DashManifest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeEncoderConfigurationInSegments', 'dashPackage_includeEncoderConfigurationInSegments' - When includeEncoderConfigurationInSegments is set to true, MediaPackage
-- places your encoder\'s Sequence Parameter Set (SPS), Picture Parameter
-- Set (PPS), and Video Parameter Set (VPS) metadata in every video segment
-- instead of in the init fragment. This lets you use different
-- SPS\/PPS\/VPS settings for your assets during content playback.
--
-- 'segmentTemplateFormat', 'dashPackage_segmentTemplateFormat' - Determines the type of SegmentTemplate included in the Media
-- Presentation Description (MPD). When set to NUMBER_WITH_TIMELINE, a full
-- timeline is presented in each SegmentTemplate, with $Number$ media URLs.
-- When set to TIME_WITH_TIMELINE, a full timeline is presented in each
-- SegmentTemplate, with $Time$ media URLs. When set to
-- NUMBER_WITH_DURATION, only a duration is included in each
-- SegmentTemplate, with $Number$ media URLs.
--
-- 'segmentDurationSeconds', 'dashPackage_segmentDurationSeconds' - Duration (in seconds) of each segment. Actual segments will be rounded
-- to the nearest multiple of the source segment duration.
--
-- 'encryption', 'dashPackage_encryption' - Undocumented member.
--
-- 'periodTriggers', 'dashPackage_periodTriggers' - A list of triggers that controls when the outgoing Dynamic Adaptive
-- Streaming over HTTP (DASH) Media Presentation Description (MPD) will be
-- partitioned into multiple periods. If empty, the content will not be
-- partitioned into more than one period. If the list contains \"ADS\", new
-- periods will be created where the Asset contains SCTE-35 ad markers.
--
-- 'dashManifests', 'dashPackage_dashManifests' - A list of DASH manifest configurations.
newDashPackage ::
  DashPackage
newDashPackage =
  DashPackage'
    { includeEncoderConfigurationInSegments =
        Prelude.Nothing,
      segmentTemplateFormat = Prelude.Nothing,
      segmentDurationSeconds = Prelude.Nothing,
      encryption = Prelude.Nothing,
      periodTriggers = Prelude.Nothing,
      dashManifests = Prelude.mempty
    }

-- | When includeEncoderConfigurationInSegments is set to true, MediaPackage
-- places your encoder\'s Sequence Parameter Set (SPS), Picture Parameter
-- Set (PPS), and Video Parameter Set (VPS) metadata in every video segment
-- instead of in the init fragment. This lets you use different
-- SPS\/PPS\/VPS settings for your assets during content playback.
dashPackage_includeEncoderConfigurationInSegments :: Lens.Lens' DashPackage (Prelude.Maybe Prelude.Bool)
dashPackage_includeEncoderConfigurationInSegments = Lens.lens (\DashPackage' {includeEncoderConfigurationInSegments} -> includeEncoderConfigurationInSegments) (\s@DashPackage' {} a -> s {includeEncoderConfigurationInSegments = a} :: DashPackage)

-- | Determines the type of SegmentTemplate included in the Media
-- Presentation Description (MPD). When set to NUMBER_WITH_TIMELINE, a full
-- timeline is presented in each SegmentTemplate, with $Number$ media URLs.
-- When set to TIME_WITH_TIMELINE, a full timeline is presented in each
-- SegmentTemplate, with $Time$ media URLs. When set to
-- NUMBER_WITH_DURATION, only a duration is included in each
-- SegmentTemplate, with $Number$ media URLs.
dashPackage_segmentTemplateFormat :: Lens.Lens' DashPackage (Prelude.Maybe SegmentTemplateFormat)
dashPackage_segmentTemplateFormat = Lens.lens (\DashPackage' {segmentTemplateFormat} -> segmentTemplateFormat) (\s@DashPackage' {} a -> s {segmentTemplateFormat = a} :: DashPackage)

-- | Duration (in seconds) of each segment. Actual segments will be rounded
-- to the nearest multiple of the source segment duration.
dashPackage_segmentDurationSeconds :: Lens.Lens' DashPackage (Prelude.Maybe Prelude.Int)
dashPackage_segmentDurationSeconds = Lens.lens (\DashPackage' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@DashPackage' {} a -> s {segmentDurationSeconds = a} :: DashPackage)

-- | Undocumented member.
dashPackage_encryption :: Lens.Lens' DashPackage (Prelude.Maybe DashEncryption)
dashPackage_encryption = Lens.lens (\DashPackage' {encryption} -> encryption) (\s@DashPackage' {} a -> s {encryption = a} :: DashPackage)

-- | A list of triggers that controls when the outgoing Dynamic Adaptive
-- Streaming over HTTP (DASH) Media Presentation Description (MPD) will be
-- partitioned into multiple periods. If empty, the content will not be
-- partitioned into more than one period. If the list contains \"ADS\", new
-- periods will be created where the Asset contains SCTE-35 ad markers.
dashPackage_periodTriggers :: Lens.Lens' DashPackage (Prelude.Maybe [PeriodTriggersElement])
dashPackage_periodTriggers = Lens.lens (\DashPackage' {periodTriggers} -> periodTriggers) (\s@DashPackage' {} a -> s {periodTriggers = a} :: DashPackage) Prelude.. Lens.mapping Lens.coerced

-- | A list of DASH manifest configurations.
dashPackage_dashManifests :: Lens.Lens' DashPackage [DashManifest]
dashPackage_dashManifests = Lens.lens (\DashPackage' {dashManifests} -> dashManifests) (\s@DashPackage' {} a -> s {dashManifests = a} :: DashPackage) Prelude.. Lens.coerced

instance Core.FromJSON DashPackage where
  parseJSON =
    Core.withObject
      "DashPackage"
      ( \x ->
          DashPackage'
            Prelude.<$> (x Core..:? "includeEncoderConfigurationInSegments")
            Prelude.<*> (x Core..:? "segmentTemplateFormat")
            Prelude.<*> (x Core..:? "segmentDurationSeconds")
            Prelude.<*> (x Core..:? "encryption")
            Prelude.<*> (x Core..:? "periodTriggers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "dashManifests" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable DashPackage where
  hashWithSalt salt' DashPackage' {..} =
    salt' `Prelude.hashWithSalt` dashManifests
      `Prelude.hashWithSalt` periodTriggers
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` segmentDurationSeconds
      `Prelude.hashWithSalt` segmentTemplateFormat
      `Prelude.hashWithSalt` includeEncoderConfigurationInSegments

instance Prelude.NFData DashPackage where
  rnf DashPackage' {..} =
    Prelude.rnf includeEncoderConfigurationInSegments
      `Prelude.seq` Prelude.rnf dashManifests
      `Prelude.seq` Prelude.rnf periodTriggers
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf segmentDurationSeconds
      `Prelude.seq` Prelude.rnf segmentTemplateFormat

instance Core.ToJSON DashPackage where
  toJSON DashPackage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("includeEncoderConfigurationInSegments" Core..=)
              Prelude.<$> includeEncoderConfigurationInSegments,
            ("segmentTemplateFormat" Core..=)
              Prelude.<$> segmentTemplateFormat,
            ("segmentDurationSeconds" Core..=)
              Prelude.<$> segmentDurationSeconds,
            ("encryption" Core..=) Prelude.<$> encryption,
            ("periodTriggers" Core..=)
              Prelude.<$> periodTriggers,
            Prelude.Just
              ("dashManifests" Core..= dashManifests)
          ]
      )
