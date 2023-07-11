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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.DashPackage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageVOD.Types.DashEncryption
import Amazonka.MediaPackageVOD.Types.DashManifest
import Amazonka.MediaPackageVOD.Types.PeriodTriggersElement
import Amazonka.MediaPackageVOD.Types.SegmentTemplateFormat
import qualified Amazonka.Prelude as Prelude

-- | A Dynamic Adaptive Streaming over HTTP (DASH) packaging configuration.
--
-- /See:/ 'newDashPackage' smart constructor.
data DashPackage = DashPackage'
  { encryption :: Prelude.Maybe DashEncryption,
    -- | When includeEncoderConfigurationInSegments is set to true, MediaPackage
    -- places your encoder\'s Sequence Parameter Set (SPS), Picture Parameter
    -- Set (PPS), and Video Parameter Set (VPS) metadata in every video segment
    -- instead of in the init fragment. This lets you use different
    -- SPS\/PPS\/VPS settings for your assets during content playback.
    includeEncoderConfigurationInSegments :: Prelude.Maybe Prelude.Bool,
    -- | When enabled, an I-Frame only stream will be included in the output.
    includeIframeOnlyStream :: Prelude.Maybe Prelude.Bool,
    -- | A list of triggers that controls when the outgoing Dynamic Adaptive
    -- Streaming over HTTP (DASH) Media Presentation Description (MPD) will be
    -- partitioned into multiple periods. If empty, the content will not be
    -- partitioned into more than one period. If the list contains \"ADS\", new
    -- periods will be created where the Asset contains SCTE-35 ad markers.
    periodTriggers :: Prelude.Maybe [PeriodTriggersElement],
    -- | Duration (in seconds) of each segment. Actual segments will be rounded
    -- to the nearest multiple of the source segment duration.
    segmentDurationSeconds :: Prelude.Maybe Prelude.Int,
    -- | Determines the type of SegmentTemplate included in the Media
    -- Presentation Description (MPD). When set to NUMBER_WITH_TIMELINE, a full
    -- timeline is presented in each SegmentTemplate, with $Number$ media URLs.
    -- When set to TIME_WITH_TIMELINE, a full timeline is presented in each
    -- SegmentTemplate, with $Time$ media URLs. When set to
    -- NUMBER_WITH_DURATION, only a duration is included in each
    -- SegmentTemplate, with $Number$ media URLs.
    segmentTemplateFormat :: Prelude.Maybe SegmentTemplateFormat,
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
-- 'encryption', 'dashPackage_encryption' - Undocumented member.
--
-- 'includeEncoderConfigurationInSegments', 'dashPackage_includeEncoderConfigurationInSegments' - When includeEncoderConfigurationInSegments is set to true, MediaPackage
-- places your encoder\'s Sequence Parameter Set (SPS), Picture Parameter
-- Set (PPS), and Video Parameter Set (VPS) metadata in every video segment
-- instead of in the init fragment. This lets you use different
-- SPS\/PPS\/VPS settings for your assets during content playback.
--
-- 'includeIframeOnlyStream', 'dashPackage_includeIframeOnlyStream' - When enabled, an I-Frame only stream will be included in the output.
--
-- 'periodTriggers', 'dashPackage_periodTriggers' - A list of triggers that controls when the outgoing Dynamic Adaptive
-- Streaming over HTTP (DASH) Media Presentation Description (MPD) will be
-- partitioned into multiple periods. If empty, the content will not be
-- partitioned into more than one period. If the list contains \"ADS\", new
-- periods will be created where the Asset contains SCTE-35 ad markers.
--
-- 'segmentDurationSeconds', 'dashPackage_segmentDurationSeconds' - Duration (in seconds) of each segment. Actual segments will be rounded
-- to the nearest multiple of the source segment duration.
--
-- 'segmentTemplateFormat', 'dashPackage_segmentTemplateFormat' - Determines the type of SegmentTemplate included in the Media
-- Presentation Description (MPD). When set to NUMBER_WITH_TIMELINE, a full
-- timeline is presented in each SegmentTemplate, with $Number$ media URLs.
-- When set to TIME_WITH_TIMELINE, a full timeline is presented in each
-- SegmentTemplate, with $Time$ media URLs. When set to
-- NUMBER_WITH_DURATION, only a duration is included in each
-- SegmentTemplate, with $Number$ media URLs.
--
-- 'dashManifests', 'dashPackage_dashManifests' - A list of DASH manifest configurations.
newDashPackage ::
  DashPackage
newDashPackage =
  DashPackage'
    { encryption = Prelude.Nothing,
      includeEncoderConfigurationInSegments =
        Prelude.Nothing,
      includeIframeOnlyStream = Prelude.Nothing,
      periodTriggers = Prelude.Nothing,
      segmentDurationSeconds = Prelude.Nothing,
      segmentTemplateFormat = Prelude.Nothing,
      dashManifests = Prelude.mempty
    }

-- | Undocumented member.
dashPackage_encryption :: Lens.Lens' DashPackage (Prelude.Maybe DashEncryption)
dashPackage_encryption = Lens.lens (\DashPackage' {encryption} -> encryption) (\s@DashPackage' {} a -> s {encryption = a} :: DashPackage)

-- | When includeEncoderConfigurationInSegments is set to true, MediaPackage
-- places your encoder\'s Sequence Parameter Set (SPS), Picture Parameter
-- Set (PPS), and Video Parameter Set (VPS) metadata in every video segment
-- instead of in the init fragment. This lets you use different
-- SPS\/PPS\/VPS settings for your assets during content playback.
dashPackage_includeEncoderConfigurationInSegments :: Lens.Lens' DashPackage (Prelude.Maybe Prelude.Bool)
dashPackage_includeEncoderConfigurationInSegments = Lens.lens (\DashPackage' {includeEncoderConfigurationInSegments} -> includeEncoderConfigurationInSegments) (\s@DashPackage' {} a -> s {includeEncoderConfigurationInSegments = a} :: DashPackage)

-- | When enabled, an I-Frame only stream will be included in the output.
dashPackage_includeIframeOnlyStream :: Lens.Lens' DashPackage (Prelude.Maybe Prelude.Bool)
dashPackage_includeIframeOnlyStream = Lens.lens (\DashPackage' {includeIframeOnlyStream} -> includeIframeOnlyStream) (\s@DashPackage' {} a -> s {includeIframeOnlyStream = a} :: DashPackage)

-- | A list of triggers that controls when the outgoing Dynamic Adaptive
-- Streaming over HTTP (DASH) Media Presentation Description (MPD) will be
-- partitioned into multiple periods. If empty, the content will not be
-- partitioned into more than one period. If the list contains \"ADS\", new
-- periods will be created where the Asset contains SCTE-35 ad markers.
dashPackage_periodTriggers :: Lens.Lens' DashPackage (Prelude.Maybe [PeriodTriggersElement])
dashPackage_periodTriggers = Lens.lens (\DashPackage' {periodTriggers} -> periodTriggers) (\s@DashPackage' {} a -> s {periodTriggers = a} :: DashPackage) Prelude.. Lens.mapping Lens.coerced

-- | Duration (in seconds) of each segment. Actual segments will be rounded
-- to the nearest multiple of the source segment duration.
dashPackage_segmentDurationSeconds :: Lens.Lens' DashPackage (Prelude.Maybe Prelude.Int)
dashPackage_segmentDurationSeconds = Lens.lens (\DashPackage' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@DashPackage' {} a -> s {segmentDurationSeconds = a} :: DashPackage)

-- | Determines the type of SegmentTemplate included in the Media
-- Presentation Description (MPD). When set to NUMBER_WITH_TIMELINE, a full
-- timeline is presented in each SegmentTemplate, with $Number$ media URLs.
-- When set to TIME_WITH_TIMELINE, a full timeline is presented in each
-- SegmentTemplate, with $Time$ media URLs. When set to
-- NUMBER_WITH_DURATION, only a duration is included in each
-- SegmentTemplate, with $Number$ media URLs.
dashPackage_segmentTemplateFormat :: Lens.Lens' DashPackage (Prelude.Maybe SegmentTemplateFormat)
dashPackage_segmentTemplateFormat = Lens.lens (\DashPackage' {segmentTemplateFormat} -> segmentTemplateFormat) (\s@DashPackage' {} a -> s {segmentTemplateFormat = a} :: DashPackage)

-- | A list of DASH manifest configurations.
dashPackage_dashManifests :: Lens.Lens' DashPackage [DashManifest]
dashPackage_dashManifests = Lens.lens (\DashPackage' {dashManifests} -> dashManifests) (\s@DashPackage' {} a -> s {dashManifests = a} :: DashPackage) Prelude.. Lens.coerced

instance Data.FromJSON DashPackage where
  parseJSON =
    Data.withObject
      "DashPackage"
      ( \x ->
          DashPackage'
            Prelude.<$> (x Data..:? "encryption")
            Prelude.<*> (x Data..:? "includeEncoderConfigurationInSegments")
            Prelude.<*> (x Data..:? "includeIframeOnlyStream")
            Prelude.<*> (x Data..:? "periodTriggers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "segmentDurationSeconds")
            Prelude.<*> (x Data..:? "segmentTemplateFormat")
            Prelude.<*> (x Data..:? "dashManifests" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DashPackage where
  hashWithSalt _salt DashPackage' {..} =
    _salt
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` includeEncoderConfigurationInSegments
      `Prelude.hashWithSalt` includeIframeOnlyStream
      `Prelude.hashWithSalt` periodTriggers
      `Prelude.hashWithSalt` segmentDurationSeconds
      `Prelude.hashWithSalt` segmentTemplateFormat
      `Prelude.hashWithSalt` dashManifests

instance Prelude.NFData DashPackage where
  rnf DashPackage' {..} =
    Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf includeEncoderConfigurationInSegments
      `Prelude.seq` Prelude.rnf includeIframeOnlyStream
      `Prelude.seq` Prelude.rnf periodTriggers
      `Prelude.seq` Prelude.rnf segmentDurationSeconds
      `Prelude.seq` Prelude.rnf segmentTemplateFormat
      `Prelude.seq` Prelude.rnf dashManifests

instance Data.ToJSON DashPackage where
  toJSON DashPackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("encryption" Data..=) Prelude.<$> encryption,
            ("includeEncoderConfigurationInSegments" Data..=)
              Prelude.<$> includeEncoderConfigurationInSegments,
            ("includeIframeOnlyStream" Data..=)
              Prelude.<$> includeIframeOnlyStream,
            ("periodTriggers" Data..=)
              Prelude.<$> periodTriggers,
            ("segmentDurationSeconds" Data..=)
              Prelude.<$> segmentDurationSeconds,
            ("segmentTemplateFormat" Data..=)
              Prelude.<$> segmentTemplateFormat,
            Prelude.Just
              ("dashManifests" Data..= dashManifests)
          ]
      )
