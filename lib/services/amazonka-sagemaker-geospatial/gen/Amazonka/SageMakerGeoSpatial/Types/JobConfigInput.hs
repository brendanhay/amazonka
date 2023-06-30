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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.JobConfigInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.JobConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.BandMathConfigInput
import Amazonka.SageMakerGeoSpatial.Types.CloudMaskingConfigInput
import Amazonka.SageMakerGeoSpatial.Types.CloudRemovalConfigInput
import Amazonka.SageMakerGeoSpatial.Types.GeoMosaicConfigInput
import Amazonka.SageMakerGeoSpatial.Types.LandCoverSegmentationConfigInput
import Amazonka.SageMakerGeoSpatial.Types.ResamplingConfigInput
import Amazonka.SageMakerGeoSpatial.Types.StackConfigInput
import Amazonka.SageMakerGeoSpatial.Types.TemporalStatisticsConfigInput
import Amazonka.SageMakerGeoSpatial.Types.ZonalStatisticsConfigInput

-- | The input structure for the JobConfig in an EarthObservationJob.
--
-- /See:/ 'newJobConfigInput' smart constructor.
data JobConfigInput = JobConfigInput'
  { bandMathConfig :: Prelude.Maybe BandMathConfigInput,
    -- | An object containing information about the job configuration for cloud
    -- masking.
    cloudMaskingConfig :: Prelude.Maybe CloudMaskingConfigInput,
    -- | An object containing information about the job configuration for cloud
    -- removal.
    cloudRemovalConfig :: Prelude.Maybe CloudRemovalConfigInput,
    -- | An object containing information about the job configuration for
    -- geomosaic.
    geoMosaicConfig :: Prelude.Maybe GeoMosaicConfigInput,
    -- | An object containing information about the job configuration for land
    -- cover segmentation.
    landCoverSegmentationConfig :: Prelude.Maybe LandCoverSegmentationConfigInput,
    -- | An object containing information about the job configuration for
    -- resampling.
    resamplingConfig :: Prelude.Maybe ResamplingConfigInput,
    stackConfig :: Prelude.Maybe StackConfigInput,
    -- | An object containing information about the job configuration for
    -- temporal statistics.
    temporalStatisticsConfig :: Prelude.Maybe TemporalStatisticsConfigInput,
    -- | An object containing information about the job configuration for zonal
    -- statistics.
    zonalStatisticsConfig :: Prelude.Maybe ZonalStatisticsConfigInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bandMathConfig', 'jobConfigInput_bandMathConfig' -
--
-- 'cloudMaskingConfig', 'jobConfigInput_cloudMaskingConfig' - An object containing information about the job configuration for cloud
-- masking.
--
-- 'cloudRemovalConfig', 'jobConfigInput_cloudRemovalConfig' - An object containing information about the job configuration for cloud
-- removal.
--
-- 'geoMosaicConfig', 'jobConfigInput_geoMosaicConfig' - An object containing information about the job configuration for
-- geomosaic.
--
-- 'landCoverSegmentationConfig', 'jobConfigInput_landCoverSegmentationConfig' - An object containing information about the job configuration for land
-- cover segmentation.
--
-- 'resamplingConfig', 'jobConfigInput_resamplingConfig' - An object containing information about the job configuration for
-- resampling.
--
-- 'stackConfig', 'jobConfigInput_stackConfig' -
--
-- 'temporalStatisticsConfig', 'jobConfigInput_temporalStatisticsConfig' - An object containing information about the job configuration for
-- temporal statistics.
--
-- 'zonalStatisticsConfig', 'jobConfigInput_zonalStatisticsConfig' - An object containing information about the job configuration for zonal
-- statistics.
newJobConfigInput ::
  JobConfigInput
newJobConfigInput =
  JobConfigInput'
    { bandMathConfig = Prelude.Nothing,
      cloudMaskingConfig = Prelude.Nothing,
      cloudRemovalConfig = Prelude.Nothing,
      geoMosaicConfig = Prelude.Nothing,
      landCoverSegmentationConfig = Prelude.Nothing,
      resamplingConfig = Prelude.Nothing,
      stackConfig = Prelude.Nothing,
      temporalStatisticsConfig = Prelude.Nothing,
      zonalStatisticsConfig = Prelude.Nothing
    }

jobConfigInput_bandMathConfig :: Lens.Lens' JobConfigInput (Prelude.Maybe BandMathConfigInput)
jobConfigInput_bandMathConfig = Lens.lens (\JobConfigInput' {bandMathConfig} -> bandMathConfig) (\s@JobConfigInput' {} a -> s {bandMathConfig = a} :: JobConfigInput)

-- | An object containing information about the job configuration for cloud
-- masking.
jobConfigInput_cloudMaskingConfig :: Lens.Lens' JobConfigInput (Prelude.Maybe CloudMaskingConfigInput)
jobConfigInput_cloudMaskingConfig = Lens.lens (\JobConfigInput' {cloudMaskingConfig} -> cloudMaskingConfig) (\s@JobConfigInput' {} a -> s {cloudMaskingConfig = a} :: JobConfigInput)

-- | An object containing information about the job configuration for cloud
-- removal.
jobConfigInput_cloudRemovalConfig :: Lens.Lens' JobConfigInput (Prelude.Maybe CloudRemovalConfigInput)
jobConfigInput_cloudRemovalConfig = Lens.lens (\JobConfigInput' {cloudRemovalConfig} -> cloudRemovalConfig) (\s@JobConfigInput' {} a -> s {cloudRemovalConfig = a} :: JobConfigInput)

-- | An object containing information about the job configuration for
-- geomosaic.
jobConfigInput_geoMosaicConfig :: Lens.Lens' JobConfigInput (Prelude.Maybe GeoMosaicConfigInput)
jobConfigInput_geoMosaicConfig = Lens.lens (\JobConfigInput' {geoMosaicConfig} -> geoMosaicConfig) (\s@JobConfigInput' {} a -> s {geoMosaicConfig = a} :: JobConfigInput)

-- | An object containing information about the job configuration for land
-- cover segmentation.
jobConfigInput_landCoverSegmentationConfig :: Lens.Lens' JobConfigInput (Prelude.Maybe LandCoverSegmentationConfigInput)
jobConfigInput_landCoverSegmentationConfig = Lens.lens (\JobConfigInput' {landCoverSegmentationConfig} -> landCoverSegmentationConfig) (\s@JobConfigInput' {} a -> s {landCoverSegmentationConfig = a} :: JobConfigInput)

-- | An object containing information about the job configuration for
-- resampling.
jobConfigInput_resamplingConfig :: Lens.Lens' JobConfigInput (Prelude.Maybe ResamplingConfigInput)
jobConfigInput_resamplingConfig = Lens.lens (\JobConfigInput' {resamplingConfig} -> resamplingConfig) (\s@JobConfigInput' {} a -> s {resamplingConfig = a} :: JobConfigInput)

jobConfigInput_stackConfig :: Lens.Lens' JobConfigInput (Prelude.Maybe StackConfigInput)
jobConfigInput_stackConfig = Lens.lens (\JobConfigInput' {stackConfig} -> stackConfig) (\s@JobConfigInput' {} a -> s {stackConfig = a} :: JobConfigInput)

-- | An object containing information about the job configuration for
-- temporal statistics.
jobConfigInput_temporalStatisticsConfig :: Lens.Lens' JobConfigInput (Prelude.Maybe TemporalStatisticsConfigInput)
jobConfigInput_temporalStatisticsConfig = Lens.lens (\JobConfigInput' {temporalStatisticsConfig} -> temporalStatisticsConfig) (\s@JobConfigInput' {} a -> s {temporalStatisticsConfig = a} :: JobConfigInput)

-- | An object containing information about the job configuration for zonal
-- statistics.
jobConfigInput_zonalStatisticsConfig :: Lens.Lens' JobConfigInput (Prelude.Maybe ZonalStatisticsConfigInput)
jobConfigInput_zonalStatisticsConfig = Lens.lens (\JobConfigInput' {zonalStatisticsConfig} -> zonalStatisticsConfig) (\s@JobConfigInput' {} a -> s {zonalStatisticsConfig = a} :: JobConfigInput)

instance Data.FromJSON JobConfigInput where
  parseJSON =
    Data.withObject
      "JobConfigInput"
      ( \x ->
          JobConfigInput'
            Prelude.<$> (x Data..:? "BandMathConfig")
            Prelude.<*> (x Data..:? "CloudMaskingConfig")
            Prelude.<*> (x Data..:? "CloudRemovalConfig")
            Prelude.<*> (x Data..:? "GeoMosaicConfig")
            Prelude.<*> (x Data..:? "LandCoverSegmentationConfig")
            Prelude.<*> (x Data..:? "ResamplingConfig")
            Prelude.<*> (x Data..:? "StackConfig")
            Prelude.<*> (x Data..:? "TemporalStatisticsConfig")
            Prelude.<*> (x Data..:? "ZonalStatisticsConfig")
      )

instance Prelude.Hashable JobConfigInput where
  hashWithSalt _salt JobConfigInput' {..} =
    _salt
      `Prelude.hashWithSalt` bandMathConfig
      `Prelude.hashWithSalt` cloudMaskingConfig
      `Prelude.hashWithSalt` cloudRemovalConfig
      `Prelude.hashWithSalt` geoMosaicConfig
      `Prelude.hashWithSalt` landCoverSegmentationConfig
      `Prelude.hashWithSalt` resamplingConfig
      `Prelude.hashWithSalt` stackConfig
      `Prelude.hashWithSalt` temporalStatisticsConfig
      `Prelude.hashWithSalt` zonalStatisticsConfig

instance Prelude.NFData JobConfigInput where
  rnf JobConfigInput' {..} =
    Prelude.rnf bandMathConfig
      `Prelude.seq` Prelude.rnf cloudMaskingConfig
      `Prelude.seq` Prelude.rnf cloudRemovalConfig
      `Prelude.seq` Prelude.rnf geoMosaicConfig
      `Prelude.seq` Prelude.rnf landCoverSegmentationConfig
      `Prelude.seq` Prelude.rnf resamplingConfig
      `Prelude.seq` Prelude.rnf stackConfig
      `Prelude.seq` Prelude.rnf temporalStatisticsConfig
      `Prelude.seq` Prelude.rnf zonalStatisticsConfig

instance Data.ToJSON JobConfigInput where
  toJSON JobConfigInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BandMathConfig" Data..=)
              Prelude.<$> bandMathConfig,
            ("CloudMaskingConfig" Data..=)
              Prelude.<$> cloudMaskingConfig,
            ("CloudRemovalConfig" Data..=)
              Prelude.<$> cloudRemovalConfig,
            ("GeoMosaicConfig" Data..=)
              Prelude.<$> geoMosaicConfig,
            ("LandCoverSegmentationConfig" Data..=)
              Prelude.<$> landCoverSegmentationConfig,
            ("ResamplingConfig" Data..=)
              Prelude.<$> resamplingConfig,
            ("StackConfig" Data..=) Prelude.<$> stackConfig,
            ("TemporalStatisticsConfig" Data..=)
              Prelude.<$> temporalStatisticsConfig,
            ("ZonalStatisticsConfig" Data..=)
              Prelude.<$> zonalStatisticsConfig
          ]
      )
