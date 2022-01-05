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
-- Module      : Amazonka.SageMaker.Types.ProfilerConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProfilerConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for Debugger system monitoring, framework
-- profiling, and storage paths.
--
-- /See:/ 'newProfilerConfig' smart constructor.
data ProfilerConfig = ProfilerConfig'
  { -- | Configuration information for capturing framework metrics. Available key
    -- strings for different profiling options are @DetailedProfilingConfig@,
    -- @PythonProfilingConfig@, and @DataLoaderProfilingConfig@. The following
    -- codes are configuration structures for the @ProfilingParameters@
    -- parameter. To learn more about how to configure the
    -- @ProfilingParameters@ parameter, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
    profilingParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A time interval for capturing system metrics in milliseconds. Available
    -- values are 100, 200, 500, 1000 (1 second), 5000 (5 seconds), and 60000
    -- (1 minute) milliseconds. The default value is 500 milliseconds.
    profilingIntervalInMilliseconds :: Prelude.Maybe Prelude.Integer,
    -- | Path to Amazon S3 storage location for system and framework metrics.
    s3OutputPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfilerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profilingParameters', 'profilerConfig_profilingParameters' - Configuration information for capturing framework metrics. Available key
-- strings for different profiling options are @DetailedProfilingConfig@,
-- @PythonProfilingConfig@, and @DataLoaderProfilingConfig@. The following
-- codes are configuration structures for the @ProfilingParameters@
-- parameter. To learn more about how to configure the
-- @ProfilingParameters@ parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
--
-- 'profilingIntervalInMilliseconds', 'profilerConfig_profilingIntervalInMilliseconds' - A time interval for capturing system metrics in milliseconds. Available
-- values are 100, 200, 500, 1000 (1 second), 5000 (5 seconds), and 60000
-- (1 minute) milliseconds. The default value is 500 milliseconds.
--
-- 's3OutputPath', 'profilerConfig_s3OutputPath' - Path to Amazon S3 storage location for system and framework metrics.
newProfilerConfig ::
  -- | 's3OutputPath'
  Prelude.Text ->
  ProfilerConfig
newProfilerConfig pS3OutputPath_ =
  ProfilerConfig'
    { profilingParameters =
        Prelude.Nothing,
      profilingIntervalInMilliseconds = Prelude.Nothing,
      s3OutputPath = pS3OutputPath_
    }

-- | Configuration information for capturing framework metrics. Available key
-- strings for different profiling options are @DetailedProfilingConfig@,
-- @PythonProfilingConfig@, and @DataLoaderProfilingConfig@. The following
-- codes are configuration structures for the @ProfilingParameters@
-- parameter. To learn more about how to configure the
-- @ProfilingParameters@ parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
profilerConfig_profilingParameters :: Lens.Lens' ProfilerConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
profilerConfig_profilingParameters = Lens.lens (\ProfilerConfig' {profilingParameters} -> profilingParameters) (\s@ProfilerConfig' {} a -> s {profilingParameters = a} :: ProfilerConfig) Prelude.. Lens.mapping Lens.coerced

-- | A time interval for capturing system metrics in milliseconds. Available
-- values are 100, 200, 500, 1000 (1 second), 5000 (5 seconds), and 60000
-- (1 minute) milliseconds. The default value is 500 milliseconds.
profilerConfig_profilingIntervalInMilliseconds :: Lens.Lens' ProfilerConfig (Prelude.Maybe Prelude.Integer)
profilerConfig_profilingIntervalInMilliseconds = Lens.lens (\ProfilerConfig' {profilingIntervalInMilliseconds} -> profilingIntervalInMilliseconds) (\s@ProfilerConfig' {} a -> s {profilingIntervalInMilliseconds = a} :: ProfilerConfig)

-- | Path to Amazon S3 storage location for system and framework metrics.
profilerConfig_s3OutputPath :: Lens.Lens' ProfilerConfig Prelude.Text
profilerConfig_s3OutputPath = Lens.lens (\ProfilerConfig' {s3OutputPath} -> s3OutputPath) (\s@ProfilerConfig' {} a -> s {s3OutputPath = a} :: ProfilerConfig)

instance Core.FromJSON ProfilerConfig where
  parseJSON =
    Core.withObject
      "ProfilerConfig"
      ( \x ->
          ProfilerConfig'
            Prelude.<$> ( x Core..:? "ProfilingParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ProfilingIntervalInMilliseconds")
            Prelude.<*> (x Core..: "S3OutputPath")
      )

instance Prelude.Hashable ProfilerConfig where
  hashWithSalt _salt ProfilerConfig' {..} =
    _salt `Prelude.hashWithSalt` profilingParameters
      `Prelude.hashWithSalt` profilingIntervalInMilliseconds
      `Prelude.hashWithSalt` s3OutputPath

instance Prelude.NFData ProfilerConfig where
  rnf ProfilerConfig' {..} =
    Prelude.rnf profilingParameters
      `Prelude.seq` Prelude.rnf profilingIntervalInMilliseconds
      `Prelude.seq` Prelude.rnf s3OutputPath

instance Core.ToJSON ProfilerConfig where
  toJSON ProfilerConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProfilingParameters" Core..=)
              Prelude.<$> profilingParameters,
            ("ProfilingIntervalInMilliseconds" Core..=)
              Prelude.<$> profilingIntervalInMilliseconds,
            Prelude.Just ("S3OutputPath" Core..= s3OutputPath)
          ]
      )
