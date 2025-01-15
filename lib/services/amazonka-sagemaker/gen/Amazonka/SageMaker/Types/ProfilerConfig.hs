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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProfilerConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for Amazon SageMaker Debugger system
-- monitoring, framework profiling, and storage paths.
--
-- /See:/ 'newProfilerConfig' smart constructor.
data ProfilerConfig = ProfilerConfig'
  { -- | Configuration to turn off Amazon SageMaker Debugger\'s system monitoring
    -- and profiling functionality. To turn it off, set to @True@.
    disableProfiler :: Prelude.Maybe Prelude.Bool,
    -- | A time interval for capturing system metrics in milliseconds. Available
    -- values are 100, 200, 500, 1000 (1 second), 5000 (5 seconds), and 60000
    -- (1 minute) milliseconds. The default value is 500 milliseconds.
    profilingIntervalInMilliseconds :: Prelude.Maybe Prelude.Integer,
    -- | Configuration information for capturing framework metrics. Available key
    -- strings for different profiling options are @DetailedProfilingConfig@,
    -- @PythonProfilingConfig@, and @DataLoaderProfilingConfig@. The following
    -- codes are configuration structures for the @ProfilingParameters@
    -- parameter. To learn more about how to configure the
    -- @ProfilingParameters@ parameter, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
    profilingParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Path to Amazon S3 storage location for system and framework metrics.
    s3OutputPath :: Prelude.Maybe Prelude.Text
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
-- 'disableProfiler', 'profilerConfig_disableProfiler' - Configuration to turn off Amazon SageMaker Debugger\'s system monitoring
-- and profiling functionality. To turn it off, set to @True@.
--
-- 'profilingIntervalInMilliseconds', 'profilerConfig_profilingIntervalInMilliseconds' - A time interval for capturing system metrics in milliseconds. Available
-- values are 100, 200, 500, 1000 (1 second), 5000 (5 seconds), and 60000
-- (1 minute) milliseconds. The default value is 500 milliseconds.
--
-- 'profilingParameters', 'profilerConfig_profilingParameters' - Configuration information for capturing framework metrics. Available key
-- strings for different profiling options are @DetailedProfilingConfig@,
-- @PythonProfilingConfig@, and @DataLoaderProfilingConfig@. The following
-- codes are configuration structures for the @ProfilingParameters@
-- parameter. To learn more about how to configure the
-- @ProfilingParameters@ parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
--
-- 's3OutputPath', 'profilerConfig_s3OutputPath' - Path to Amazon S3 storage location for system and framework metrics.
newProfilerConfig ::
  ProfilerConfig
newProfilerConfig =
  ProfilerConfig'
    { disableProfiler = Prelude.Nothing,
      profilingIntervalInMilliseconds = Prelude.Nothing,
      profilingParameters = Prelude.Nothing,
      s3OutputPath = Prelude.Nothing
    }

-- | Configuration to turn off Amazon SageMaker Debugger\'s system monitoring
-- and profiling functionality. To turn it off, set to @True@.
profilerConfig_disableProfiler :: Lens.Lens' ProfilerConfig (Prelude.Maybe Prelude.Bool)
profilerConfig_disableProfiler = Lens.lens (\ProfilerConfig' {disableProfiler} -> disableProfiler) (\s@ProfilerConfig' {} a -> s {disableProfiler = a} :: ProfilerConfig)

-- | A time interval for capturing system metrics in milliseconds. Available
-- values are 100, 200, 500, 1000 (1 second), 5000 (5 seconds), and 60000
-- (1 minute) milliseconds. The default value is 500 milliseconds.
profilerConfig_profilingIntervalInMilliseconds :: Lens.Lens' ProfilerConfig (Prelude.Maybe Prelude.Integer)
profilerConfig_profilingIntervalInMilliseconds = Lens.lens (\ProfilerConfig' {profilingIntervalInMilliseconds} -> profilingIntervalInMilliseconds) (\s@ProfilerConfig' {} a -> s {profilingIntervalInMilliseconds = a} :: ProfilerConfig)

-- | Configuration information for capturing framework metrics. Available key
-- strings for different profiling options are @DetailedProfilingConfig@,
-- @PythonProfilingConfig@, and @DataLoaderProfilingConfig@. The following
-- codes are configuration structures for the @ProfilingParameters@
-- parameter. To learn more about how to configure the
-- @ProfilingParameters@ parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
profilerConfig_profilingParameters :: Lens.Lens' ProfilerConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
profilerConfig_profilingParameters = Lens.lens (\ProfilerConfig' {profilingParameters} -> profilingParameters) (\s@ProfilerConfig' {} a -> s {profilingParameters = a} :: ProfilerConfig) Prelude.. Lens.mapping Lens.coerced

-- | Path to Amazon S3 storage location for system and framework metrics.
profilerConfig_s3OutputPath :: Lens.Lens' ProfilerConfig (Prelude.Maybe Prelude.Text)
profilerConfig_s3OutputPath = Lens.lens (\ProfilerConfig' {s3OutputPath} -> s3OutputPath) (\s@ProfilerConfig' {} a -> s {s3OutputPath = a} :: ProfilerConfig)

instance Data.FromJSON ProfilerConfig where
  parseJSON =
    Data.withObject
      "ProfilerConfig"
      ( \x ->
          ProfilerConfig'
            Prelude.<$> (x Data..:? "DisableProfiler")
            Prelude.<*> (x Data..:? "ProfilingIntervalInMilliseconds")
            Prelude.<*> ( x
                            Data..:? "ProfilingParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "S3OutputPath")
      )

instance Prelude.Hashable ProfilerConfig where
  hashWithSalt _salt ProfilerConfig' {..} =
    _salt
      `Prelude.hashWithSalt` disableProfiler
      `Prelude.hashWithSalt` profilingIntervalInMilliseconds
      `Prelude.hashWithSalt` profilingParameters
      `Prelude.hashWithSalt` s3OutputPath

instance Prelude.NFData ProfilerConfig where
  rnf ProfilerConfig' {..} =
    Prelude.rnf disableProfiler `Prelude.seq`
      Prelude.rnf profilingIntervalInMilliseconds `Prelude.seq`
        Prelude.rnf profilingParameters `Prelude.seq`
          Prelude.rnf s3OutputPath

instance Data.ToJSON ProfilerConfig where
  toJSON ProfilerConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisableProfiler" Data..=)
              Prelude.<$> disableProfiler,
            ("ProfilingIntervalInMilliseconds" Data..=)
              Prelude.<$> profilingIntervalInMilliseconds,
            ("ProfilingParameters" Data..=)
              Prelude.<$> profilingParameters,
            ("S3OutputPath" Data..=) Prelude.<$> s3OutputPath
          ]
      )
