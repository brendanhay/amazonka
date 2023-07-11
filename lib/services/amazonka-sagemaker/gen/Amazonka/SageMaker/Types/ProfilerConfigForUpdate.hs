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
-- Module      : Amazonka.SageMaker.Types.ProfilerConfigForUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProfilerConfigForUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for updating the Amazon SageMaker Debugger
-- profile parameters, system and framework metrics configurations, and
-- storage paths.
--
-- /See:/ 'newProfilerConfigForUpdate' smart constructor.
data ProfilerConfigForUpdate = ProfilerConfigForUpdate'
  { -- | To turn off Amazon SageMaker Debugger monitoring and profiling while a
    -- training job is in progress, set to @True@.
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
-- Create a value of 'ProfilerConfigForUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disableProfiler', 'profilerConfigForUpdate_disableProfiler' - To turn off Amazon SageMaker Debugger monitoring and profiling while a
-- training job is in progress, set to @True@.
--
-- 'profilingIntervalInMilliseconds', 'profilerConfigForUpdate_profilingIntervalInMilliseconds' - A time interval for capturing system metrics in milliseconds. Available
-- values are 100, 200, 500, 1000 (1 second), 5000 (5 seconds), and 60000
-- (1 minute) milliseconds. The default value is 500 milliseconds.
--
-- 'profilingParameters', 'profilerConfigForUpdate_profilingParameters' - Configuration information for capturing framework metrics. Available key
-- strings for different profiling options are @DetailedProfilingConfig@,
-- @PythonProfilingConfig@, and @DataLoaderProfilingConfig@. The following
-- codes are configuration structures for the @ProfilingParameters@
-- parameter. To learn more about how to configure the
-- @ProfilingParameters@ parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
--
-- 's3OutputPath', 'profilerConfigForUpdate_s3OutputPath' - Path to Amazon S3 storage location for system and framework metrics.
newProfilerConfigForUpdate ::
  ProfilerConfigForUpdate
newProfilerConfigForUpdate =
  ProfilerConfigForUpdate'
    { disableProfiler =
        Prelude.Nothing,
      profilingIntervalInMilliseconds = Prelude.Nothing,
      profilingParameters = Prelude.Nothing,
      s3OutputPath = Prelude.Nothing
    }

-- | To turn off Amazon SageMaker Debugger monitoring and profiling while a
-- training job is in progress, set to @True@.
profilerConfigForUpdate_disableProfiler :: Lens.Lens' ProfilerConfigForUpdate (Prelude.Maybe Prelude.Bool)
profilerConfigForUpdate_disableProfiler = Lens.lens (\ProfilerConfigForUpdate' {disableProfiler} -> disableProfiler) (\s@ProfilerConfigForUpdate' {} a -> s {disableProfiler = a} :: ProfilerConfigForUpdate)

-- | A time interval for capturing system metrics in milliseconds. Available
-- values are 100, 200, 500, 1000 (1 second), 5000 (5 seconds), and 60000
-- (1 minute) milliseconds. The default value is 500 milliseconds.
profilerConfigForUpdate_profilingIntervalInMilliseconds :: Lens.Lens' ProfilerConfigForUpdate (Prelude.Maybe Prelude.Integer)
profilerConfigForUpdate_profilingIntervalInMilliseconds = Lens.lens (\ProfilerConfigForUpdate' {profilingIntervalInMilliseconds} -> profilingIntervalInMilliseconds) (\s@ProfilerConfigForUpdate' {} a -> s {profilingIntervalInMilliseconds = a} :: ProfilerConfigForUpdate)

-- | Configuration information for capturing framework metrics. Available key
-- strings for different profiling options are @DetailedProfilingConfig@,
-- @PythonProfilingConfig@, and @DataLoaderProfilingConfig@. The following
-- codes are configuration structures for the @ProfilingParameters@
-- parameter. To learn more about how to configure the
-- @ProfilingParameters@ parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
profilerConfigForUpdate_profilingParameters :: Lens.Lens' ProfilerConfigForUpdate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
profilerConfigForUpdate_profilingParameters = Lens.lens (\ProfilerConfigForUpdate' {profilingParameters} -> profilingParameters) (\s@ProfilerConfigForUpdate' {} a -> s {profilingParameters = a} :: ProfilerConfigForUpdate) Prelude.. Lens.mapping Lens.coerced

-- | Path to Amazon S3 storage location for system and framework metrics.
profilerConfigForUpdate_s3OutputPath :: Lens.Lens' ProfilerConfigForUpdate (Prelude.Maybe Prelude.Text)
profilerConfigForUpdate_s3OutputPath = Lens.lens (\ProfilerConfigForUpdate' {s3OutputPath} -> s3OutputPath) (\s@ProfilerConfigForUpdate' {} a -> s {s3OutputPath = a} :: ProfilerConfigForUpdate)

instance Prelude.Hashable ProfilerConfigForUpdate where
  hashWithSalt _salt ProfilerConfigForUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` disableProfiler
      `Prelude.hashWithSalt` profilingIntervalInMilliseconds
      `Prelude.hashWithSalt` profilingParameters
      `Prelude.hashWithSalt` s3OutputPath

instance Prelude.NFData ProfilerConfigForUpdate where
  rnf ProfilerConfigForUpdate' {..} =
    Prelude.rnf disableProfiler
      `Prelude.seq` Prelude.rnf profilingIntervalInMilliseconds
      `Prelude.seq` Prelude.rnf profilingParameters
      `Prelude.seq` Prelude.rnf s3OutputPath

instance Data.ToJSON ProfilerConfigForUpdate where
  toJSON ProfilerConfigForUpdate' {..} =
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
