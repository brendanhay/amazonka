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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProfilerConfigForUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for updating the Debugger profile parameters,
-- system and framework metrics configurations, and storage paths.
--
-- /See:/ 'newProfilerConfigForUpdate' smart constructor.
data ProfilerConfigForUpdate = ProfilerConfigForUpdate'
  { -- | A time interval for capturing system metrics in milliseconds. Available
    -- values are 100, 200, 500, 1000 (1 second), 5000 (5 seconds), and 60000
    -- (1 minute) milliseconds. The default value is 500 milliseconds.
    profilingIntervalInMilliseconds :: Prelude.Maybe Prelude.Integer,
    -- | Path to Amazon S3 storage location for system and framework metrics.
    s3OutputPath :: Prelude.Maybe Prelude.Text,
    -- | Configuration information for capturing framework metrics. Available key
    -- strings for different profiling options are @DetailedProfilingConfig@,
    -- @PythonProfilingConfig@, and @DataLoaderProfilingConfig@. The following
    -- codes are configuration structures for the @ProfilingParameters@
    -- parameter. To learn more about how to configure the
    -- @ProfilingParameters@ parameter, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
    profilingParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | To disable Debugger monitoring and profiling, set to @True@.
    disableProfiler :: Prelude.Maybe Prelude.Bool
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
-- 'profilingIntervalInMilliseconds', 'profilerConfigForUpdate_profilingIntervalInMilliseconds' - A time interval for capturing system metrics in milliseconds. Available
-- values are 100, 200, 500, 1000 (1 second), 5000 (5 seconds), and 60000
-- (1 minute) milliseconds. The default value is 500 milliseconds.
--
-- 's3OutputPath', 'profilerConfigForUpdate_s3OutputPath' - Path to Amazon S3 storage location for system and framework metrics.
--
-- 'profilingParameters', 'profilerConfigForUpdate_profilingParameters' - Configuration information for capturing framework metrics. Available key
-- strings for different profiling options are @DetailedProfilingConfig@,
-- @PythonProfilingConfig@, and @DataLoaderProfilingConfig@. The following
-- codes are configuration structures for the @ProfilingParameters@
-- parameter. To learn more about how to configure the
-- @ProfilingParameters@ parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
--
-- 'disableProfiler', 'profilerConfigForUpdate_disableProfiler' - To disable Debugger monitoring and profiling, set to @True@.
newProfilerConfigForUpdate ::
  ProfilerConfigForUpdate
newProfilerConfigForUpdate =
  ProfilerConfigForUpdate'
    { profilingIntervalInMilliseconds =
        Prelude.Nothing,
      s3OutputPath = Prelude.Nothing,
      profilingParameters = Prelude.Nothing,
      disableProfiler = Prelude.Nothing
    }

-- | A time interval for capturing system metrics in milliseconds. Available
-- values are 100, 200, 500, 1000 (1 second), 5000 (5 seconds), and 60000
-- (1 minute) milliseconds. The default value is 500 milliseconds.
profilerConfigForUpdate_profilingIntervalInMilliseconds :: Lens.Lens' ProfilerConfigForUpdate (Prelude.Maybe Prelude.Integer)
profilerConfigForUpdate_profilingIntervalInMilliseconds = Lens.lens (\ProfilerConfigForUpdate' {profilingIntervalInMilliseconds} -> profilingIntervalInMilliseconds) (\s@ProfilerConfigForUpdate' {} a -> s {profilingIntervalInMilliseconds = a} :: ProfilerConfigForUpdate)

-- | Path to Amazon S3 storage location for system and framework metrics.
profilerConfigForUpdate_s3OutputPath :: Lens.Lens' ProfilerConfigForUpdate (Prelude.Maybe Prelude.Text)
profilerConfigForUpdate_s3OutputPath = Lens.lens (\ProfilerConfigForUpdate' {s3OutputPath} -> s3OutputPath) (\s@ProfilerConfigForUpdate' {} a -> s {s3OutputPath = a} :: ProfilerConfigForUpdate)

-- | Configuration information for capturing framework metrics. Available key
-- strings for different profiling options are @DetailedProfilingConfig@,
-- @PythonProfilingConfig@, and @DataLoaderProfilingConfig@. The following
-- codes are configuration structures for the @ProfilingParameters@
-- parameter. To learn more about how to configure the
-- @ProfilingParameters@ parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
profilerConfigForUpdate_profilingParameters :: Lens.Lens' ProfilerConfigForUpdate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
profilerConfigForUpdate_profilingParameters = Lens.lens (\ProfilerConfigForUpdate' {profilingParameters} -> profilingParameters) (\s@ProfilerConfigForUpdate' {} a -> s {profilingParameters = a} :: ProfilerConfigForUpdate) Prelude.. Lens.mapping Lens.coerced

-- | To disable Debugger monitoring and profiling, set to @True@.
profilerConfigForUpdate_disableProfiler :: Lens.Lens' ProfilerConfigForUpdate (Prelude.Maybe Prelude.Bool)
profilerConfigForUpdate_disableProfiler = Lens.lens (\ProfilerConfigForUpdate' {disableProfiler} -> disableProfiler) (\s@ProfilerConfigForUpdate' {} a -> s {disableProfiler = a} :: ProfilerConfigForUpdate)

instance Prelude.Hashable ProfilerConfigForUpdate where
  hashWithSalt _salt ProfilerConfigForUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` profilingIntervalInMilliseconds
      `Prelude.hashWithSalt` s3OutputPath
      `Prelude.hashWithSalt` profilingParameters
      `Prelude.hashWithSalt` disableProfiler

instance Prelude.NFData ProfilerConfigForUpdate where
  rnf ProfilerConfigForUpdate' {..} =
    Prelude.rnf profilingIntervalInMilliseconds
      `Prelude.seq` Prelude.rnf s3OutputPath
      `Prelude.seq` Prelude.rnf profilingParameters
      `Prelude.seq` Prelude.rnf disableProfiler

instance Data.ToJSON ProfilerConfigForUpdate where
  toJSON ProfilerConfigForUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ProfilingIntervalInMilliseconds" Data..=)
              Prelude.<$> profilingIntervalInMilliseconds,
            ("S3OutputPath" Data..=) Prelude.<$> s3OutputPath,
            ("ProfilingParameters" Data..=)
              Prelude.<$> profilingParameters,
            ("DisableProfiler" Data..=)
              Prelude.<$> disableProfiler
          ]
      )
