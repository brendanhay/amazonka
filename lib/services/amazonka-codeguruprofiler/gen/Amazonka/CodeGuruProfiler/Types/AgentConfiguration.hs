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
-- Module      : Amazonka.CodeGuruProfiler.Types.AgentConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.AgentConfiguration where

import Amazonka.CodeGuruProfiler.Types.AgentParameterField
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The response of
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ConfigureAgent.html ConfigureAgent>
-- that specifies if an agent profiles or not and for how long to return
-- profiling data.
--
-- /See:/ 'newAgentConfiguration' smart constructor.
data AgentConfiguration = AgentConfiguration'
  { -- | Parameters used by the profiler. The valid parameters are:
    --
    -- -   @MaxStackDepth@ - The maximum depth of the stacks in the code that
    --     is represented in the profile. For example, if CodeGuru Profiler
    --     finds a method @A@, which calls method @B@, which calls method @C@,
    --     which calls method @D@, then the depth is 4. If the @maxDepth@ is
    --     set to 2, then the profiler evaluates @A@ and @B@.
    --
    -- -   @MemoryUsageLimitPercent@ - The percentage of memory that is used by
    --     the profiler.
    --
    -- -   @MinimumTimeForReportingInMilliseconds@ - The minimum time in
    --     milliseconds between sending reports.
    --
    -- -   @ReportingIntervalInMilliseconds@ - The reporting interval in
    --     milliseconds used to report profiles.
    --
    -- -   @SamplingIntervalInMilliseconds@ - The sampling interval in
    --     milliseconds that is used to profile samples.
    agentParameters :: Prelude.Maybe (Prelude.HashMap AgentParameterField Prelude.Text),
    -- | How long a profiling agent should send profiling data using
    -- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ConfigureAgent.html ConfigureAgent>
    -- . For example, if this is set to 300, the profiling agent calls
    -- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ConfigureAgent.html ConfigureAgent>
    -- every 5 minutes to submit the profiled data collected during that
    -- period.
    periodInSeconds :: Prelude.Int,
    -- | A @Boolean@ that specifies whether the profiling agent collects
    -- profiling data or not. Set to @true@ to enable profiling.
    shouldProfile :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AgentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentParameters', 'agentConfiguration_agentParameters' - Parameters used by the profiler. The valid parameters are:
--
-- -   @MaxStackDepth@ - The maximum depth of the stacks in the code that
--     is represented in the profile. For example, if CodeGuru Profiler
--     finds a method @A@, which calls method @B@, which calls method @C@,
--     which calls method @D@, then the depth is 4. If the @maxDepth@ is
--     set to 2, then the profiler evaluates @A@ and @B@.
--
-- -   @MemoryUsageLimitPercent@ - The percentage of memory that is used by
--     the profiler.
--
-- -   @MinimumTimeForReportingInMilliseconds@ - The minimum time in
--     milliseconds between sending reports.
--
-- -   @ReportingIntervalInMilliseconds@ - The reporting interval in
--     milliseconds used to report profiles.
--
-- -   @SamplingIntervalInMilliseconds@ - The sampling interval in
--     milliseconds that is used to profile samples.
--
-- 'periodInSeconds', 'agentConfiguration_periodInSeconds' - How long a profiling agent should send profiling data using
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ConfigureAgent.html ConfigureAgent>
-- . For example, if this is set to 300, the profiling agent calls
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ConfigureAgent.html ConfigureAgent>
-- every 5 minutes to submit the profiled data collected during that
-- period.
--
-- 'shouldProfile', 'agentConfiguration_shouldProfile' - A @Boolean@ that specifies whether the profiling agent collects
-- profiling data or not. Set to @true@ to enable profiling.
newAgentConfiguration ::
  -- | 'periodInSeconds'
  Prelude.Int ->
  -- | 'shouldProfile'
  Prelude.Bool ->
  AgentConfiguration
newAgentConfiguration
  pPeriodInSeconds_
  pShouldProfile_ =
    AgentConfiguration'
      { agentParameters =
          Prelude.Nothing,
        periodInSeconds = pPeriodInSeconds_,
        shouldProfile = pShouldProfile_
      }

-- | Parameters used by the profiler. The valid parameters are:
--
-- -   @MaxStackDepth@ - The maximum depth of the stacks in the code that
--     is represented in the profile. For example, if CodeGuru Profiler
--     finds a method @A@, which calls method @B@, which calls method @C@,
--     which calls method @D@, then the depth is 4. If the @maxDepth@ is
--     set to 2, then the profiler evaluates @A@ and @B@.
--
-- -   @MemoryUsageLimitPercent@ - The percentage of memory that is used by
--     the profiler.
--
-- -   @MinimumTimeForReportingInMilliseconds@ - The minimum time in
--     milliseconds between sending reports.
--
-- -   @ReportingIntervalInMilliseconds@ - The reporting interval in
--     milliseconds used to report profiles.
--
-- -   @SamplingIntervalInMilliseconds@ - The sampling interval in
--     milliseconds that is used to profile samples.
agentConfiguration_agentParameters :: Lens.Lens' AgentConfiguration (Prelude.Maybe (Prelude.HashMap AgentParameterField Prelude.Text))
agentConfiguration_agentParameters = Lens.lens (\AgentConfiguration' {agentParameters} -> agentParameters) (\s@AgentConfiguration' {} a -> s {agentParameters = a} :: AgentConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | How long a profiling agent should send profiling data using
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ConfigureAgent.html ConfigureAgent>
-- . For example, if this is set to 300, the profiling agent calls
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ConfigureAgent.html ConfigureAgent>
-- every 5 minutes to submit the profiled data collected during that
-- period.
agentConfiguration_periodInSeconds :: Lens.Lens' AgentConfiguration Prelude.Int
agentConfiguration_periodInSeconds = Lens.lens (\AgentConfiguration' {periodInSeconds} -> periodInSeconds) (\s@AgentConfiguration' {} a -> s {periodInSeconds = a} :: AgentConfiguration)

-- | A @Boolean@ that specifies whether the profiling agent collects
-- profiling data or not. Set to @true@ to enable profiling.
agentConfiguration_shouldProfile :: Lens.Lens' AgentConfiguration Prelude.Bool
agentConfiguration_shouldProfile = Lens.lens (\AgentConfiguration' {shouldProfile} -> shouldProfile) (\s@AgentConfiguration' {} a -> s {shouldProfile = a} :: AgentConfiguration)

instance Data.FromJSON AgentConfiguration where
  parseJSON =
    Data.withObject
      "AgentConfiguration"
      ( \x ->
          AgentConfiguration'
            Prelude.<$> ( x
                            Data..:? "agentParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "periodInSeconds")
            Prelude.<*> (x Data..: "shouldProfile")
      )

instance Prelude.Hashable AgentConfiguration where
  hashWithSalt _salt AgentConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` agentParameters
      `Prelude.hashWithSalt` periodInSeconds
      `Prelude.hashWithSalt` shouldProfile

instance Prelude.NFData AgentConfiguration where
  rnf AgentConfiguration' {..} =
    Prelude.rnf agentParameters `Prelude.seq`
      Prelude.rnf periodInSeconds `Prelude.seq`
        Prelude.rnf shouldProfile
