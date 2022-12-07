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
-- Module      : Amazonka.CodeGuruProfiler.Types.AgentOrchestrationConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.AgentOrchestrationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies whether profiling is enabled or disabled for a profiling
-- group. It is used by
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ConfigureAgent.html ConfigureAgent>
-- to enable or disable profiling for a profiling group.
--
-- /See:/ 'newAgentOrchestrationConfig' smart constructor.
data AgentOrchestrationConfig = AgentOrchestrationConfig'
  { -- | A @Boolean@ that specifies whether the profiling agent collects
    -- profiling data or not. Set to @true@ to enable profiling.
    profilingEnabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AgentOrchestrationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profilingEnabled', 'agentOrchestrationConfig_profilingEnabled' - A @Boolean@ that specifies whether the profiling agent collects
-- profiling data or not. Set to @true@ to enable profiling.
newAgentOrchestrationConfig ::
  -- | 'profilingEnabled'
  Prelude.Bool ->
  AgentOrchestrationConfig
newAgentOrchestrationConfig pProfilingEnabled_ =
  AgentOrchestrationConfig'
    { profilingEnabled =
        pProfilingEnabled_
    }

-- | A @Boolean@ that specifies whether the profiling agent collects
-- profiling data or not. Set to @true@ to enable profiling.
agentOrchestrationConfig_profilingEnabled :: Lens.Lens' AgentOrchestrationConfig Prelude.Bool
agentOrchestrationConfig_profilingEnabled = Lens.lens (\AgentOrchestrationConfig' {profilingEnabled} -> profilingEnabled) (\s@AgentOrchestrationConfig' {} a -> s {profilingEnabled = a} :: AgentOrchestrationConfig)

instance Data.FromJSON AgentOrchestrationConfig where
  parseJSON =
    Data.withObject
      "AgentOrchestrationConfig"
      ( \x ->
          AgentOrchestrationConfig'
            Prelude.<$> (x Data..: "profilingEnabled")
      )

instance Prelude.Hashable AgentOrchestrationConfig where
  hashWithSalt _salt AgentOrchestrationConfig' {..} =
    _salt `Prelude.hashWithSalt` profilingEnabled

instance Prelude.NFData AgentOrchestrationConfig where
  rnf AgentOrchestrationConfig' {..} =
    Prelude.rnf profilingEnabled

instance Data.ToJSON AgentOrchestrationConfig where
  toJSON AgentOrchestrationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("profilingEnabled" Data..= profilingEnabled)
          ]
      )
